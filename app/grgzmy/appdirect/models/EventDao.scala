package grgzmy.appdirect.models

import java.sql.{PreparedStatement, Connection, Statement}
import org.h2.jdbc.JdbcSQLException
import org.h2.tools.Server
import play.api.db._
import play.api.Play.current
import scala.xml.NodeSeq
import NoticeType._, AccountStatus._


class DbException(msg: String, val err: ErrorCode.Value = ErrorCode.UNKNOWN_ERROR) extends Exception(msg)

class EventDao {
  val server = Server.createTcpServer()

  val db = DB.getDataSource()

  val conn: Connection = db.getConnection

  val GENERATE_RETURNED_ID = true
  val DONT_GENERATE_RETURNED_ID = false

  private def auditEvent(xml: NodeSeq) = {
    val eventSql = "INSERT INTO EVENTS(PAYLOAD) VALUES(?)"
    insertAndRetrunId(eventSql, List(xml))
  }

  private def enrichPreparedStatements(ps: PreparedStatement, inputs: List[Any]): Unit =
    inputs.zipWithIndex.foreach{
      case(x, i) => x match{
        case data: Int => ps.setInt(i+1, data)
        case data: String =>
          ps.setString(i+1, data)
        case data: NodeSeq =>
          val clob = conn.createClob()
          clob.setString(1, data.toString())
          ps.setClob(i+1, clob)
        case null => ps.setNull(i+1, 0)
        case data => throw new DbException(s"unsopported data : $data")
      }
    }

  private def update(sql: String, inputs: List[Any]) = {
    val ps = conn.prepareStatement(sql)
    enrichPreparedStatements(ps, inputs)
    ps.executeUpdate()
  }

  private def insert(sql: String, inputs: List[Any], returnGeneratedID: Boolean): PreparedStatement = {
    val ps = if(returnGeneratedID) conn.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS) else conn.prepareStatement(sql)
    enrichPreparedStatements(ps, inputs)
    try{ps.executeUpdate()} catch{
      case e: JdbcSQLException if e.getMessage.contains("CONSTRAINT_INDEX") => ()
      case e: Exception =>
        throw new DbException("Cannot execute prepared statement")
    }
    ps
  }


  private def insertAndRetrunId(sql: String, inputs: List[Any]): Long = {
    val ps =  insert(sql, inputs, GENERATE_RETURNED_ID)
    Option(ps.getGeneratedKeys).map{
      case rs if rs.next =>
        rs.getLong(1)
      case _ => throw new DbException("ResultSet Empty")
    }.getOrElse{
      throw new DbException("No ResultSet Found")
    }
  }

  private def insertUnique(sql: String, inputs: List[Any]): Unit = {
    try{
      insert(sql, inputs, DONT_GENERATE_RETURNED_ID)
    }catch {
      case e: Exception =>
        e.printStackTrace()
        throw new DbException(s"Database Error saving company info", ErrorCode.UNKNOWN_ERROR)
    }
  }

  private def saveReleventInfo(event: Event, xml: NodeSeq): Unit = {
    //uuid unique, so save will fail if already exists
    val companySql = "INSERT INTO COMPANY VALUES (?,?,?,?,?)"
    val creatorSql = "INSERT INTO CREATOR VALUES (?,?,?,?,?,?,?)"
    val marketPlaceSql = "INSERT INTO MARKETPLACE VALUES(?,?)"
    for{
      marketPlace <- event.marketPlace
      marketPlaceParter <- marketPlace.partner
      marketPlaceUrl <- marketPlace.baseUrl
      paylaod <- event.payload
      company <- paylaod.company
      companyUuid <- company.uuid
      creator <- event.creator
      creatorUuid <- creator.uuid
      openId <- creator.openId
    } yield{
      insertUnique(companySql, List(companyUuid, company.email.orNull, company.phoneNumber.orNull, company.name.orNull, company.website.orNull))
      insertUnique(creatorSql, List(creatorUuid, openId, creator.firstName.orNull, creator.lastName.orNull, creator.email.map(_.address).orNull, companyUuid, marketPlaceParter))
      insertUnique(marketPlaceSql, List(marketPlaceParter, marketPlaceUrl))
    }
    ()
  }

  def createSubscription(event: Event, xml: NodeSeq): Option[Long] = {
    auditEvent(xml)
    saveReleventInfo(event, xml)

    val stmt = "INSERT INTO SUBSCRIPTIONS(MARKETPLACE, CREATOR, COMPANY, STATUS, NUM_USERS, NUM_MEGABYTES, EDITION_CODE) VALUES (?,?,?,?,?,?,?)"
    for{
      marketPlaceObj <- event.marketPlace
      marketPlace <- marketPlaceObj.partner
      creator <- event.creator
      creatorId <- creator.uuid
      payload <- event.payload
      account <- payload.account
      order <- payload.order
      editionCode <- order.editionCode
    }yield{
        try{
          insertAndRetrunId(stmt, List(marketPlace, creatorId, payload.company.flatMap(_.name).orNull, account.status.toString, order.users, order.megaBytes, editionCode.toString))
        } catch{
          case e: DbException => throw e
          case e: Exception =>
            throw new Exception(e)
        }
      }
  }

  def changeSubscription(event: Event, xml: NodeSeq): Boolean = {
    auditEvent(xml)
    val updateSql = "UPDATE SUBSCRIPTIONS SET EDITION_CODE=?, NUM_USERS=?, NUM_MEGABYTES=? WHERE ACCOUNT_IDENTIFIER=?"
    (for{
      payload <- event.payload
      account <- payload.account
      accountIdentifier <- account.accountIdentifier
      order <- payload.order
      editionCode <- order.editionCode
    }yield{
      try{
        update(updateSql, List(editionCode.toString, order.users, order.megaBytes, accountIdentifier)) == 1
      } catch {
        case e: Exception => throw new DbException(s"Failed to update account $accountIdentifier.", ErrorCode.UNKNOWN_ERROR)
      }
    }).getOrElse{
      throw new EventParamException("Missing Payload data")
    }
  }

  def cancelSubscription(event: Event, xml: NodeSeq): Boolean = {
    auditEvent(xml)
    setStatus(event, CANCELLED)
  }

  private def setStatus(event: Event, status: AccountStatus.Value): Boolean = {
    val sql = "UPDATE SUBSCRIPTIONS SET STATUS=? WHERE ACCOUNT_IDENTIFIER=?"
    (for{
      payload <- event.payload
      account <- payload.account
      accountIdentifier <- account.accountIdentifier
    }yield{
        try{
          update(sql, List(status.toString, accountIdentifier)) == 1
        }catch {
          case e: Exception => throw new DbException(s"Failed to set status of  account $accountIdentifier to ${status.toString}", ErrorCode.UNKNOWN_ERROR)
        }
      }).getOrElse{
      throw new EventParamException("Missing Payload data")
    }
  }

  def notice(event: Event, xml: NodeSeq): Boolean = {
    auditEvent(xml)
    (for{
      payload <- event.payload
      account<- payload.account
      notice <- payload.notice
    }yield{
      notice.typ match {
        case CLOSED =>  setStatus(event, CANCELLED)
        case DEACTIVATED if canTransitionTo(account.status, FREE_TRIAL_EXPIRED) => setStatus(event, FREE_TRIAL_EXPIRED)
        case DEACTIVATED if canTransitionTo(account.status, SUSPENDED) => setStatus(event, SUSPENDED)
        case REACTIVATED if canTransitionTo(account.status, ACTIVE) => setStatus(event, ACTIVE)
        case UPCOMING_INVOICE => true //dont care
        case _ => throw new EventParamException("Invalid notice")
      }
    }).getOrElse{
      throw new EventParamException("Missing Payload data")
    }
  }

  def assignUser(event: Event, xml: NodeSeq): Unit = {
    auditEvent(xml)
    val sql = "INSERT INTO USER VALUES(?,?,?,?,?,?)"
    (for{
      payload <-event.payload
      user <- payload.user
      email <- user.email
      first <- user.firstName
      last <- user.lastName
      openID <- user.openId
      language <- user.language
    }yield{
      try{
        require(!checkIfUserExists(user).contains(true), throw new DbException("User already exists", ErrorCode.USER_ALREADY_EXISTS))
        insertUnique(sql, List(openID, first, last, email.address, language.toString, user.uuid.orNull))
      }catch{
        case e: DbException => throw e
        case e: Exception => throw new DbException(s"Failed to add user", ErrorCode.UNKNOWN_ERROR)
      }
    }).orElse{
      println(s"event didnt have everything $event")
      Option(())
    }
  }

  private def checkIfUserExists(user: User): Option[Boolean] = {
    val sql =  "SELECT * FROM USER WHERE OPENID=? AND EMAIL=? AND FIRST_NAME=? AND LAST_NAME=?"
    for{
      email <- user.email
      first <- user.firstName
      last <- user.lastName
      openID <- user.openId
    }yield{
      val ps = conn.prepareStatement(sql)
      enrichPreparedStatements(ps, List(openID, email.address, first, last))
      val rsOpt = Option(ps.executeQuery())
      rsOpt.exists(_.next)
    }
  }

  def unassignUser(event: Event, xml: NodeSeq): Boolean = {
    val sql = "DELETE FROM USER WHERE OPENID=? AND EMAIL=? AND FIRST_NAME=? AND LAST_NAME=?"
    (for{
      payload <-event.payload
      user <- payload.user
      email <- user.email
      first <- user.firstName
      last <- user.lastName
      openID <- user.openId
    }yield{
        try{
          update(sql, List(openID, email.address, first, last)) == 1
        }catch {
          case e: Exception => throw new DbException(s"Failed to delete user", ErrorCode.UNKNOWN_ERROR)
        }
      }).getOrElse{
      throw new EventParamException("Missing Payload data")
    }
  }

  def getSubscriptions(): List[Subscription]


}
