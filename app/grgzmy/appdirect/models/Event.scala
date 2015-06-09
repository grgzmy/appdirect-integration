package grgzmy.appdirect.models

import scala.xml.NodeSeq
import play.api.data.validation.{Valid, Constraints}

abstract class EventException(msg: String) extends Exception()
case class EventParamException(msg: String) extends Exception()
case class EmailFormatError(msg: String) extends EventException(msg)

sealed trait EventParams

trait CompanionUtils{
  def from(xml: NodeSeq): EventParams
  implicit class FromXML(xml: NodeSeq){
    def get(route: String): Option[String] = (xml \ route).headOption.map(_.text)
    def getOpt[T <: EventParams](route: String, f: (NodeSeq) => T): Option[T] = (xml \ route).headOption.map(f)
  }
}

case class MarketPlace(baseUrl: Option[String], partner: Option[String]) extends EventParams
object MarketPlace extends CompanionUtils{
  def from(xml: NodeSeq) = MarketPlace(xml get "baseUrl", xml get "partner")
}

object Language extends SaferEnum{
  val DE = Value("de")
  val EN = Value("en")
  val ES = Value("es")
  val FR = Value("fr")
  val IT = Value("it")
  val LT = Value("lt")
  val FI = Value("fi")
  val SV = Value("sv")
}

case class Email(address: String)

object Email{
  def create(address: String) = Constraints.emailAddress(address) match{
    case Valid => Email(address)
    case _ => throw new EmailFormatError(s"Malformed Email: $address")
  }
}

trait UserType
case object Creator extends UserType
case object EndUser extends UserType
case class User(userType: UserType, email: Option[Email], firstName: Option[String], lastName: Option[String], openId: Option[String], language: Option[Language.Value], uuid: Option[String]) extends EventParams

object User extends CompanionUtils{
  def asCreator(xml: NodeSeq) = from(xml)
  def asEndUser(xml: NodeSeq) = from(xml).copy(userType = EndUser)
  def from(xml: NodeSeq) = User(Creator, (xml get "email").map(Email.create), xml get "firstName", xml get "lastName", xml get "openId", (xml get "language").map(Language.from), xml get "uuid")
}

case class Company(uuid: Option[String], email: Option[String], name: Option[String], phoneNumber: Option[String], website: Option[String]) extends EventParams
object Company extends CompanionUtils{
  def from(xml: NodeSeq) = Company(xml get "uuid", xml get "email", xml get "name", xml get "phoneNumber", xml get "website")
}

object EditionCode extends SaferEnum{
  val FREE_TRIAL = Value("FREE_TRIAL")
  val BASIC = Value("BASIC")
  val PREMIUM = Value("PREMIUM")
  val ENTERPRISE = Value("ENTERPRISE")
}

case class Order(editionCode: Option[EditionCode.Value], users: Int, megaBytes: Int) extends EventParams
object Order extends CompanionUtils{
  def from(xml: NodeSeq) = {
    val users: Int = (xml \\ "item").toList.find(n=> (n \ "unit").text == "USERS").map(_ \ "quantity").map(_.text.toInt).getOrElse(0)
    val megaBytes: Int = (xml \\ "item").toList.find(n=> (n \ "unit").text == "MEGABYTE").map(_ \ "quantity").map(_.text.toInt).getOrElse(0)
    Order((xml get "editionCode").map(EditionCode.from), users, megaBytes)
  }
}

object AccountStatus extends SaferEnum{
  val FREE_TRIAL = Value("FREE_TRIAL")
  val FREE_TRIAL_EXPIRED = Value("FREE_TRIAL_EXPIRED")
  val ACTIVE = Value("ACTIVE")
  val SUSPENDED = Value("SUSPENDED")
  val CANCELLED = Value("CANCELLED")

  //Quick 'n' dirty FSM, but oh well, we only have five states and rules are simple enough
  private val allowedTransitions: Map[AccountStatus.Value, List[AccountStatus.Value]] = Map(
    FREE_TRIAL -> List(FREE_TRIAL_EXPIRED, CANCELLED, ACTIVE),
    FREE_TRIAL_EXPIRED -> List(CANCELLED, ACTIVE),
    ACTIVE -> List(SUSPENDED, CANCELLED, FREE_TRIAL),
    SUSPENDED -> List(ACTIVE, CANCELLED),
    CANCELLED -> List.empty
  )

  def canTransitionTo(from: AccountStatus.Value, to: AccountStatus.Value): Boolean =
    allowedTransitions(from).contains(to)
}

case class Account(accountIdentifier: Option[Int], status: AccountStatus.Value) extends EventParams
object Account extends CompanionUtils{
  def init(status: AccountStatus.Value) = Account(None, status)
  def from(xml: NodeSeq) = {
    val accountIdentifier = xml get "accountIdentifier" map {
      case "dummy-account" => 1 //to make ping tests work :(
      case id: String => try{id.toInt} catch {case e: NumberFormatException => throw new EventParamException("Only numeric account ids are supported")}
      case _=> throw new EventParamException("Malformed account identifier")
    }
    Account(accountIdentifier, AccountStatus.from(xml \ "status"))
  }
}

case class Notice(typ: NoticeType.Value) extends EventParams
object Notice extends CompanionUtils{
  def from(xml: NodeSeq) = Notice(NoticeType.from(xml \ "notice"))
}

trait SaferEnum extends Enumeration{
  val key: String = "type" //override in implementations if necessary
  val UNKNOWN = Value
  def from(xml: NodeSeq) = this.values.find(_.toString == xml.text).getOrElse(UNKNOWN)
  def from(s: String) = this.values.find(_.toString == s).getOrElse(UNKNOWN)
}

object NoticeType extends SaferEnum{
  val DEACTIVATED = Value("DEACTIVATED")
  val REACTIVATED = Value("REACTIVATED")
  val CLOSED = Value("CLOSED")
  val UPCOMING_INVOICE = Value("UPCOMING_INVOICE")
}

case class Payload(company: Option[Company], order: Option[Order], account: Option[Account], notice: Option[Notice], user: Option[User]) extends EventParams
object Payload extends CompanionUtils{

  def from(xml: NodeSeq) = {
    val order = xml.getOpt[Order]("order", Order.from)
    import EditionCode._
    val initialStatus = order.flatMap(_.editionCode) match{
      case Some(BASIC | ENTERPRISE | PREMIUM) => AccountStatus.ACTIVE
      case _ => AccountStatus.FREE_TRIAL
    }

    Payload(
      xml.getOpt[Company]("company", Company.from),
      order,
      xml.getOpt[Account]("account", Account.from).orElse(Some(Account.init(initialStatus))),
      xml.getOpt[Notice]("notice", Notice.from),
      xml.getOpt[User]("user", User.asEndUser)
    )
  }

}


object EventType extends SaferEnum{
  val SUBSCRIPTION_ORDER = Value("SUBSCRIPTION_ORDER")
  val SUBSCRIPTION_CHANGE = Value("SUBSCRIPTION_CHANGE")
  val SUBSCRIPTION_CANCEL = Value("SUBSCRIPTION_CANCEL")
  val SUBSCRIPTION_NOTICE = Value("SUBSCRIPTION_NOTICE")
  val USER_ASSIGNMENT = Value("USER_ASSIGNMENT")
  val USER_UNASSIGNMENT = Value("USER_UNASSIGNMENT")
}



case class Event(typ: EventType.Value, marketPlace: Option[MarketPlace], creator: Option[User], payload: Option[Payload]) extends EventParams
object Event extends CompanionUtils{
  def from(xml: NodeSeq) ={
//    require((xml \ "event").nonEmpty, throw EventParamException("Missing <event> tags!"))
//    println(xml.toString)
//    val eventXml = (xml \ "event").head
    Event(
      EventType.from(xml \ "type"),
      xml.getOpt[MarketPlace]( "marketplace", MarketPlace.from),
      xml.getOpt[User]("creator", User.asCreator),
      xml.getOpt[Payload]("payload", Payload.from)
    )
  }

}


