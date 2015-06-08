package grgzmy.appdirect.models


import scala.xml.NodeSeq


trait Response {
  val success: Boolean
  val message: String
  lazy val xml =
    <result>
      <success>{success}</success>
      <message>{message}</message>
    </result>
  def toXml: NodeSeq
}

case class SuccessResponse(message: String, accountIdentifier: Option[String]) extends Response{
  val success = true
  def toXml = accountIdentifier.map(id => xml.copy(child = xml.child ++ <accountIdentifier>{id}</accountIdentifier>)).getOrElse(xml)
}

case class FailureResponse(message: String, errorCode: ErrorCode.Value) extends Response{
  val success = false
  def toXml = xml.copy(child = xml.child ++ <errorCode>{errorCode.toString}</errorCode>)
}


