package grgzmy.appdirect.models

case class Subscription(accountId: Int,
                        marketPlace: Option[String],
                        creator: Option[String],
                        edition: Option[EditionCode.Value],
                        company: Option[String],
                        status: Option[AccountStatus.Value],
                        users: Option[Int],
                        megabytes: Option[Int])