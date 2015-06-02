package grgzmy.appdirect

import play.api.Play._
import play.api.libs.oauth.ConsumerKey

/**
 * Created by grg on 15-05-31.
 */
object Security {
  def $(s: String) = current.configuration.getString(s)
  val consumer: Option[ConsumerKey] =
    for{
      key <- $("security.oauth.key")
      secret <- $("security.oauth.secret")
    } yield ConsumerKey(key, secret)
  val AUTH_HEADER = "Authorization"




 }
