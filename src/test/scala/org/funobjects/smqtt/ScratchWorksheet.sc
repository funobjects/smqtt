
import java.nio.charset.Charset

import org.scalatest._
import org.scalatest.Matchers._


import scala.collection.JavaConversions._

import scodec._
import scodec.bits._
import scodec.codecs._


val a = utf8.encode("hello")

val b1 = bin"11011"

val codec = uint8.xmap( (a: Int) => a.toString, (s: String) => Integer.parseInt(s))

codec.encode("3")


//
// 01



