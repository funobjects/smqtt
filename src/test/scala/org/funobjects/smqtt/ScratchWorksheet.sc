import org.funobjects.smqtt._
import org.funobjects.smqtt.MqttCodec._
import org.funobjects.smqtt.SomethingProto._
import org.scalatest._
import org.scalatest.Matchers._
import scodec._
import scodec.bits._
import scodec.codecs._
import shapeless.HNil

val a = utf8.encode("hello")
val b1 = bin"11011"

val codec = (codecs.bits(4) flatPrepend { flags =>
  constant(bin"0000") ::
  conditional(flags(0), uint8) ::
  conditional(flags(1), uint8) ::
  conditional(flags(2), uint8) ::
  conditional(flags(3), uint8) }).dropUnits

codec.decode(bin"0010" ++ bin"0000" ++ hex"cc".bits).require
codec.encode(bin"0011" :: None :: None :: Some(0xcc) :: None :: HNil)

object BenchTimer {
  def time[A](iterations: Int)(f: => A): Double = {
    val start = System.currentTimeMillis()
    (1 to iterations) foreach { _ => f }
    val elapsed = System.currentTimeMillis() - start
    val per = elapsed/iterations.toDouble
    if (per < 0.1)
      println(s"Elapsed time $elapsed msec for $iterations iterations (${per*1000} nanosec per iteration)")
    else
      println(s"Elapsed time $elapsed msec for $iterations iterations ($per msec per iteration)")
    per
  }
}

BenchTimer.time(10000) { (1 to 300).map(Math.pow(_,3)) }
case class What(n1: Int, n2: Int, n3: Int, n4: Int, s1: String, s2: String, s3: String, s4: String)
def checkWhatCond(w: What): Int = {
  if (w.n1 == 1 && w.n2 == 2 && w.n3 == 3 && w.n4 == 4)
    3
  else if (w.n1 == 4 && w.n2 == 3 && w.n3 == 2 && w.n4 == 1)
    4
  else if (w.s1 == "1" && w.s2 == "2" && w.s3 == "3" && w.s4 == "4")
    5
  else if (w.s1 == "4" && w.s2 == "3" && w.s3 == "2" && w.s4 == "1")
    6
  else
    7
}

def checkWhatMatch(what: What): Int = what match {
  case What(n1, n2, n3, n4, _, _, _, _) if (n1 == 1 && n2 == 2 && n3 == 3 && n4 == 4) => 3
  case What(n1, n2, n3, n4, _, _, _, _) if (n1 == 4 && n2 == 3 && n3 == 2 && n4 == 1) => 4
  case What(_, _, _, _, s1, s2, s3, s4) if (s1 == "1" && s2 == "2" && s3 == "3" && s4 == "4") => 5
  case What(_, _, _, _, s1, s2, s3, s4) if (s1 == "4" && s2 == "3" && s3 == "2" && s4 == "1") => 6
  case _ => 7
}

def w1 = What(1, 2, 3, 4, "1", "2", "3", "4")
def w2 = What(4, 3, 2, 1, "4", "3", "2", "1")
BenchTimer.time(100000000) { checkWhatCond(w1); checkWhatCond(w2) }
BenchTimer.time(100000000) { checkWhatMatch(w1); checkWhatMatch(w2) }