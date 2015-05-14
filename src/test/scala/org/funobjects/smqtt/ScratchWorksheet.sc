import org.funobjects.smqtt._
import org.funobjects.smqtt.MqttCodec._
import org.funobjects.smqtt.SomethingProto._
import org.scalatest._
import org.scalatest.Matchers._
import scodec._
import scodec.bits._
import scodec.codecs._
import shapeless.HNil


val b = bin"01"


ByteVector(0).decodeUtf8
hex"00".decodeUtf8
hex"61 62 63 20 f0 82 82 ac".decodeUtf8
hex"61 62 63 c0 80".decodeUtf8
hex"61 62 63 20 e1 80 a9 20 f0 aa 9b 94".decodeUtf8

hex"61 62 63 20 f0 82 82 ac".decodeUtf8


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

//BenchTimer.time(100000) {
//  hex"61 62 63 20 e1 80 a9 20 f0 aa 9b 94".decodeUtf8
//}
//
//BenchTimer.time(100000) {
//  UnicodeUtil.bytesToCodePoints(hex"61 62 63 20 e1 80 a9 20 f0 aa 9b 94")
//}

val a = Array(0x61, 0x62, 0x63, 0x20, 0x1029, 0x20, 0x2A6D4 )
new String(a, 0, a.length)
