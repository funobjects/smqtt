import org.funobjects.smqtt._
import org.funobjects.smqtt.MqttCodec._
import org.funobjects.smqtt.SomethingProto._
import org.scalatest._
import org.scalatest.Matchers._
import scodec._
import scodec.bits._
import scodec.codecs._
import shapeless.HNil


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

val s = "$this/that"

