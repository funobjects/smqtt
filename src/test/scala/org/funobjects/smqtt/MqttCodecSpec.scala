package org.funobjects.smqtt

import java.nio.charset.Charset

import org.funobjects.smqtt.SomethingProto._

import org.scalatest.{Matchers, FlatSpec}

import shapeless._

import scodec._
import scodec.bits._
import scodec.codecs._
import scodec.codecs.implicits._

/**
 * Created by rgf on 4/12/15.
 */
class MqttCodecSpec extends FlatSpec with Matchers {


  "A codec" should "do something" in {


    val cocodec = somethingCodec

    val gbv = cocodec.encode(GreenThing("light"))
    println(s"encoded $gbv" )

    val sbv = cocodec.encode(SoftThing(7))
    println(s"encoded $sbv" )

    val what = cocodec.decode(gbv.getOrElse(fail()))
    println(what)

    val what2 = cocodec.decode(sbv.getOrElse(fail()))
    println(what2)
  }
}
