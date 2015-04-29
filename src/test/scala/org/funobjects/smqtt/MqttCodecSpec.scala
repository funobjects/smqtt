package org.funobjects.smqtt

import org.funobjects.smqtt.SomethingProto._

import org.scalatest._
import scodec.Attempt.Successful

import shapeless._

import scodec._
import scodec.bits._
import scodec.codecs._
import scodec.codecs.implicits._

/**
 * Created by rgf on 4/12/15.
 */
class MqttCodecSpec extends WordSpec with Matchers {

  "Mqtt Codec" should {

    import MqttCodecs._
    import MqttFixtures._

    "encode and decode CONNECT packets" in {

      // note, when testing
      mqttCodec.encode(connectMessage).require shouldBe connectBits

      // when testing one of the discriminated codecs directly,
      // we drop the discriminator, since the aggregate (coproduct) decoder
      // normally takes it
      ConnectPacket.codec.encode(connectMessage).require shouldBe connectBits.drop(4)
    }
  }
}

object MqttFixtures {
  import MqttCodecs._
  /**
   *
   * Connect Packet w/
   *
   * fixed header (1, 0, nn)
   * -- start of variable header
   * protocol name w/ 16-bit size (MQTT)
   * protocol level (4)
   * connect flags (User=1,Pass=1,Retain=0,QoS=0,Will=1,Clean=1)
   * keep-alive (uint16)
   * -- (start of payload) -
   * client ID ("clientid")
   * will topic ("/topic")
   * will message (4 bytes, 0x11223344)
   * user name ("user")
   * password ("pass"
   *
   */
  val connectBits = hex"""
  10 2e
  00 04 4d 51 54 54
  04
  c6
  00 00
  00 08 63 6c 69 65 6e 74 69 64
  00 06 2f 74 6f 70 69 63
  00 04 11 22 33 44
  00 04 75 73 65 72
  00 04 70 61 73 73
  """.bits

  val connectMessage = ConnectPacket(
    protoName = "MQTT",
    protoLevel = 4,
    flags = ConnectPacket.ConnectFlags(true,true,false,0,true,true),
    keepAlive = 0,
    clientId = "clientid",
    willTopic = Some("/topic"),
    willMessage = Some(hex"11223344"),
    username = Some("user"),
    password = Some(hex"70617373"))
}
