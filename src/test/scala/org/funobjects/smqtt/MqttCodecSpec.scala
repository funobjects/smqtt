package org.funobjects.smqtt

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scodec.Attempt.Successful
import scodec.Err.InsufficientBits

import shapeless._

import scodec._
import scodec.bits._
import scodec.codecs._
import scodec.codecs.implicits._

import scala.collection.mutable.ListBuffer

/**
 * Tests for MqttCodec
 *
 * @author Robert Fries
 */
class MqttCodecSpec extends WordSpec with Matchers with PropertyChecks {

  "Mqtt Codec" should {

    import MqttCodec._
    import MqttFixtures._

    /**
     * Checks a given msg and expected bit pattern, encoding both with the given
     * codec (which must be one of the coproducts of ControlPacket) and the aggregate codec.
     *
     * @param codec The codec for the given type
     * @param msg   The message to check
     * @param bits  The expected bit pattern
     * @tparam A    The type of the codec and message
     */
    //
    //
    def checkEncodeDecode[A <: ControlPacket](codec: Codec[A], msg: A, bits: BitVector): Unit = {
      // when using the codec directly, the discriminator must be stripped off
      codec.encode(msg).require shouldBe bits.drop(4)
      codec.decode(bits.drop(4)).require.value shouldBe msg

      // make sure the codec works through the coproduct (descriminator) codec
      mqttCodec.encode(msg).require shouldBe bits
      mqttCodec.decode(bits).require.value shouldBe msg

      // make sure short packets return the right failure
      codec.decode(bits.drop(4).dropRight(8)) match {
        case Attempt.Successful(_) => fail("Short packet should have failed decode, but succeeded instead.")
        case Attempt.Failure(InsufficientBits(_,_,_)) => // "Yes, we have no bits."
        case Attempt.Failure(_) => fail("Short packet failed with error other than InsufficientBits")
      }

      // long packet should be OK, but return the correct remaining length
      codec.decode(bits.drop(4) ++ hex"0102".bits).require match {
        case DecodeResult(pkt, remaining) =>
          pkt shouldBe msg
          remaining.bytes.length shouldBe 2
        case _ => fail("Unexpected return type from mqttCodec.decode of long packet")
      }
    }

    "encode and decode CONNECT packets" in {
      // note: when testing the packet-specific codec directly, the type must be
      // dropped first, since normally the discriminator codec takes it
      ConnectPacket.codec.encode(connectMessage).require shouldBe connectBits.drop(4)
      ConnectPacket.codec.decode(connectBits.drop(4)).require.value shouldBe connectMessage

      // short packet
      an [IllegalArgumentException] shouldBe thrownBy {
        ConnectPacket.codec.decode(connectBits.drop(4).dropRight(8)).require.value shouldBe connectMessage
      }

      // going through the Coproduct (discriminator) codec, the standard use case
      mqttCodec.encode(connectMessage).require shouldBe connectBits
      mqttCodec.decode(connectBits).require.value shouldBe connectMessage

      // long packet should be OK, but return the correct remaining length
      mqttCodec.decode(connectBits ++ hex"0102".bits).require match {
        case DecodeResult(pkt, remaining) =>
          pkt shouldBe connectMessage
          remaining.bytes.length shouldBe 2
        case _ => fail("Unexpected return type from mqttCodec.decode of long packet")
      }
    }
    
    "emcpde amd decpde CONNACK packets" in {

      def connAckBits(sess: Boolean, ret: Int) = ((hex"2002" ++ (if (sess) hex"01" else hex"00")) :+ ret.toByte).bits

      for (ret <- 0 to 5;
           sess <- List(true, false)) {
        checkEncodeDecode(ConnAckPacket.codec, ConnAckPacket(session = sess, ret), connAckBits(sess, ret))
      }

      // Connection Acknowledgement
//      val connAckBitsNormal =
//      val connAckNormal =
//      checkEncodeDecode(ConnAckPacket.codec, ConnAckPacket(session = false, 0), connAckBitsNormal)
//
//      val connAckBitsSessionPresent = hex"20 02 01 00".bits
//      val connAckSessionPresent = ConnAckPacket(session = true, 0)
//      checkEncodeDecode(ConnAckPacket.codec, ConnAckPacket(session = false, 0), connAckBitsNormal)
//
//
//      val connAckBitsReturnMin = hex"20 02 00 00".bits
//      val connAckBitsReturnMax = hex"20 02 00 05".bits
//      val connAckBitsBadReturn = hex"20 02 00 09".bits
//      val connAckBitsBadFlag = hex"20 02 07 00".bits
//
//      val connAckReturnMin = ConnAckPacket(session = true, 0)
//      val connAckReturnMax = ConnAckPacket(session = true, 0)
//
//      ConnAckPacket.codec.encode(connAckNormal).require shouldBe connAckBitsNormal.drop(4)
//      ConnAckPacket.codec.decode(connAckBitsNormal.drop(4)).require.value shouldBe connAckNormal
//
//      ConnAckPacket.codec.encode(connAckSessionPresent).require shouldBe connAckBitsSessionPresent.drop(4)
//      ConnAckPacket.codec.decode(connAckBitsSessionPresent.drop(4)).require.value shouldBe connAckSessionPresent

      (0 to 5).foreach { n =>
        val msg = ConnAckPacket(session = false, n)
        val bits = connAckBits(sess = false, n).drop(4)
        ConnAckPacket.codec.encode(msg).require shouldBe bits
        ConnAckPacket.codec.decode(bits).require.value shouldBe msg
      }

      ConnAckPacket.codec.decode(connAckBits(false, 9)) match {
        case Successful(DecodeResult(connack, _)) => fail("No error returned for illegal value.")
        case _ =>
      }
    }

    "encode and decode PUBLISH packets" in {
      import PublishPacket.PublishFlags

      checkEncodeDecode(PublishPacket.codec, publishMessage, publishBits)

      val allFlags = for (
        qos <- 0 to 2;
        dup <- List(true, false);
        retain <- List(true, false) if !(dup & qos == 0)) yield PublishFlags(dup, qos, retain)

      def pflagBits(flags: PublishFlags) = {
        BitVector.bit(flags.dup) ++ BitVector.fromInt(flags.qos, 2) ++ BitVector.bit(flags.retain)
      }

      def topicBits(topic: String) = BitVector.fromInt(topic.length, 16) ++ BitVector(topic.getBytes)

      val topics: List[String] = List("a", "a/b", "c")
      val packetIds: List[Option[Int]] = List(Some(0x55aa), None)

      val payload = hex"11223344"

      for (flags <- allFlags;
           topic <- topics;
           packetId <- packetIds) {

        val pidLen = if (flags.qos == 1 || flags.qos == 2) 2 else 0
        val packetId = if (pidLen > 0) Some(0xaabb) else None
        val msg = PublishPacket(flags, topic, packetId, payload)
        val len = pidLen + 2 + topic.length + 4

        val bits =
          BitVector.fromInt(3, 4) ++
          pflagBits(flags) ++
          BitVector.fromInt(len, 8) ++
          topicBits(topic) ++
          (if (pidLen > 0) hex"aabb".bits else BitVector.empty) ++
          payload.bits

        //println(s"<$bits> $msg")

        checkEncodeDecode(PublishPacket.codec, msg, bits)

        val badBits =
          BitVector.fromInt(3, 4) ++
            (pflagBits(flags) or bin"1") ++
            BitVector.fromInt(len, 8) ++
            topicBits(topic) ++
            (if (pidLen > 0) hex"aabb".bits else BitVector.empty) ++
            payload.bits

        PublishPacket.codec.decode(badBits.drop(4)) shouldBe an [Attempt.Failure]
      }

      // TODO: check UTF codes disallowed in spec
      // TODO: check allowed filter characters
    }

    "encode and decode PUBACK packets" in {
      checkEncodeDecode(PubAckPacket.codec, PubAckPacket(0xaabb), hex"40 02 aa bb".bits)
    }

    "encode and decode PUBREC packets" in {
      checkEncodeDecode(PubRecPacket.codec, PubRecPacket(0xaabb), hex"50 02 aa bb".bits)
    }

    "encode and decode PUBREL packets" in {
      checkEncodeDecode(PubRelPacket.codec, PubRelPacket(0xaabb), hex"62 02 aa bb".bits)
    }

    "encode and decode PUBCOMP packets" in {
      checkEncodeDecode(PubCompPacket.codec, PubCompPacket(0xaabb), hex"70 02 aa bb".bits)
    }

    "encode and decode SUBSCRIBE packets" in {
      import SubscribePacket.TopicFilter

      val topics = List(
        TopicFilter("a/b", 0),
        TopicFilter("c", 1))
      val subMsg = SubscribePacket(0x1122, topics)

      val hd = hex"82 0c".bits
      val pktId = hex"1122".bits
      val topicListBits = hex"0003 612f62 00 0001 63 01".bits
      val subMsgBits = hd ++ pktId ++ topicListBits

      checkEncodeDecode(SubscribePacket.codec, subMsg, subMsgBits)

      SubscribePacket.codec.encode(SubscribePacket(0x1122, List())) shouldBe an [Attempt.Failure]
      SubscribePacket.codec.decode(hex"82 02 1122".bits) shouldBe an [Attempt.Failure]
    }

    "encode and decode SUBACK packets" in {
      pending
    }

    "encode and decode UNSUBSCRIBE packets" in {
      pending
    }

    "encode and decode UNSUBACK packets" in {
      checkEncodeDecode(UnsubAckPacket.codec, UnsubAckPacket(0xaabb), hex"b0 02 aa bb".bits)
    }

    "encode and decode PINGREQ packets" in {
      PingReqPacket.codec.encode(pingReqMessage).require shouldBe pingReqBits.drop(4)
      PingReqPacket.codec.decode(pingReqBits.drop(4)).require.value shouldBe pingReqMessage

      // TODO: short packets in a length codec, which for the shortest packets like PINGREQ
      // can't be avoided, throw a different exception (IndexOutOfBounds)
      // than short packets in simpler encoders, which throw IllegalArgumentException
      an [IndexOutOfBoundsException] should be thrownBy {
        PingReqPacket.codec.decode(pingReqBits.drop(4).dropRight(8)).require.value shouldBe pingReqMessage
      }

      mqttCodec.encode(pingReqMessage).require shouldBe pingReqBits
      mqttCodec.decode(pingReqBits).require.value shouldBe pingReqMessage
    }

    "encode and decode PINGRESP packets" in {
      PingRespPacket.codec.encode(pingRespMessage).require shouldBe pingRespBits.drop(4)
      PingRespPacket.codec.decode(pingRespBits.drop(4)).require.value shouldBe pingRespMessage

      an [IndexOutOfBoundsException] should be thrownBy {
        PingRespPacket.codec.decode(pingReqBits.drop(4).dropRight(8)).require.value shouldBe pingRespMessage
      }

      mqttCodec.encode(pingRespMessage).require shouldBe pingRespBits
      mqttCodec.decode(pingRespBits).require.value shouldBe pingRespMessage
    }

  }
}

object MqttFixtures {
  import MqttCodec._
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

  val connectBitsBadLen = connectBits.dropRight(8)

  val connectMessage = ConnectPacket(
    "MQTT",
    4,
    flags = ConnectPacket.ConnectFlags(true,true,false,0,true,true),
    keepAlive = 0,
    clientId = "clientid",
    willTopic = Some("/topic"),
    willMessage = Some(hex"11223344"),
    username = Some("user"),
    password = Some(hex"70617373"))

  val pingReqBits = hex"c0 00".bits
  val pingReqMessage = PingReqPacket()

  val pingRespBits = hex"d0 00".bits
  val pingRespMessage = PingRespPacket()

  val publishFlag0 = bin"0000"
  val publishFlagQos1 = bin"0010"
  val publishFlagQos1Dup = bin"1010"
  val publishFlagQos1DupRetain = bin"1011"

  val publishBits = hex"""
    32 0b
    0003 612f62
    1122
    aabbccdd
    """.bits

  val publishMessage = PublishPacket(
    flags = PublishPacket.PublishFlags(dup = false, qos = 1, retain = false),
    topic = "a/b",
    packetId = Some(0x1122),
    payload = hex"aabbccdd")
}
