package org.funobjects.smqtt

import scodec.Err.General
import scodec._
import scodec.bits._
import scodec.codecs._
import shapeless.HNil

import scala.annotation.tailrec


/**
 * Simple MQTT Model
 */
object MqttCodecs {

  val vstring = variableSizeBytes(uint16, utf8)
  val vint28 = VariableInt28.intCodec
  val packetIdCodec = variableSizeBytes(vint28, uint16)

  def mqttPacketType(n: Int) = BitVector.fromInt(n, 4)

  sealed trait ControlPacket
  object ControlPacket {
    implicit val discriminated: Discriminated[ControlPacket, BitVector] = Discriminated(codecs.bits(4))
  }

  case class ConnectPacket(
    protoName: String,
    protoLevel: Int,
    flags: ConnectPacket.ConnectFlags,
    keepAlive: Int,
    clientId: String,
    willTopic: Option[String],
    willMessage: Option[ByteVector],
    username: Option[String],
    password: Option[ByteVector]) extends ControlPacket

  object ConnectPacket {
    val packetType = mqttPacketType(1)
    val packetFlags = bin"0000"

    implicit val discriminator: Discriminator[ControlPacket, ConnectPacket, BitVector] = Discriminator(packetType)

    case class ConnectFlags(
      hasUserName: Boolean,
      hasPassword: Boolean,
      willRetain: Boolean,
      willQos: Int,
      willFlag: Boolean,
      cleanSession: Boolean)

    object ConnectFlags {
      implicit val codec: Codec[ConnectFlags] =
        (bool :: bool :: bool :: uint2 :: bool :: bool :: constant(bin"0")).dropUnits.as[ConnectFlags]
    }

    val vbytes = variableSizeBytes(uint16, bytes)

    implicit val codec: Codec[ConnectPacket] = (
      constant(packetFlags) ::
      variableSizeBytes(vint28,
        ("protoName" | vstring) ::
        ("protoVersion" | uint8) :: (
          ("connectFlags" | ConnectFlags.codec) flatPrepend ((cf: ConnectFlags) =>
          ("keepAlive" | uint16) ::
            ("clientId" | vstring) ::
            ("willTopic" | conditional(cf.willFlag, vstring)) ::
            ("willMessage" | conditional(cf.willFlag, vbytes)) ::
            ("username" | conditional(cf.hasUserName, vstring)) ::
            ("password" | conditional(cf.hasPassword, vbytes)))))).dropUnits.as[ConnectPacket]
  }

  case class ConnAckPacket(sessionPresent: Boolean, ret: Int) extends ControlPacket
  object ConnAckPacket {
    val packetType = mqttPacketType(0x2)
    val packetFlags = bin"0000"

    implicit val discriminator: Discriminator[ControlPacket, ConnAckPacket, BitVector] = Discriminator(packetType)
    implicit val codec: Codec[ConnAckPacket] = (constant(packetFlags) :: variableSizeBytes(vint28, constant(BitVector.low(7)) :~>:  bool :: uint8)).dropUnits.as[ConnAckPacket]
  }

  case class PublishPacket(flags: PublishPacket.PublishFlags, topic: String, packetId: Option[Int], payload: ByteVector) extends ControlPacket
  object PublishPacket {
    val packetType = mqttPacketType(0x3)

    case class PublishFlags(dup: Boolean, qos: Int, retain: Boolean) {
      def packetIdPresent = (qos == 1 || qos == 2)
    }

    object PublishFlags {
      implicit val codec = (bool :: uint2 :: bool).as[PublishFlags]
    }

    implicit val discriminator: Discriminator[ControlPacket, PublishPacket, BitVector] = Discriminator(packetType)

    implicit val codec: Codec[PublishPacket] = (
      PublishFlags.codec flatPrepend (hdr =>
        variableSizeBytes(vint28,
          ("topic" | vstring ) ::
          ("packetId" | conditional(hdr.packetIdPresent, uint16)) ::
          ("payload" | bytes)))).as[PublishPacket]
  }

  case class PubAckPacket(packetId: Int) extends ControlPacket
  object PubAckPacket {
    val packetType = mqttPacketType(0x4)
    val packetFlags = bin"0000"
    implicit val discriminator: Discriminator[ControlPacket, PubAckPacket, BitVector] = Discriminator(packetType)
    implicit val codec: Codec[PubAckPacket] = (constant(packetFlags) :: packetIdCodec).dropUnits.as[PubAckPacket]
  }

  case class PubRecPacket(packetId: Int) extends ControlPacket
  object PubRecPacket {
    val packetType = mqttPacketType(0x5)
    val packetFlags = bin"0000"
    implicit val discriminator: Discriminator[ControlPacket, PubRecPacket, BitVector] = Discriminator(packetType)
    implicit val codec: Codec[PubRecPacket] = (constant(packetFlags) :: packetIdCodec).dropUnits.as[PubRecPacket]
  }

  case class PubRelPacket(packetId: Int) extends ControlPacket
  object PubRelPacket {
    val packetType = mqttPacketType(0x6)
    val packetFlags = bin"0010"
    implicit val discriminator: Discriminator[ControlPacket, PubRelPacket, BitVector] = Discriminator(packetType)
    implicit val codec: Codec[PubRelPacket] = (constant(packetFlags) :: packetIdCodec).dropUnits.as[PubRelPacket]
  }

  case class PubCompPacket(packetId: Int) extends ControlPacket
  object PubCompPacket {
    val packetType = mqttPacketType(0x7)
    val packetFlags = bin"0000"
    implicit val discriminator: Discriminator[ControlPacket, PubCompPacket, BitVector] = Discriminator(packetType)
    implicit val codec: Codec[PubCompPacket] = (constant(packetFlags) :: packetIdCodec).dropUnits.as[PubCompPacket]
  }

  case class SubscribePacket(packetId: Int, filters: List[SubscribePacket.TopicFilter]) extends ControlPacket
  object SubscribePacket {
    val packetType = mqttPacketType(0x8)
    val packetFlags = bin"0010"

    case class TopicFilter(filter: String, qos: Int) {
      require(qos >= 0 && qos <= 2, s"Invalid QoS: $qos")
    }
    object TopicFilter {
      implicit val codec: Codec[TopicFilter] = (vstring :: constant(bin"000000") :: uint2).dropUnits.as[TopicFilter]
    }

    implicit val discriminator: Discriminator[ControlPacket, SubscribePacket, BitVector] = Discriminator(packetType)
    implicit val codec: Codec[SubscribePacket] = (
      constant(packetFlags) ::
      variableSizeBytes(vint28,
        ("packetId" | uint16) ::
        ("filters" | list(TopicFilter.codec)))).dropUnits.as[SubscribePacket]
  }

  case class SubAckPacket(packetId: Int, returnCodes: List[Int]) extends ControlPacket
  object SubAckPacket {
    val packetType = mqttPacketType(0x9)
    val packetFlags = bin"0000"

    implicit val discriminator: Discriminator[ControlPacket, SubAckPacket, BitVector] = Discriminator(packetType)
    implicit val codec: Codec[SubAckPacket] = (
      constant(packetFlags) ::
      variableSizeBytes(vint28,
        ("packetId" | uint16) ::
        ("returnCodes" | list(uint8)))).dropUnits.as[SubAckPacket]
  }

  case class UnsubPacket(packetId: Int, filters: List[String]) extends ControlPacket
  object UnsubPacket {
    val packetType = mqttPacketType(0xa)
    val packetFlags = bin"0010"
    implicit val discriminator: Discriminator[ControlPacket, UnsubPacket, BitVector] = Discriminator(packetType)
    implicit val codec: Codec[UnsubPacket] = (constant(packetFlags) :: variableSizeBytes(vint28, uint16 :: list(vstring))).dropUnits.as[UnsubPacket]
  }

  case class UnsubAckPacket(packetId: Int) extends ControlPacket
  object UnsubAckPacket {
    val packetType = mqttPacketType(0xb)
    val packetFlags = bin"0000"
    implicit val discriminator: Discriminator[ControlPacket, UnsubAckPacket, BitVector] = Discriminator(packetType)
    implicit val codec: Codec[UnsubAckPacket] = (constant(packetFlags) :: packetIdCodec).dropUnits.as[UnsubAckPacket]
  }

  case class PingReqPacket() extends ControlPacket
  object PingReqPacket {
    val packetType = mqttPacketType(0xc)
    val packetFlags = bin"0000"
    implicit val discriminator: Discriminator[ControlPacket, PingReqPacket, BitVector] = Discriminator(packetType)
    implicit val codec: Codec[PingReqPacket] = (constant(packetFlags) :: vint28.unit(0)).dropUnits.as[PingReqPacket]
  }

  case class PingRespPacket() extends ControlPacket
  object PingRespPacket {
    val packetType = mqttPacketType(0xd)
    val packetFlags = bin"0000"
    implicit val discriminator: Discriminator[ControlPacket, PingRespPacket, BitVector] = Discriminator(packetType)
    implicit val codec: Codec[PingRespPacket] = (constant(packetFlags) :: vint28.unit(0)).dropUnits.as[PingRespPacket]
  }

  implicit def mqttCodec: Codec[ControlPacket] = Codec.coproduct[ControlPacket].auto
}
