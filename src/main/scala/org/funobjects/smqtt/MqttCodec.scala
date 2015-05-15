package org.funobjects.smqtt

import org.scalactic.Accumulation._
import org.scalactic.{Good, Bad, Or, Every}
import scodec._
import scodec.bits._
import scodec.codecs._
import shapeless.HNil


/**
 * Simple MQTT Model
 */
object MqttCodec {

  val vstring = variableSizeBytes(uint16, utf8)
  val vint28 = VariableInt28.intCodec
  val packetIdCodec = variableSizeBytes(vint28, uint16)

  def mqttPacketType(n: Int) = BitVector.fromInt(n, 4)

  def apply = mqttCodec

  // filter for UTF "non-characters" and control characters
  //def badUtf(cp: Int): Boolean = { cp >= 1 && cp <= 0x1f || (cp >= 0xfdd0 && cp <= 0xfdef) || (cp & 0xffff) == 0xffff }

  case class MqttErr(errors: Every[MqttError], context: List[String] = Nil) extends Err {
    def refsString = errors.toList.map(_.ref).mkString(",")
    def msgString = errors.toList.map(_.msg).mkString("; ")
    override def message: String = s"$msgString Protocol Reference: $refsString"
    override def pushContext(ctx: String) = copy(context = ctx :: context)
  }
  
  object MqttErr {
    def apply(protoRef: String, msg: String): MqttErr = MqttErr(MqttError(protoRef, msg))
  }

  sealed trait ControlPacket {
    def validate: Attempt[_] = Attempt.Successful(this)
  }

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
    password: Option[ByteVector]) extends ControlPacket {

    override def validate = this match {
      // note: since unsupported protocol level requires an application level response,
      // it is not checked here

      case ConnectPacket(name, _, _, _, _, _, _, _, _) if name != "MQTT" =>
        Attempt.Failure(MqttErr("[MQTT-3.1.2-1]", s"Unsupported protocol ($name)."))

      case ConnectPacket(_, _, f, _, _, _, _, _, _) if !f.willFlag && (f.willQos != 0) =>
        Attempt.Failure(MqttErr("[MQTT-3.1.2-13]", s"Inconsistent willFlag and willQos values."))

      case ConnectPacket(_, _, f, _, _, _, _, _, _) if !f.willFlag && f.willRetain =>
        Attempt.Failure(MqttErr("[MQTT-3.1.2-15]", s"Inconsistent willFlag and willRetain values."))

      case ConnectPacket(_, _, f, _, _, wTopic, _, _, _) if wTopic.isDefined && !f.willFlag =>
        Attempt.Failure(MqttErr("[MQTT-3.1.3-10]", s"Inconsistent willFlag and willTopic values."))

      case pkt: ConnectPacket => Attempt.successful(pkt)
    }
  }

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
            ("password" | conditional(cf.hasPassword, vbytes))))))
      .dropUnits
      .as[ConnectPacket]
      .exmap(conn => conn.validate, conn => conn.validate)
  }

  case class ConnAckPacket(session: Boolean, ret: Int) extends ControlPacket
  object ConnAckPacket {
    val packetType = mqttPacketType(0x2)
    val packetFlags = bin"0000"

    implicit val discriminator: Discriminator[ControlPacket, ConnAckPacket, BitVector] = Discriminator(packetType)
    implicit val codec: Codec[ConnAckPacket] = (constant(packetFlags) :: variableSizeBytes(vint28, constant(BitVector.low(7)) :~>:  bool :: uint8)).dropUnits.as[ConnAckPacket]
  }

  case class PublishPacket(
    flags: PublishPacket.PublishFlags,
    topic: String,
    packetId: Option[Int],
    payload: ByteVector) extends ControlPacket {

    override def validate: Attempt[PublishPacket] = {
      if (flags.qos == 3)
        Attempt.Failure(MqttErr("[MQTT-3.3.1-4]", s"Invalid QoS (${flags.qos}})"))
      else if (flags.qos == 0 && flags.dup)
        Attempt.Failure(MqttErr("[MQTT-3.3.1-2]", s"Dup flag set with QoS 0."))
      else if (flags.qos == 0 && packetId.isDefined)
        Attempt.Failure(MqttErr("[MQTT-2.3.1-5]", s"Packet ID present with QoS 0."))
      else {
        Topic.topicLevels(topic) match {
          case Good(_) => Attempt.successful(this)
          case Bad(errs) => Attempt.failure(MqttErr(errs))
        }
      }
    }
  }

  object PublishPacket {
    val packetType = mqttPacketType(0x3)

    case class PublishFlags(dup: Boolean, qos: Int, retain: Boolean) {
      def packetIdPresent: Boolean = qos == 1 || qos == 2
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
          ("payload" | bytes))))
      .as[PublishPacket]
      .exmap(pub => pub.validate, pub => pub.validate)
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

  case class SubscribePacket(packetId: Int, filters: List[SubscribePacket.TopicFilter]) extends ControlPacket {
    override def validate: Attempt[SubscribePacket] = {
      if (filters.size == 0)
        Attempt.failure(MqttErr("[MQTT-3.8.3-3]", "Subscribe packet must contain at least one topic filter."))
      else
        Attempt.successful(this)
    }

  }
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
        ("filters" | list(TopicFilter.codec))))
        .dropUnits
        .as[SubscribePacket]
        .exmap(sub => sub.validate, sub => sub.validate)

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
