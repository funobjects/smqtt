package org.funobjects.smqtt

import scodec.Err.General
import scodec._
import scodec.bits._
import scodec.codecs._
import shapeless.HNil

import scala.annotation.tailrec

/**
 * Represents an unencoded RL (Remaining Length) field,
 * uses a variable length encoding scheme for representation
 * of an unsigned 28-bit integer in 1-4 bytes.
 */
case class VariableInt4(int: Int) {
  require(int >= 0 && int < 0x10000000)
}

object VariableInt4 {
  implicit val codec: Codec[VariableInt4] = new VariableInt4Codec
  val intCodec: Codec[Int] = codec xmap (v => v.int, n => VariableInt4(n))
}

sealed trait ConnectStatus {
  val value: Int
  val msg: String
}
object ConnectionAccepted extends ConnectStatus { val value = 0; val msg = "Connection accepted." }
object ConnectionRefusedBadProto extends ConnectStatus { val value = 1; val msg = "Unsupported Protocol Level" }

sealed trait ControlPacket
object ControlPacket {
  implicit val discriminated: Discriminated[ControlPacket, Int] = Discriminated(uint4)
}

case class FixedHeader(flags: Int, remainingLength: VariableInt4)
object FixedHeader {
  implicit val codec = (uint4 :: VariableInt4.codec).as[FixedHeader]
}

case class ConnectPacket(
  header: FixedHeader,
  protoName: String,
  protoLevel: Int,
  flags: ConnectPacket.ConnectFlags,
  keepAlive: Int,
  clientId: String,
  willTopic: Option[String],
  willMessage: Option[ByteVector],
  username: Option[String],
  password: Option[ByteVector]
) extends ControlPacket

object ConnectPacket {

  val packetFlags = constant(hex"0")

  implicit val discriminator: Discriminator[ControlPacket, ConnectPacket, Int] = Discriminator(0x1)

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

  val vstring = variableSizeBytes(uint16, utf8)
  val vbytes = variableSizeBytes(uint16, bytes)

  implicit val codec: Codec[ConnectPacket]= (FixedHeader.codec flatPrepend( hdr =>
      fixedSizeBytes(hdr.remainingLength.int,
        ("protoName" | vstring) ::
        ("protoVersion" | uint8) :: (
          ("connectFlags" | ConnectFlags.codec) flatPrepend ((cf: ConnectFlags) =>
            ("keepAlive" | uint16) ::
            ("clientId" | vstring) ::
            ("willTopic" | conditional(cf.willFlag, vstring)) ::
            ("willMessage" | conditional(cf.willFlag, vbytes)) ::
            ("username" | conditional(cf.hasUserName, vstring)) ::
            ("password" | conditional(cf.hasPassword, vbytes))))))).as[ConnectPacket]
}

case class ConnAckPacket(header: FixedHeader, sessionPresent: Boolean, ret: Int) extends ControlPacket
object ConnAckPacket {
  implicit val discriminator: Discriminator[ControlPacket, ConnAckPacket, Int] = Discriminator(0x2)
  implicit val codec: Codec[ConnAckPacket] = (FixedHeader.codec :: constant(BitVector.low(7)) :: bool :: uint8).dropUnits.as[ConnAckPacket]
}

case class PingReqPacket(header: FixedHeader) extends ControlPacket
object PingReqPacket {
  implicit val discriminator: Discriminator[ControlPacket, PingReqPacket, Int] = Discriminator(0xc)
  implicit val codec: Codec[PingReqPacket] = FixedHeader.codec.as[PingReqPacket]
}

case class PingRespPacket(header: FixedHeader) extends ControlPacket
object PingRespPacket {
  implicit val discriminator: Discriminator[ControlPacket, PingRespPacket, Int] = Discriminator(0xd)
  implicit val codec: Codec[PingRespPacket] = FixedHeader.codec.as[PingRespPacket]
}

//case class FixedHeader(packetType: Int, bit3: Boolean, bit2: Boolean, bit1: Boolean, bit0: Boolean, remainingLength: VariableInt4)
case class PublishFixedHeader(packetType: Int, dup: Boolean, qos: Int, retain: Boolean, remainingLength: VariableInt4)
case class VariableHeader(bytes: ByteVector)

case class PublishPacket() extends ControlPacket
case class PubAckPacket() extends ControlPacket
//case object PubRec extends ControlPacket(5)
//case object PubRel extends ControlPacket(6)
//case object PubComp extends ControlPacket(7)
//case object Subscribe extends ControlPacket(8)
//case object SubAck extends ControlPacket(9)
//case object Unsubscribe extends ControlPacket(10)
//case object UnsubAck extends ControlPacket(11)
//case object Disconnect extends ControlPacket(14)


//case object ConnAck extends ControlPacket(2)
//case object Publish extends ControlPacket(3)
//case object PubAck extends ControlPacket(4)
//case object PubRec extends ControlPacket(5)
//case object PubRel extends ControlPacket(6)
//case object PubComp extends ControlPacket(7)
//case object Subscribe extends ControlPacket(8)
//case object SubAck extends ControlPacket(9)
//case object Unsubscribe extends ControlPacket(10)
//case object UnsubAck extends ControlPacket(11)
//case object PingReq extends ControlPacket(12)
//case object PingResp extends ControlPacket(13)
//case object Disconnect extends ControlPacket(14)

/**
 * Simple MQTT Model
 */
object MqttCodecs {

  sealed trait MqttMessage {
    val fixedHeader: FixedHeader
    val variableHeader: Option[VariableHeader]
    val payload: ByteVector
  }

  val controlPacket = discriminated[ControlPacket].by(codecs.bits(4))

  implicit val sizedUtf8 = variableSizeBytes(uint16, utf8)
  implicit val varInt4: VariableInt4Codec = new VariableInt4Codec

  case class Foo(a: Int, b: String)
  implicit val someCodec: Codec[Foo] = (uint4 :: sizedUtf8).as[Foo]

  //implicit val fixedHeaderCodec = (codecs.bits(4) :: bool :: bool :: bool :: bool).as[FixedHeader]
  //implicit val publishHeader = (constant(BitVector(4,1)) :: bool :: codecs.bits(2) :: bool :: varInt4).as[PublishFixedHeader]

  implicit val variableHeader = variableSizeBytes(uint16, bytes)
}

class VariableInt4Codec extends Codec[VariableInt4] {
  import VariableInt4Codec._

  override def sizeBound: SizeBound = SizeBound.atMost(maxBytes)

  /**
   * The encoding scheme for the MQTT Remaining Length (RL) field encodes
   * an integer between 0 and 268,435,455 (28 bits) into 1-4 bytes, by
   * encoding 7 bits per byte, with the high bit used as a continuation flag
   */

  override def decode(bits: BitVector): Attempt[DecodeResult[VariableInt4]] = {
    @tailrec
    def bytesToInt(bytes: ByteVector, n: Int, mult: Int = 1): Attempt[DecodeResult[VariableInt4]] = {
      val curByte: Byte = bytes(0)
      val curVal: Byte = (curByte & 0x7f).toByte
      val remainingBytes = bytes.drop(1)
      val total = n + (curVal * mult)
      if ((curByte & 0x80) == 0)
        Attempt.successful(DecodeResult(VariableInt4(total), remainingBytes.bits))
      else
        bytesToInt(bytes.drop(1), total, mult * 128)
    }
    bytesToInt(bits.bytes, 0)
  }

  override def encode(value: VariableInt4): Attempt[BitVector] = {
    @tailrec
    def intToBytes(n: Int, bytes: ByteVector): Attempt[ByteVector] = {
      if (bytes.length >= maxBytes)
        Attempt.failure(new Err.General(s"Attempt to encode greater than $maxBytes bytes failed."))
      else {
        val lower7: Byte = (n % 0x80).toByte
        val remaining: Int = n / 0x80
        if (remaining == 0)
          Attempt.successful(bytes :+ lower7)
        else
          intToBytes(remaining, bytes :+ (lower7 | 0x80).toByte)
      }
    }
    intToBytes(value.int, ByteVector.empty).map(_.bits)
  }
}

object VariableInt4Codec {
  val maxBytes = 4
  val minValue = 0
  val maxValue = 0xfffffff
}
