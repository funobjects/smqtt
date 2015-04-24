
import org.funobjects.smqtt._

import scodec._
import scodec.bits._
import scodec.codecs._
import scodec.codecs.implicits._
import shapeless.HNil

case class MyControlPacket(
  ptype: Int,
  flags: Int,
  bytes: ByteVector
)

object MyControlPacket {
  implicit def codec: Codec[MyControlPacket] =
    (uint4 :: uint4 :: variableSizeBytes(VariableInt4.intCodec, bytes)).as[MyControlPacket]
}

val controlPacket = uint4 :: uint4 :: variableSizeBytes(VariableInt4.intCodec, bytes)
controlPacket.encode( 3 :: 7 :: hex"01020304" :: HNil)
controlPacket.decode(hex"370401020304".bits)
val cp2 = Codec[MyControlPacket]
cp2.encode(MyControlPacket(3,7,hex"010203"))
cp2.decode(hex"370401020304".bits).require.value
val vstring = variableSizeBytes(uint16, utf8)
val combo = cp2.encode(MyControlPacket(3,7,vstring.encode("hello").require.bytes))


import org.funobjects.smqtt._
ConnectPacket.ConnectFlags.codec.encode(ConnectPacket.ConnectFlags(false, false, false, 0, false, false))
ConnectPacket.ConnectFlags.codec.decode(hex"26".bits).require.value

import ControlPacket._

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



val pingReqBits = hex"c000"
val pingRespBits = hex"d000"
val x = 40
val s = f"$x%x"

ConnectPacket.codec.decode(connectBits).require.value
