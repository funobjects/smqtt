
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
    (uint4 :: uint4 :: variableSizeBytes(VariableInt28.intCodec, bytes)).as[MyControlPacket]
}

val controlPacket = uint4 :: uint4 :: variableSizeBytes(VariableInt28.intCodec, bytes)
controlPacket.encode( 3 :: 7 :: hex"01020304" :: HNil)
controlPacket.decode(hex"370401020304".bits)
val cp2 = Codec[MyControlPacket]
cp2.encode(MyControlPacket(3,7,hex"010203"))
cp2.decode(hex"370401020304".bits).require.value
val vstring = variableSizeBytes(uint16, utf8)
val combo = cp2.encode(MyControlPacket(3,7,vstring.encode("hello").require.bytes))

import MqttCodec._

ConnectPacket.ConnectFlags.codec.encode(ConnectPacket.ConnectFlags(false, false, false, 0, false, false))
ConnectPacket.ConnectFlags.codec.decode(hex"26".bits).require.value

MqttCodec.mqttCodec.decode(hex"20020000".bits)
