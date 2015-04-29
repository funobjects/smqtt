import org.funobjects.smqtt._
import org.funobjects.smqtt.MqttCodecs._
import org.funobjects.smqtt.SomethingProto._
import org.scalatest._
import org.scalatest.Matchers._
import scodec._
import scodec.bits._
import scodec.codecs._

val a = utf8.encode("hello")
val b1 = bin"11011"
val codec = uint8.xmap( (a: Int) => a.toString, (s: String) => Integer.parseInt(s))

bin"1010"

BitVector.fromInt(0xd, 4)




codec.encode("3")

codecs.bits(1).encode(bin"10")

implicitly[Codec[Something]].encode(SoftThing(3))
implicitly[Codec[Something]].encode(NullThing())
implicitly[Codec[ControlPacket]].encode(PingReqPacket())

implicitly[Codec[Something]].decode(hex"0300".bits).require.value

implicitly[Codec[ControlPacket]].decode(hex"c000".bits).require.value
