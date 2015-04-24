package org.funobjects.smqtt

import java.nio.charset.Charset

import scodec.Codec
import scodec.codecs._

/**
 * Created by rgf on 4/13/15.
 */
object SomethingProto {
  implicit val charSet = Charset.forName("UTF-8")

  sealed trait Something
  object Something {
    implicit val discriminated: Discriminated[Something, Int] = Discriminated(uint8)
  }

  case class GreenThing(shade: String) extends Something
  object GreenThing {
    implicit val codec: Codec[GreenThing] = string.as[GreenThing]
    implicit val discriminator: Discriminator[Something, GreenThing, Int] = Discriminator(1)
  }

  case class SoftThing(fuzziness: Int) extends Something
  object SoftThing {
    implicit val codec: Codec[SoftThing] = uint8.as[SoftThing]
    implicit val discriminator: Discriminator[Something, SoftThing, Int] = Discriminator(2)
  }

  def somethingCodec = Codec.coproduct[Something].discriminatedBy(uint8).auto
}
