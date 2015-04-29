package org.funobjects.smqtt

import scodec.Codec
import scodec.codecs._
import shapeless._
import shapeless.syntax.singleton._

/**
 * Created by rgf on 4/13/15.
 */
object SomethingProto {

  sealed trait Something
  object Something {
    implicit val discriminated: Discriminated[Something, Int] = Discriminated(uint8)
  }

  case class GreenThing(shade: String) extends Something
  object GreenThing {
    implicit val codec: Codec[GreenThing] = utf8.as[GreenThing]
    implicit val discriminator: Discriminator[Something, GreenThing, Int] = Discriminator(1)
  }

  case class SoftThing(fuzziness: Int) extends Something
  object SoftThing {
    implicit val codec: Codec[SoftThing] = uint8.as[SoftThing]
    implicit val discriminator: Discriminator[Something, SoftThing, Int] = Discriminator(2)
  }

  case class NullThing() extends Something
  object NullThing {
    implicit val codec: Codec[NullThing] = VariableInt28.intCodec.unit(0).hlist.dropUnits.as[NullThing]
    implicit val discriminator: Discriminator[Something, NullThing, Int] = Discriminator(3)
  }

  implicit def somethingCodec: Codec[Something] = Codec.coproduct[Something].discriminatedBy(uint8).auto
//  def somethingCodec = Codec.coproduct[Something].discriminatedBy(uint8).using(
//    'GreenThing ->> 1 :: 'SoftThing ->> 2 :: HNil)
}



