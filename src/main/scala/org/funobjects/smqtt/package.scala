package org.funobjects

import scodec.Codec
import scodec.codecs._

import shapeless._
import shapeless.syntax.singleton._

/**
 * Created by rgf on 4/28/15.
 */
package object smqtt {
  //def somethingCodec = Codec.coproduct[Something].discriminatedBy(uint8).auto

//  def somethingCodec = Codec.coproduct[Something].discriminatedBy(uint8).using(
//    'GreenThing ->> 1 :: 'SoftThing ->> 2 :: HNil)

}
