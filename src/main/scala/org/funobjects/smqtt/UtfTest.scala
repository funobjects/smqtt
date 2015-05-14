package org.funobjects.smqtt

import scodec.bits._

/**
 * Created by rgf on 5/3/15.
 */
object UtfTest extends App {
  val bytes = hex"61 62 63 20 e1 80 a9 20 f0 aa 9b 94"
  val byteArray = bytes.toArray
  (1 to 1000000).foreach { _ =>
    val s = UnicodeUtil.bytesToCodePoints(bytes)
  }
}
