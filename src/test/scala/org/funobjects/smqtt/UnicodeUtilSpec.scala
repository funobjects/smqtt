package org.funobjects.smqtt

import org.scalatest.{WordSpec, Matchers}

/**
 * Created by rgf on 5/2/15.
 */
class UnicodeUtilSpec extends WordSpec with Matchers {
  "The codePoints method" should {
    "translate strings into an array of Unicode code points." in {

      import UnicodeUtil.codePoints

      codePoints("abc") shouldBe Array(0x61, 0x62, 0x63)
      codePoints("\ud869\uded4 abc") shouldBe Array(0x2A6D4, 0x20, 0x61, 0x62, 0x63)
    }

    "support correct encoding of UTF-8 'Zero Width No-Break Space' code point" in {
      // this is more of a test of the underlying JDK unicode support, but it is
      // called out specifically [MQTT-1.5.3-3]

      val utf8Bytes = Array(0xEF, 0xBB, 0xBF).map(_.toByte)
      (new String(utf8Bytes, "UTF8")).codePointAt(0) shouldBe 0xfeff

    }

  }
}
