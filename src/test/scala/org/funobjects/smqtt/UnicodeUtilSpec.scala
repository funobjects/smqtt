package org.funobjects.smqtt

import org.scalatest.{WordSpec, Matchers}
import scodec.Attempt

import scodec.bits._

/**
 * Created by rgf on 5/2/15.
 */
class UnicodeUtilSpec extends WordSpec with Matchers {
  import UnicodeFixtures._

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

  "The bytesToCodePoints method" should {
    "only decode valid UTF-8 strings" in {
      UnicodeUtil.bytesToCodePoints(ByteVector(basic)) shouldBe Attempt.Successful(List(0x61, 0x62, 0x63))
      UnicodeUtil.bytesToCodePoints(ByteVector(basicMultiOk)) shouldBe Attempt.Successful(List(0x61, 0x62, 0x63, 0x20, 0x1029, 0x20, 0x2A6D4 ))
      UnicodeUtil.bytesToCodePoints(ByteVector(basicBom)) shouldBe Attempt.Successful(List(0xfeff, 0x61, 0x62, 0x63))

      UnicodeUtil.bytesToCodePoints(ByteVector(basicOver)) shouldBe an [Attempt.Failure]
      UnicodeUtil.bytesToCodePoints(ByteVector(basicMultiInvalid)) shouldBe an [Attempt.Failure]
      UnicodeUtil.bytesToCodePoints(ByteVector(basicMultiShort)) shouldBe an [Attempt.Failure]
      UnicodeUtil.bytesToCodePoints(ByteVector(basicMultiOver)) shouldBe an [Attempt.Failure]

      // make sure our behavior is consistent with scodec
      ByteVector(basic).decodeUtf8 shouldBe Right(new String(Array(0x61, 0x62, 0x63), 0, 3))
      ByteVector(basicMultiOk).decodeUtf8 shouldBe Right(new String(Array(0x61, 0x62, 0x63, 0x20, 0x1029, 0x20, 0x2A6D4 ), 0, 7))
      ByteVector(basicBom).decodeUtf8 shouldBe Right(new String(Array(0xfeff, 0x61, 0x62, 0x63), 0, 4))

      ByteVector(basicOver).decodeUtf8 shouldBe a [Left[_,_]]
      ByteVector(basicMultiInvalid).decodeUtf8 shouldBe a [Left[_,_]]
      ByteVector(basicMultiShort).decodeUtf8 shouldBe a [Left[_,_]]
      ByteVector(basicMultiOver).decodeUtf8 shouldBe a [Left[_,_]]
    }
  }
}

object UnicodeFixtures {
  val basic             = hex"  61 62 63                            ".toArray
  val basicOver         = hex"  61 62 63 20 f0 82 82 ac             ".toArray
  val basicOverNull     = hex"  61 62 63 c0 80                      ".toArray
  val basicBom          = hex"  ef bb bf 61 62 63                   ".toArray
  val basicMultiOk      = hex"  61 62 63 20 e1 80 a9 20 f0 aa 9b 94 ".toArray
  val basicMultiInvalid = hex"  61 62 63 20 e1 20 a9                ".toArray
  val basicMultiShort   = hex"  61 62 63 20 e1 80                   ".toArray
  val basicMultiOver    = hex"  61 62 63 20 e1 80 a9 20 f0 82 82 ac ".toArray
  val basicBad          = hex"  61 62 63 20 e1 80 a9 20 f0 82 82 ac ".toArray
}
