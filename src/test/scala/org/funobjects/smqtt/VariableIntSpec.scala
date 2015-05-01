package org.funobjects.smqtt

import org.scalatest._
import org.scalatest.Inside._

import scodec._
import scodec.bits._

/**
 * Created by rgf on 4/11/15.
 */
class VariableIntSpec extends FlatSpec with Matchers
{
  val codec = new VariableInt28Codec

  "VariableInt4" should "have consistent and correct encode and decode" in {

    check(0, hex"00")
    check(1, hex"01")
    check(127, hex"7f")
    check(128, hex"8001")
    check(16383, hex"ff7f")
    check(16384, hex"808001")
    check(2097151, hex"ffff7f")
    check(2097152, hex"80808001")
    check(268435455, hex"ffffff7f")
  }

  def check(n: Int, bytes: ByteVector): Unit = {
    val attempt = codec.encode(VariableInt28(n))
    attempt shouldBe an [Attempt.Successful[_]]
    inside (attempt) {
      case Attempt.Successful(bits: BitVector) =>
        bits.bytes shouldBe bytes

        codec.decode(bytes.bits) match {
          case Attempt.Successful(DecodeResult(VariableInt28(decoded), _)) => decoded shouldBe n
          case Attempt.Failure(_) => fail("unable decode after encoding")
        }

      case _ => fail("can't get vector for check")
    }
  }

  it should "throw an exception for values that are too large or too small" in {
    an [IllegalArgumentException] should be thrownBy VariableInt28(268435456)
    an [IllegalArgumentException] should be thrownBy VariableInt28(Integer.MAX_VALUE)
    an [IllegalArgumentException] should be thrownBy VariableInt28(-1)
    an [IllegalArgumentException] should be thrownBy VariableInt28(Integer.MIN_VALUE)
  }
}
