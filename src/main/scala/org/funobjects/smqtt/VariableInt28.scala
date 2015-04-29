package org.funobjects.smqtt

import scodec.bits.{ByteVector, BitVector}
import scodec._

import scala.annotation.tailrec

/**
 * Represents an unencoded RL (Remaining Length) field, which
 * uses a variable length encoding scheme for representation
 * of an unsigned 28-bit integer in 1-4 bytes.
 *
 * @author Robert Fries
 */
case class VariableInt28(int: Int) {
  require(int >= 0 && int < 0x10000000)
}

object VariableInt28 {
  implicit val codec: Codec[VariableInt28] = new VariableInt28Codec
  val intCodec: Codec[Int] = codec xmap (v => v.int, n => VariableInt28(n))
}

class VariableInt28Codec extends Codec[VariableInt28] {
  import VariableInt28Codec._

  override def sizeBound: SizeBound = SizeBound.atMost(maxBytes)

  /**
   * The encoding scheme for the MQTT Remaining Length (RL) field encodes
   * an integer between 0 and 268,435,455 (28 bits) into 1-4 bytes, by
   * encoding 7 bits per byte, with the high bit used as a continuation flag
   */

  override def decode(bits: BitVector): Attempt[DecodeResult[VariableInt28]] = {
    @tailrec
    def bytesToInt(bytes: ByteVector, n: Int, mult: Int = 1): Attempt[DecodeResult[VariableInt28]] = {
      val curByte: Byte = bytes(0)
      val curVal: Byte = (curByte & 0x7f).toByte
      val remainingBytes = bytes.drop(1)
      val total = n + (curVal * mult)
      if ((curByte & 0x80) == 0)
        Attempt.successful(DecodeResult(VariableInt28(total), remainingBytes.bits))
      else
        bytesToInt(bytes.drop(1), total, mult * 128)
    }
    bytesToInt(bits.bytes, 0)
  }

  override def encode(value: VariableInt28): Attempt[BitVector] = {
    @tailrec
    def intToBytes(n: Int, bytes: ByteVector): Attempt[ByteVector] = {
      if (bytes.length >= maxBytes)
        Attempt.failure(new Err.General(s"Attempt to encode greater than $maxBytes bytes failed."))
      else {
        val lower7: Byte = (n % 0x80).toByte
        val remaining: Int = n / 0x80
        if (remaining == 0)
          Attempt.successful(bytes :+ lower7)
        else
          intToBytes(remaining, bytes :+ (lower7 | 0x80).toByte)
      }
    }
    intToBytes(value.int, ByteVector.empty).map(_.bits)
  }
}

object VariableInt28Codec {
  val maxBytes = 4
  val minValue = 0
  val maxValue = 0xfffffff
}
