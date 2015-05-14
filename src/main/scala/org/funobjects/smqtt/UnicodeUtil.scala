package org.funobjects.smqtt

import scodec._
import scodec.bits._

import scala.annotation.tailrec

/**
 * General Unicode utilities.
 */
object UnicodeUtil {
  /**
   * Get a list of code points from a String.
   *
   * This is only here to avoid a dependency on Java 8.
   *
   * @param s A string
   * @return An Array of Unicode code points.
   */
  def codePoints(s: String): Array[Int] = {
    @tailrec
    def next(s: String, codePoints: Array[Int]): Array[Int] =
      if (s.length == 0)
        codePoints
      else {
        val codePoint = s.codePointAt(0)
        val count = Character.charCount(codePoint)
        next(s.substring(count), codePoints :+ codePoint)
      }
    next(s, Array.empty[Int])
  }

  /**
   * A conservative UTF-8 byte verifier, which converts bytes
   * to code points.
   *
   * @param bytesToDecode   The bytes to decode.
   *
   * @return If Successful, an Attempt.Successful with the list
   *         of code points, otherwise an Attempt.Failure[Err]
   *         describing the problem.
   */

  def bytesToCodePoints(bytesToDecode: ByteVector): Attempt[List[Int]] = {

    @tailrec
    def nextByte(bytes: ByteVector, points: List[Int]): Attempt[List[Int]] = {

      //println(s"nextByte(${bytes.toHex}, $points)")

      @tailrec
      def nextCodePoint(bv: ByteVector, acc: BitVector, expected: Int): Attempt[(BitVector, ByteVector)] = {
        //println(s"nextCodePoint(${bv.toHex}, ${acc} (${acc.length}/${acc.populationCount}}), $expected)")
        if (expected == 0)
          Attempt.successful((acc, bv)).flatMap(validateCodePoint)
        else {
          if (bv.length == 0)
            Attempt.failure(Err.General("Insufficient bytes for multi-byte sequence.", List("UTF8:decode")))
          else {
            val b = bv.take(1).bits
            if (!b.startsWith(bin"10"))
              Attempt.failure(Err.General(s"Improper continuation byte in multi-byte sequence (${b.toHex}).", List("UTF8:decode")))
            else
              nextCodePoint(bv.drop(1), acc ++ b.drop(2), expected - 1)
          }
        }
      }

      if (bytes.length == 0) {
        Attempt.successful(points)
      } else {
        val b = bytes.take(1).bits
        // look at the high bit
        if (b(0)) {
          // the index of the first "10" pattern also happens to be the count of additional bytes,
          // assuming all bits to the left are 1 (which must be verified)
          val index = b.indexOfSlice(bin"10")
          if (index <= 0 || index > 5)
            Attempt.failure(Err.General(s"Badly formed initial byte of multi-byte sequence (${b.toHex}).", List("UTF8:decode")))
          else if (b.slice(0, index).populationCount != index)
            Attempt.failure(Err.General(s"Badly formed initial byte of multi-byte sequence (${b.toHex}).", List("UTF8:decode")))
          else {

            nextCodePoint(bytes.drop(1), b.drop(index + 2), index.toInt) match {
              case Attempt.Successful((codePoint, remaining)) =>
                // we can detect overlong encoding by comparing the number of
                // bytes decoded with what the code point should take
                //println(s"code point check: bytes used: ${bytes.length - remaining.length}, bytesForCodepoint: ${bytesForCodepoint(codePoint)}")
                if ((bytes.length - remaining.length) != bytesForCodepoint(codePoint))
                  Attempt.failure(Err.General(s"Malformed (overlong) encoding for code point ${codePoint.toHex}.", List("UTF8:decode")))
                else
                  nextByte(remaining, points :+ codePoint.toInt(signed = false))

              case fail: Attempt.Failure => fail
            }
          }
        } else {
          // high bit was 0, so we already have the complete codePoint
          nextByte(bytes.drop(1), points :+ b.toInt(signed = false))
        }
      }
    }

    nextByte(bytesToDecode, List.empty)
  }

  /**
   * Code point validator for strict (i.e. non-modified) UTF-8.
   *
   * Works on a tuple to allow the remaining input to be carried
   * when used, for instance, with flatMap to filter incoming
   * code points.
   */
  def validateCodePoint(codePointAndRemaining: (BitVector, ByteVector)): Attempt[(BitVector, ByteVector)] = {
    val (cpBits, remaining) = codePointAndRemaining
    val cp = cpBits.toInt(signed = false)
    val bad = Set(192, 193) ++ (245 to 255).toSet
    if (cp > 0x10ffff)
      Attempt.Failure(Err.General(s"Code points > 10FFFF not supported in strict UTF-8 ($cp).", List("UTF8:decode")))
    else if (cp >= 0xD800 && cp <= 0xDFFF)
      Attempt.Failure(Err.General(s"UTF-16 surrogate halves are not valid in strict UTF-8 ($cp).", List("UTF8:decode")))
    else if (cp < 0 || bad.contains(cp))
      Attempt.Failure(Err.General(s"Invalid code point ($cp).", List("UTF8:decode")))
    else
      Attempt.successful((cpBits, remaining))
  }

  // 7                      // 7   -> 1
  // 5, 6                   // 11  -> 2
  // 4, 6, 6                // 16  -> 3
  // 3, 6, 6, 6             // 21  -> 4
  def bytesForCodepoint(value: BitVector): Int = highestBit(value) match {
    case n: Int if n <= 7  => 1
    case n: Int if n <= 11 => 2
    case n: Int if n <= 16 => 3
    case n: Int if n <= 21 => 4
    case _ => 0
  }

  def highestBit(value: BitVector): Int = {
    @tailrec
    def hiBit(bv: BitVector, count: Int): Int = {
      if (bv.length == 0)
        -1
      else if (bv(0))
        value.length.toInt - (count+1)
      else
        hiBit(bv.drop(1), count + 1)
    }
    hiBit(value, 0)
  }
}
