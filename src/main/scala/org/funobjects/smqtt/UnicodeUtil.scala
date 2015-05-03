package org.funobjects.smqtt

import scodec.Attempt
import scodec.bits.ByteVector

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

//  /**
//   * A conservative UTF-8 byte verifier, which converts bytes
//   * to code points.
//   *
//   * @param bytes   The bytes to convert.
//   * @return If Successful, a list of code points, otherwise a Failure[Err]
//   *         describing the problem.
//   */
//  def validUtf8Bytes(bytesToVerify: ByteVector): Attempt[List[Int]] = {
//    @tailrec
//    def getCodePoint(bytes: ByteVector, points: List[Int], List[]): Attempt[Int]
//  }
}
