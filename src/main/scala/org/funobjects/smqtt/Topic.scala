package org.funobjects.smqtt

import org.scalactic._
import org.scalactic.Accumulation._

import scala.annotation.tailrec

/**
 * Represents a topic name as defined by MQTT
 */

object Topic {

  val wildSingle  = '+'
  val wildAll     = '#'
  val sep         = '/'
  val noMatch     = '$'

  def topicMatches(topic: String, filter: String): Boolean Or Every[MqttError] = {

    @tailrec
    def nextLevel(topicLevels: List[String], filterLevels: List[String]): Boolean Or Every[MqttError] = (topicLevels, filterLevels) match {
      case (Nil,Nil)          => Good(true)
      case (Nil,_)            => Good(false)

      // Note that after this point, _1 will be non-empty

      case (_, "#" :: Nil)  => Good(true)
      case (_, "#" :: _)    => Bad(MqttError("Multi-level wildcard must be alone or follow a topic separator, and be the last character in the filter.", "[MQTT-4.7.1-2]"))

      case (_ :: ttail, "+" :: ftail) => nextLevel(ttail, ftail)

      case (thead :: ttail, fhead :: ftail) if thead == fhead => nextLevel(topicLevels.tail, ftail)
      case (thead :: ttail, _) => Good(false)
    }

    val levels = topicLevels(topic) zip filterLevels(filter)
    levels flatMap { case (t, f) => nextLevel(t, f) }
  }

  def filterLevels(s: String): List[String] Or Every[MqttError] = {
    if (s.length == 0) {
      Bad(MqttError("Topic names and filters must be at least one character in length.", "[MQTT-4.7.3-1]"))
    } else {
      val levels = s.split(sep.toString, -1).toList
      levels
        .zipWithIndex
        .map(validLevel(_, levels.size-1))
        .combined
    }
  }

  def topicLevels(topic: String): List[String] Or Every[MqttError] = topic match {
    case s: String if s.length == 0 =>
      Bad(MqttError("Topic names and filters must be at least one character in length.", "[MQTT-4.7.3-1]"))

    case s: String if s.contains("\u0000") =>
      Bad(MqttError("Topic names may not contain the unicode null (0x0000) character.", "[MQTT-4.7.3-2]"))

    case s: String if containsWildcard(s) =>
      Bad(MqttError("Topic names may not contain wildcard characters.", "[MQTT-4.7.1-1]"))

    case s: String => Good(s.split(sep.toString, -1).toList)
  }

  def validLevel(strWithIndex: (String, Int), lastIndex: Int): String Or One[MqttError] = strWithIndex match {
    case (str, index) => str match {
      case level: String if level.contains("\u0000") =>
        Bad(MqttError("Topic filters may not contain the unicode null (U+0000) character.", "[MQTT-4.7.3-2]"))

      case level: String if level.contains(wildSingle) && level.length > 1 =>
        Bad(MqttError("Single-level wildcard must occupy an entire level.", "[MQTT-4.7.1-3]"))

      case level: String if level.contains(wildAll) && (level.length > 1 || index != lastIndex) =>
        Bad(MqttError("Multi-level wildcard must be alone or follow a topic separator, and be the last character in the filter.", "[MQTT-4.7.1-2]"))

      case level: String => Good(level)
    }
  }

  def isWildcard(char: Char) = char == '#' || char == '+'
  def nonMatchingTopic(s: String): Boolean = s.startsWith(noMatch.toString)
  def containsWildcard(str: String) = str exists isWildcard
}
