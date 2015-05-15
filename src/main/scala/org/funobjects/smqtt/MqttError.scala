package org.funobjects.smqtt

import org.scalactic.One

import scala.language.implicitConversions

/**
 * Represents an error augmented with a protocol reference string.
 */
case class MqttError(msg: String, ref: String)

object MqttError {
  implicit def errToOne(err: MqttError): One[MqttError] = One(err)
}
