package org.funobjects.smqtt

import org.scalactic.{Good, Bad}
import org.scalatest.{Matchers, WordSpec}

/**
 * Created by rgf on 5/14/15.
 */
class TopicSpec extends WordSpec with Matchers {

  "A Topic Filter" should {
    "only be able to match well formed filters" in {
      val topic = "a/b"

      Topic.topicMatches(topic, "") shouldBe a [Bad[_,String]]
      Topic.topicMatches(topic, "a+b") shouldBe a [Bad[_,String]]
      Topic.topicMatches(topic, "a/#/b") shouldBe a [Bad[_,String]]
      Topic.topicMatches(topic, "a#b") shouldBe a [Bad[_,String]]
      Topic.topicMatches(topic, "a/b/c#d/e") shouldBe a [Bad[_,String]]
      Topic.topicMatches(topic, "a#") shouldBe a [Bad[_,String]]
      Topic.topicMatches(topic, "a\u0000b") shouldBe a [Bad[_,String]]
      Topic.topicMatches(topic, "ab\u0000") shouldBe a [Bad[_,String]]
      Topic.topicMatches(topic, "\u0000c") shouldBe a [Bad[_,String]]

      Topic.topicMatches(topic, "a") shouldBe Good(false)
      Topic.topicMatches(topic, "a/b") shouldBe Good(true)
      Topic.topicMatches(topic, "a/+") shouldBe Good(true)
      Topic.topicMatches(topic, "a/+/") shouldBe Good(false)
      Topic.topicMatches(topic, "a/+/b") shouldBe Good(false)
      Topic.topicMatches(topic, "a/#") shouldBe Good(true)
      Topic.topicMatches(topic, "#") shouldBe Good(true)

      Topic.topicMatches("////", "//+//") shouldBe Good(true)
      Topic.topicMatches("////", "//+/") shouldBe Good(false)
      Topic.topicMatches("////", "//#") shouldBe Good(true)
      Topic.topicMatches("////", "#") shouldBe Good(true)
      Topic.topicMatches("/", "#") shouldBe Good(true)
      Topic.topicMatches("", "#") shouldBe a [Bad[_,String]]
    }

    "only be able to match well formed topics" in {
      val filter = "a/b"

      Topic.topicMatches("a\u0000/b", filter) shouldBe a [Bad[_,String]]
      Topic.topicMatches("\u0000", filter) shouldBe a [Bad[_,String]]
      Topic.topicMatches("a/+", filter) shouldBe a [Bad[_,String]]
      Topic.topicMatches("a/b/c#d/e", filter) shouldBe a [Bad[_,String]]
      Topic.topicMatches("a/#", filter) shouldBe a [Bad[_,String]]
    }
  }
}
