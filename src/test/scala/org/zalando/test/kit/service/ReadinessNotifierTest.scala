package org.zalando.test.kit.service

import org.mockserver.verify.VerificationTimes
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Milliseconds, Seconds, Span}
import org.scalatest.{FlatSpec, MustMatchers}
import org.zalando.test.kit.ScalatestServiceKit
import org.zalando.test.kit.service.ReadinessNotifier._

import scala.concurrent.duration._

class ReadinessNotifierTest extends FlatSpec with ScalaFutures with MustMatchers with ScalatestServiceKit {

  val sampleHealthCheck = new MockServerTestService("Sample resource", 8080)
  override def testServices = sampleHealthCheck

  behavior of "ReadinessNotifier.immediately"

  it must "return future" in {
    whenReady(immediately("foo").whenReady(), Timeout(Span(0, Seconds))) { foo =>
      foo mustBe "foo"
    }
  }

  it must "await ready" in {
    immediately("foo").awaitReady(Duration.Zero) mustBe "foo"
  }

  behavior of "ReadinessNotifier.duration"

  it must "return future" in {
    whenReady(duration(100.millis, "foo").whenReady(), Timeout(Span(100, Milliseconds)))(_ mustBe "foo")
  }

  it must "await ready" in {
    duration(100.millis, "foo").awaitReady(100.millis) mustBe "foo"
  }

  behavior of "ReadinessNotifier.healthCheck"

  it must "be ready if url responds with success (2XX)" in {
    sampleHealthCheck.expectResponseWithStatus(200, VerificationTimes.atLeast(1))
    healthCheck(sampleHealthCheck.apiUrl, interval = 100.millis, "bar").awaitReady(1.second) mustBe "bar"
  }

  it must "timeout if url responds with error (not 2XX)" in {
    intercept[RuntimeException] { // TODO-ylazaryev: test message
      sampleHealthCheck.expectResponseWithStatus(500, VerificationTimes.atLeast(1))
      healthCheck(sampleHealthCheck.apiUrl, interval = 200.millis).awaitReady(1.second)
    }
  }

}
