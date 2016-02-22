package org.zalando.test.kit.service

import java.util.concurrent._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scalaj.http.{Http ⇒ Get}

sealed trait Ready
case object Ready extends Ready

abstract class ReadinessNotifier[R](val resultToReturn: R = Ready) {

  protected def timeOut(): Unit = {}
  protected def timeOutMessage(duration: Duration) = s"Resource is not ready within duration ($duration)"
  def whenReady(): Future[R]
  def awaitReady(atMost: Duration): R = {
    try {
      Await.result(whenReady(), atMost)
    } catch {
      case e: TimeoutException ⇒
        timeOut()
        throw new RuntimeException(timeOutMessage(atMost), e)
    }
  }
}

object ReadinessNotifier {

  def immediately[R](resultToReturn: R = Ready) = new ReadinessNotifier[R](resultToReturn) {
    override def whenReady(): Future[R] = Future.successful(resultToReturn)
  }

  def duration[R](duration: Duration, resultToReturn: R = Ready) = new ReadinessNotifier[R](resultToReturn) {
    lazy val executor = Executors.newSingleThreadScheduledExecutor()
    override def whenReady(): Future[R] = {
      val promise = Promise[R]()
      val executor = Executors.newSingleThreadScheduledExecutor()
      executor.schedule(new Runnable {
        override def run(): Unit = {
          promise.success(resultToReturn)
          executor.shutdown()
        }
      }, duration.toMillis, TimeUnit.MILLISECONDS)
      promise.future
    }
    override protected def timeOut(): Unit = executor.shutdown()
  }

  def healthCheck[R](url: String,
                     interval: FiniteDuration = 1.second,
                     resultToReturn: R = Ready,
                     httpMethod: String = "HEAD",
                     connectionTimeout: FiniteDuration = 1.second,
                     readTimeout: FiniteDuration = 1.second) = new ReadinessNotifier[R](resultToReturn) {
    val connectionTimeoutMs = connectionTimeout.toMillis.toInt
    val readTimeoutMs = readTimeout.toMillis.toInt
    lazy val executor = Executors.newSingleThreadScheduledExecutor()

    override protected def timeOut(): Unit = executor.shutdown()

    override protected def timeOutMessage(duration: Duration) =
      s"Resource ($url) is not ready within duration ($duration)"

    override def whenReady(): Future[R] = {
      val promise = Promise[R]()
      executor.scheduleAtFixedRate(new Runnable {
        override def run(): Unit = {
          if (Get(url).timeout(connectionTimeoutMs, readTimeoutMs).method(httpMethod).asString.isNotError) {
            promise.success(resultToReturn)
            executor.shutdown()
          }
        }
      }, 0, interval.toMillis, TimeUnit.MILLISECONDS)
      promise.future
    }
  }


}