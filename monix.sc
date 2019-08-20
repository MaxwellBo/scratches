import $ivy.`io.monix::monix:2.3.0`

import monix.execution.Scheduler.Implicits.global

import scala.concurrent.Await
import scala.concurrent.duration._

import monix.eval._

// A specification for evaluating a sum,
// nothing gets triggered at this point!
val task = Task { 1 + 1 }

val future = task.runToFuture

println(Await.result(future, 5.seconds))

