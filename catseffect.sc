import $ivy.`org.typelevel::cats-effect:1.3.1`
import cats._, cats.data._, cats.syntax.all._
import cats.effect.IO

import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.duration._

// https://typelevel.org/cats-effect/datatypes/io.html

implicit val timer = IO.timer(ExecutionContext.global)

val ioa = IO.sleep(5.seconds) *> IO { println("hey!") }

val program: IO[Unit] =
  for {
     _ <- ioa
     _ <- ioa
  } yield ()

program.unsafeRunSync()
//=> hey!
//=> hey!
()
