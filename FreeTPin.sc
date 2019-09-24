interp.configureCompiler(_.settings.YpartialUnification.value = true)
import $ivy.`org.typelevel::cats-free:2.0.0-M1`
import $ivy.`org.typelevel::cats-core:2.0.0-M1`
import $ivy.`org.typelevel::cats-effect:2.0.0-M1`

import cats._
import cats.free._
import cats.data._
import cats.implicits._
import cats.effect.IO

/* A base ADT for the user interaction without state semantics */
sealed abstract class Teletype[A] extends Product with Serializable
// defined class Teletype

final case class WriteLine(line : String) extends Teletype[Unit]
// defined class WriteLine

final case class ReadLine(prompt : String) extends Teletype[String]
// defined class ReadLine

type TeletypeT[M[_], A] = FreeT[Teletype, M, A]
// defined type alias TeletypeT

type Log = List[String]
// defined type alias Log

type TeletypeState[A] = State[List[String], A]
// defined type alias TeletypeState

type Effect[A] = IO[A]

/** Teletype smart constructors */
object TeletypeOps {
  def writeLine(line : String) : TeletypeT[Effect, Unit] =
		FreeT.liftF[Teletype, Effect, Unit](WriteLine(line))

  def readLine(prompt : String) : TeletypeT[Effect, String] =
		FreeT.liftF[Teletype, Effect, String](ReadLine(prompt))
}
// defined object TeletypeOps

def program : TeletypeT[Effect, Unit] = {
  for {
	userSaid <- TeletypeOps.readLine("what's up?!")
	_ <- FreeT.liftT[Teletype, Effect, Unit](IO { println(s"user said : $userSaid") })
	_ <- TeletypeOps.writeLine("thanks, see you soon!")
  } yield ()
}
// program: TeletypeT[TeletypeState,Unit]

def interpreter = new (Teletype ~> Effect) {
  def apply[A](fa : Teletype[A]) : Effect[A] = {
	fa match {
	  case ReadLine(prompt) =>
			IO { readLine }
	  case WriteLine(line) =>
			IO { println(line) }
	}
  }
}
// interpreter: Teletype ~> TeletypeState

import TeletypeOps._
// import TeletypeOps._

val action = program.foldMap(interpreter)
action.unsafeRunSync()