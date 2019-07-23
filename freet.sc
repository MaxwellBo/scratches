interp.configureCompiler(_.settings.YpartialUnification.value = true)
import $ivy.`org.typelevel::cats-free:2.0.0-M1`
import $ivy.`org.typelevel::cats-core:2.0.0-M1`

import cats._
import cats.free._
import cats.data._
import cats.implicits._

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

/** Teletype smart constructors */
object TeletypeOps {
  def writeLine(line : String) : TeletypeT[TeletypeState, Unit] =
		FreeT.liftF[Teletype, TeletypeState, Unit](WriteLine(line))
  def readLine(prompt : String) : TeletypeT[TeletypeState, String] =
		FreeT.liftF[Teletype, TeletypeState, String](ReadLine(prompt))
  def log(s : String) : TeletypeT[TeletypeState, Unit] =
		FreeT.liftT[Teletype, TeletypeState, Unit](State.modify(s :: _))
}
// defined object TeletypeOps

def program : TeletypeT[TeletypeState, Unit] = {
  for {
	userSaid <- TeletypeOps.readLine("what's up?!")
	_ <- TeletypeOps.log(s"user said : $userSaid")
	_ <- 
	_ <- TeletypeOps.writeLine("thanks, see you soon!")
  } yield ()
}
// program: TeletypeT[TeletypeState,Unit]

def interpreter = new (Teletype ~> TeletypeState) {
  def apply[A](fa : Teletype[A]) : TeletypeState[A] = {
	fa match {
	  case ReadLine(prompt) =>
			println(prompt)
		val userInput = "hanging in here" //scala.io.StdIn.readLine()
			StateT.pure[Eval, List[String], A](userInput)
	  case WriteLine(line) =>
			StateT.pure[Eval, List[String], A](println(line))
	}
  }
}
// interpreter: Teletype ~> TeletypeState

import TeletypeOps._
// import TeletypeOps._

val state = program.foldMap(interpreter)
// state: TeletypeState[Unit] = cats.data.IndexedStateT@84850db

val initialState = Nil
// initialState: scala.collection.immutable.Nil.type = List()

val (stored, _) = state.run(initialState).value
// what's up?!
// thanks, see you soon!
// stored: List[String] = List(user said : hanging in here)