import $plugin.$ivy.`org.spire-math::kind-projector:0.9.3`

def add(x: Int, y: Int) = x + y

// println(add(3, 5))

def addCurried(x: Int)(y: Int) = x + y

// println(addCurried(3)(5))

val add2: Function1[Int, Int] = addCurried(2)

// println(add2(3))

///////////////////////////////////////////////////////////////////////////////


def emptyParamterList() = println("I'm a function that has an empty paramater list")

// emptyParamterList()
// emptyParamterList()

def noParameterList = println("I'm a function that has no paramater list")

// noParameterList
// noParameterList


///////////////////////////////////////////////////////////////////////////////

implicit val implicitString: String = "implicit String"

def implicitString()(implicit x: String) = x

// println(implicitString()) // implicit String

///////////////////////////////////////////////////////////////////////////////

implicit val implicitBoolean:   Boolean = true
implicit val implicitInt:       Int     = 5

def rawImplicitly[T](implicit x: T): T  = {
  x
}

// println(rawImplicitly[Boolean]) // true
// println(rawImplicitly[Int]) // 5

///////////////////////////////////////////////////////////////////////////////

implicit val implicitListInt:       List[Int]   = List(1)
implicit val implicitOptionInt:     Option[Int] = Some(1)

def hktImplicitly[F[_]](implicit x: F[Int]): F[Int]  = {
  x
}

// scala> :k List
// List's kind is F[+A]

// println(hktImplicitly[List])
// println(hktImplicitly[Option])

///////////////////////////////////////////////////////////////////////////////

type Id[A] = A
// * -> *

implicit val implicitListString:   List[String]   = List("hello")
implicit val implicitListBoolean:     List[Boolean]  = List(true)

def hktAppImplicitly[F[_], A](implicit x: F[A]): F[A]  = {
  x
}

// println(hktAppImplicitly[List, String])
// println(hktAppImplicitly[List, Boolean])
// println(hktAppImplicitly[Id, String])

///////////////////////////////////////////////////////////////////////////////

// generally good practise to wrap implicit instances in an object so you can conditionally import their extension methods
object IntSyntax {
  implicit final class IntExtensions(private val self: Int) extends AnyVal {
    def increment(): Int = self + 1
  }
}
// https://kotlinlang.org/docs/reference/extensions.html

import IntSyntax._

// println(5.increment()) // 6

///////////////////////////////////////////////////////////////////////////////
// ENCODE NORTHSTAR

// println(5.encode().value) // 5
// println("hello".encode().value) // "hello"

val me = Person(name="Max Bo", age=22, alive=true)

// println(me.encode().value) // { "name": "Max Bo", "age": 22, "alive": true }

///////////////////////////////////////////////////////////////////////////////
// ENCODER LIBRARY IMPLEMENTATION

case class Json(value: String)

trait Encode[A] {
  def encode(x: A): Json
}

object EncodeInstances {
  implicit val encodeString: Encode[String] = new Encode[String] {
    override def encode(x: String) = Json("\"" + x.toString() + "\"")
  }

  implicit val encodeInt: Encode[Int] = new Encode[Int] {
    override def encode(x: Int) = Json(x.toString())
  }

  implicit val encodeBoolean: Encode[Boolean] = new Encode[Boolean] {
    override def encode(x: Boolean) = Json(x.toString())
  }

  implicit def encodeMap[A, B]: Encode[Map[String, Json]] = new Encode[Map[String, Json]] {
    override def encode(kv: Map[String, Json]) = {
      val inner = 
        kv
          .toList
          .map { case (k, v) => s"${encodeString.encode(k).value}: ${v.value}" }
          .mkString(", ")

      val outer = s"{ ${inner} }"
      Json(outer)
    }
  }
}

object EncodeSyntax {
  implicit class EncodeIdExtensions[A](private val self: A) extends AnyVal {
    def encode()(implicit instance: Encode[A]): Json = {
      instance.encode(self)
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
// ENCODER USAGE

import EncodeInstances._
import EncodeSyntax._

case class Person(name: String, age: Int, alive: Boolean)

// at this point in type, we break object orientation.
// We've defined the Encode[Person] _seperate_ (!) from Person.
// We've seperated behaviour and data.
//
// The practical implication of this is that Person could be a 3rd party
// datatype, and we could still define an Encode instance for it

implicit def encodePerson: Encode[Person] = new Encode[Person] {
    override def encode(person: Person): Json = 
      // we can obviously do this in a macro
      Map(
        "name" -> person.name.encode(),
        "age" -> person.age.encode(),
        "alive" -> person.alive.encode(),
      ).encode()
}

// println(me.encode().value) // { "name": "Max Bo", "age": 22, "alive": true }
// this now works!

// obviously these do as well
// println(5.encode().value)
// println("hello".encode().value) 

////////////////////////////////////////////////////////////////////////////////

def needsAnEncoder[A](a: A)(implicit instance: Encode[A]) {
  println(a.encode().value)
}

// sugars to

def needsAnEncoderPrime[A: Encode](a: A) {
  // val instance = implicitly[Encode[A]] // we can still recover the instance
  println(a.encode().value)
}

// needsAnEncoder(me)

case class HasNoEncoder()

// hkts.sc:150: could not find implicit value for evidence parameter of type ammonite.$file.hkts.Encode[ammonite.$file.hkts.HasNoEncoder]
// val res_28 = needsAnEncoder(HasNoEncoder())
// needsAnEncoder(HasNoEncoder())

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// # FUNCTORS #

// sealed trait Option[+A] 
// case class Some[+A](value: A) extends Option[A]
// case object None extends Option[Nothing]

case class FamilyMember(age: Int, parent: Option[FamilyMember])

val grandad = FamilyMember(age=79, parent=None)
val mum = FamilyMember(age=55, parent=Some(grandad))
val son = FamilyMember(age=22, parent=Some(mum))

val family: List[FamilyMember] = List(son, mum, grandad)

def getAges(family: List[FamilyMember]): List[Int] = {
  family.map(_.age)
}

def getParentAge(member: FamilyMember): Option[Int] = {
  val parent: Option[FamilyMember] = member.parent
  parent.map(_.age)
}

// println(getParentAge(son)) // Some(55)
// println(getAges(family)) // List(22, 55, 79)

// How do we make something like this? 🤔
// def getAge[F[_]: ???](f: F[FamilyMember]): F[Int] = {
//   f.map(_.age)
// }

////////////////////////////////////////////////////////////////////////////////

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object FunctorInstances {
    // @ List(1, 2, 3).map(x => x + 1)
  // res0: List[Int] = List(2, 3, 4)
  implicit val listFunctorInstance: Functor[List] = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit val optionFunctorInstance: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
      case Some(a) => Some(f(a))
      case n@None => n
    }
  }
}

object FunctorSyntax {
  implicit final class FunctorExtensions[F[_], A](private val self: F[A]) extends AnyVal {
    def map[B](f: A => B)(implicit instance: Functor[F]): F[B] = {
      instance.map(self)(f)
    }
  }
}

import FunctorInstances._
import FunctorSyntax._

def getAge[F[_]: Functor](f: F[FamilyMember]): F[Int] = {
  f.map(_.age)
}

// val family: List[FamilyMember] = List(son, mum, grandad)
val optionParent: Option[FamilyMember] = son.parent

// println(getAge(family)) // List(22, 55, 79)
// println(getAge(optionParent)) // Some(55)

////////////////////////////////////////////////////////////////////////////////
// # FUNCTOR EXAMPLE 2 #

// Functors are not good at dealing with chained operations.
// this type would get bigger and bigger as we traversed up the family tree
// if only there was some way to flatten these down 🤔 
def getGrandparentAge(member: FamilyMember): Option[Option[Int]] = {
  member.parent.map(_.parent.map(_.age))
  //           ^             ^ multiple successive Functor operations
  //                           causes nesting
}


def poll(backoff: Int): IO[Int] = {
  // wait for backoff seconds before polling again
  // poll some endpoint
  // get the new endpoint provided backoff value
  IO.effect(backoff + 1)
}

// if only there was some way to flatten these down 🤔 
// def naivePoll3Times(): IO[IO[IO[Int]]] = {
//   poll(backoff=1).map(poll).map(poll)
// }

///////////////////////////////////////////////////////////////////////////////

trait Monad[F[_]] {
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  
  def flatten[A](ffa: F[F[A]]): F[A]
  // ^ this is a bit of an oddball method. It's often not talked about in
  // literature, but you CAN make a Monad with just a `flatten` function (+ `pure`),
  // and skip the `flatMap` (arguably, the more complex) function entirely
  // This is because you can derive `flatMap` with `flatten, and vice-versa.
  
  // These default implementations are as follows:

  // def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = fa.map(f).flatten
  // def flatten[A](ffa: F[F[A]]): F[A] = ffa.flatMap(x => x)

  // We're not going to define these to force us to implement Monad in both ways.
}

object MonadInstances {
  implicit val listMonadInstance: Monad[List] = new Monad[List] {
    def pure[A](a: A): List[A] = List(a)
    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    def flatten[A](ffa: List[List[A]]): List[A] = ffa.flatten
  }

  implicit val optionMonadInstance: Monad[Option] = new Monad[Option] {
    def pure[A](a: A): Option[A] = Some(a)

    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
      case Some(a) => f(a)
      case _ => None
    }

    // OR

    def flatten[A](ffa: Option[Option[A]]): Option[A] = ffa match {
      case Some(Some(a)) => Some(a)
      case _ => None
    }
  }
}

object MonadSyntax {
  implicit class MonadIdExtensions[A](private val self: A) extends AnyVal {
    def pure[F[_]]()(implicit instance: Monad[F]): F[A] = {
      instance.pure(self)
    }
  }

  implicit final class MonadExtensions[F[_], A](private val self: F[A]) extends AnyVal {
    def flatMap[B](f: A => F[B])(implicit instance: Monad[F]): F[B] = {
      instance.flatMap(self)(f)
    }
  }
}

import FunctorInstances._
import FunctorSyntax._
import MonadInstances._
import MonadSyntax._

val manualListComprehension = 
  List(0, 1, 2).flatMap(n => 
    List(n * 10, n * 100).flatMap(big => 
      List((n, big))
    )
  )

// println(manualListComprehension)

// Effectively identitical to list comprehensions in Python
// ```
// list_comprehension = [ (n, big) 
//   for n in [0, 1, 2] 
//   for big in [n * 10, n * 100 ] 
// ]
// ```
//

// using `for` comprehension, we get a nicely sugared form

val listComprehension: List[(Int, Int)] = for {
  n    <- List(0, 1, 2)
  big  <- List(n * 10, n * 100)
} yield (n, big)

// println(listComprehension)


val bailsOutOnNone: Option[(Int, Int)]= for {
  // x <- Some(5)
  x <- (None: Option[Int])
  y <- Some(x + 6)
} yield (x, y)

// println(bailsOutOnNone)

///////////////////////////////////////////////////////////////////////////////

class IO[+A](val unsafeInterpret: () => A) 

object IO {
  def effect[A](eff: => A) = new IO(() => eff)
}

object MoreFunctorInstances {
  implicit val ioFunctorInstance: Functor[IO] = new Functor[IO] {
    def map[A, B](fa: IO[A])(f: A => B): IO[B] = IO.effect(f(fa.unsafeInterpret()))
  }
}

object MoreMonadInstances {
  implicit val ioMonadInstance: Monad[IO] = new Monad[IO] {
    def pure[A](a: A): IO[A] = IO.effect(a)

    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
      IO.effect(f(fa.unsafeInterpret()).unsafeInterpret())

    // OR

    def flatten[A](ffa: IO[IO[A]]): IO[A] =
      IO.effect(ffa.unsafeInterpret().unsafeInterpret())
  }
}

import FunctorSyntax._
import MonadSyntax._
import MoreFunctorInstances._
import MoreMonadInstances._

def putStrLn(line: String): IO[Unit] = 
  IO.effect(println(line))

val getStrLn: IO[String] = 
  IO.effect(scala.io.StdIn.readLine())

val echo: IO[Unit] = for {
  _        <- putStrLn("Please enter something to be echoed:")
  str      <- getStrLn
  _        <- putStrLn("Echoing: " + str)
} yield ()

// echo.unsafeInterpret()

// is roughyl equivalent to

// putStrLn("Please enter something to be echoed:").flatMap(_ => 
//   getStrLn.flatMap(str => 
//     putStrLn("Echoing: " + str)
//   )
// )

def workingPoll3Times(): IO[Int] = {
  poll(backoff=1)
    .flatMap(poll)
    .flatMap(poll)
}

def workingPoll3TimesWithForNation(): IO[Int] = for {
  newBackoff <- poll(backoff=1)
  newBackoff2 <- poll(newBackoff)
  finalBackoff <- poll(newBackoff2)
} yield finalBackoff

// println(workingPoll3Times().unsafeInterpret())
// println(workingPoll3TimesWithForNation().unsafeInterpret())


///////////////////////////////////////////////////////////////////////////////

val showInt: Function1[Int, String] = _.toString

val unshowInt: Function1[String, Int] = _.toInt

object EvenMoreFunctorInstances {
  implicit def function1FunctorInstance[R]: Functor[Function1[R, ?]] = new Functor[Function1[R, ?]] {
    def map[A, B](fa: Function1[R, A])(f: A => B): Function1[R, B] = fa.andThen(f)
  }
}

import FunctorSyntax._
import EvenMoreFunctorInstances._

// val roundtrip: Function1[Int, Int] = showInt.map(unshowInt)

// println(roundtrip(10))

object EvenMoreMonadInstances {
  implicit def function1MonadInstance[R]: Monad[Function1[R, ?]] = new Monad[Function1[R, ?]] {

    def pure[A](a: A): Function1[R, A] = (r: R) => a

    def flatMap[A, B](fa: Function1[R, A])(f: A => Function1[R, B]): Function1[R, B] = { (r: R) =>
      val a: A = fa(r)
      val fb: Function1[R, B] = f(a)
      val b: B = fb(r)
      b
    }

    // OR

    def flatten[A](ffa: Function1[R, Function1[R, A]]): Function1[R, A] = { (r: R) => 
      val fa: Function1[R, A] = ffa(r)
      val a: A = fa(r)
      a
    }
  }
}

// val addDigits: Function1[Int, (Int, Int, Int)] = for {
//   addOne <- (_ + 1)
//   addTwo <- (_ + 2)
//   addThree <- (_ + 3)
// } yield (addOne, addTwo, addThree)

// println(addDigits(0)) // (1, 2, 3)


///////////////////////////////////////////////////////////////////////////////
// OUR NORTHSTAR

case class HttpRequest(method: String, body: Option[String], parameters: Map[String, String])
case class HttpResponse(body: String)
case class JwtToken()

// def POST(request: HttpRequest): Option[Unit]
// def extractBody(request: HttpRequest): Option[String]
// def extractHeader(name: String)(request: HttpRequest): Option[String]

// def parseJWT(token: String): Option[JwtToken]

// def echoController: HttpRequest => Option[HttpResponse] = for {
//   _ <- POST
//   body <- extractBody
//   header <- extractHeader("Authorisation").map(_.flatMap(parseJWT))
// } yield HttpResponse(body)


// println(handler())

final case class Kleisli[F[_]: Monad, A, B](run: A => F[B]) {
  def compose[Z](k: Kleisli[F, Z, A]): Kleisli[F, Z, B] =
    Kleisli[F, Z, B](z => k.run(z).flatMap(run))
} 