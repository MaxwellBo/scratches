def add(x: Int, y: Int) = x + y

// println(add(3, 5))

def addCurried(x: Int)(y: Int) = x + y

// println(addCurried(3)(5))

val add2: Function1[Int, Int]= addCurried(2)

// println(add2(3))

///////////////////////////////////////////////////////////////////////////////


def fNoArgs = println("fNoArgs got called")

// fNoArgs
// fNoArgs
// fNoArgs


///////////////////////////////////////////////////////////////////////////////

implicit val x: String = "up here"

def implicitString()(implicit x: String) = x

// println(implicitString()) // up here

///////////////////////////////////////////////////////////////////////////////

implicit val booleanInstance:   Boolean = true
implicit val intInstance:       Int     = 5

def primativeImplicitly[T](implicit x: T): T  = {
  x
}

// println(primativeImplicitly[Boolean]) // true
// println(primativeImplicitly[Int]) // 5

///////////////////////////////////////////////////////////////////////////////

implicit val listIntInstance:   List[Int]   = List(1)
implicit val optionInstance:    Option[Int] = Some(1)

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

implicit val listStringInstance:   List[String]   = List("hello")
implicit val listBoolInstance:     List[Boolean]  = List(true)

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

case class Json(innerString: String)

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
          .map { case (k, v) => s"${encodeString.encode(k).innerString}: ${v.innerString}" }
          .mkString(", ")

      val outer = s"{ ${inner} }"
      Json(outer)
    }
  }
}

////////////////////////////////////////////////////////////////////////////////

implicit class EncodeSyntax[A](private val self: A) extends AnyVal {
  def encode()(implicit instance: Encode[A]): Json = {
    instance.encode(self)
  }
}

////////////////////////////////////////////////////////////////////////////////
// YOUR CONCERNS

import EncodeInstances._
import EncodeSyntax._

case class Person(name: String, age: Int, alive: Boolean)

implicit def encodePerson: Encode[Person] = new Encode[Person] {
    override def encode(person: Person): Json = 
      // we can obviously do this in a macro
      Map(
        "name" -> person.name.encode(),
        "age" -> person.age.encode(),
        "alive" -> person.alive.encode(),
      ).encode()
}

val me = Person(name="Max Bo", age=22, alive=true)

println(me.encode().innerString)

def needsAnEncoder[A](a: A)(implicit instance: Encode[A]) {
  println(a.encode().innerString)
}

def needsAnEncoderPrime[A: Encode](a: A) {
  println(a.encode().innerString)
}

// needsAnEncoder(me)

case class HasNoEncoder()

// hkts.sc:150: could not find implicit value for evidence parameter of type ammonite.$file.hkts.Encode[ammonite.$file.hkts.HasNoEncoder]
// val res_28 = needsAnEncoder(HasNoEncoder())
// needsAnEncoder(HasNoEncoder())

////////////////////////////////////////////////////////////////////////////////

// what if we want the `.map` to work on not just lists, but anything?

case class Foo[T](x: T)

trait Functor[F[_]] {
  def fmap[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor {
  def fmap[F[_], A, B](fa: F[A], f: A => B)(implicit instance: Functor[F]): F[B] = {
    instance.fmap(fa)(f)
  }
}

object FunctorInstances {
    // @ List(1, 2, 3).map(x => x + 1)
  // res0: List[Int] = List(2, 3, 4)
  implicit val listInstance: Functor[List] = new Functor[List] {
    def fmap[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  // our custom function for our container (we can define Functor instances for anything we want!)
  implicit val fooInstance: Functor[Foo] = new Functor[Foo] {
    def fmap[A, B](fa: Foo[A])(f: A => B): Foo[B] = Foo(f(fa.x))
  }
}


import FunctorInstances._

println(Functor.fmap(List(1, 2, 3), (x: Int) => x + 1))
println(Functor.fmap(Foo(1),        (x: Int) => x + 1))

// but what if we want to do Foo(1).map((x: Int) => x + 1)?

object FunctorSyntax {
  implicit final class FunctorExtensions[F[_], A](private val self: F[A]) extends AnyVal {
    def fmap[B](f: A => B)(implicit instance: Functor[F]): F[B] = {
      instance.fmap(self)(f)
    }
  }
}

import FunctorSyntax._

// println(Foo(1).fmap((x: Int) => x + 1)) // 6

///////////////////////////////////////////////////////////////////////////////

trait RowA[F[_]] {
  def x: F[String]
  def y: F[Int]
}

// def id(f) = f

type InsertRowA = RowA[Id]
type PatchRowA = RowA[Option]

val insert = new InsertRowA {
  def x = "hello"
  def y = 5
}

val patch = new PatchRowA {
  def x = Some("hello")
  def y = None
}

///////////////////////////////////////////////////////////////////////////////

// trait RowIncompat[F[_]] {
//   def y: F[String]
//   def z: F[Boolean]
// }

trait RowB[F[_]] {
  def y: F[Int]
  def z: F[Boolean]
}

type Join = RowA[Id] with RowB[Id]
// type Join = RowA[Id] with RowIncompat[Id]

// can't construct with typealias
val join = new RowA[Id] with RowB[Id] {
// val join = new RowA[Id] with RowIncompat[Id] {
  def x = "hello"
  def y = 5
  def z = true
}

type Anon = { def x: String; def y: Int; def z: Boolean }

def acceptsAJoin(join: Anon) = {
  println(join.x)
  println(join.y)
  println(join.z)
}

// acceptsAJoin(join)

type HasX = { def x: Int }
type HasY = { def y: Int }
type HasZ = { def z: Int }

case class Coordinate(x: Int, y: Int)

val origin = Coordinate(0, 0)

def acceptsX(w: HasX with HasY) = {
  println(w)
}

// acceptsX(origin)
