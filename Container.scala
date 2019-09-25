import cats._
import cats.data._
import cats.implicits._

import scala.annotation.tailrec
import scala.languageFeature.higherKinds

sealed trait Container[+A]
final case class Full[+A](x: A) extends Container[A]
final case class Empty() extends Container[Nothing]

implicit val instance = new FlatMap[Container] {
  override def flatMap[A, B](fa: Container[A])(f: A => Container[B]): Container[B] = fa match {
    case Full(x) => f(x)
    case Empty() => Empty()
  }

  override def map[A, B](fa: Container[A])(f: A => B): Container[B] = fa match {
    case Full(x) => Full(f(x))
    case Empty() => Empty()
  }

  @tailrec
  def tailRecM[A, B](a: A)(f: A => Container[Either[A, B]]): Container[B] =
    f(a) match {
      case Empty() => Empty()
      case Full(Left(a1)) => tailRecM(a1)(f)
      case Full(Right(b)) => Full(b)
    }
}

def useFlatMap[F[_], A](x: F[A], y: F[A])(implicit fm: FlatMap[F]): F[(A, A)] = {
  for {
    a <- x
    b <- y
  } yield ((a, b))
}

val x = useFlatMap(Full(5): Container[Int], Full(6): Container[Int])
