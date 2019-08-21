  // if you're gonna say something like "oh this is so much nicer in Haskell"
  // then shut the fuck up
  implicit def eitherFunctorInstance[E]: Functor[Either[E, ?]] = new Functor[Either[E, ?]] {
    def map[A, B](fa: Either[E, A])(f: A => B): Either[E, B] = fa match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }
  }

// sealed trait Either[+E, +A]
// final case class Right[+E, +A](value: A) extends Either[E, A]
// final case class Left[+E, +A](value: E) extends Either[E, A]

  object EitherOps {
  implicit final class EitherIdExtensions[A](val self: A) extends AnyVal {
    final def left[B]: Either[A, B] =
      Left(self)
  
    final def right[B]: Either[B, A] =
      Right(self)
  }
}

// def divideBy(x: Int, y: Int): Either[String, Int] = {
//   if (y == 0) { Left("divide by zero") } else { Right(x / y) }

implicit def eitherFunctorInstance[E]: Monad[Either[E, ?]] = new Monad[Either[E, ?]] {
    def pure[A](a: A): Either[E, A] = Right(a)

    def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = { 
      fa match {
        case Right(a) => f(a)
        case Left(e) => Left(e)
      }
    }

    // OR

    def flatten[A](ffa: Either[E, Either[E, A]]): Either[E, A] = ffa match {
      case Right(Right(a)) => Right(a)
      case Right(Left(e))  => Left(e)
      case Left(e)         => Left(e)
    }
  }