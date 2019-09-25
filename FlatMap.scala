import cats._
import cats.implicits._
import scala.languageFeature.higherKinds

def useFlatMap[F[_], A](x: F[A], y: F[A])(implicit fm: FlatMap[F]): F[(A, A)] = {
  for {
    a <- x
    b <- y
  } yield ((a, b))
}

val x = useFlatMap(Some(5): Option[Int], Some(6): Option[Int])
