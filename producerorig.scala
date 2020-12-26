

case class Expr[A](eval: A)
case class Producer[A](produce: A)

trait StreamShape[A]
case class Linear[A](producer: Producer[A]) extends StreamShape[A]
case class Nested[B, A](
  producer: Producer[A], 
  nestedf: A => StreamShape[B]
) extends StreamShape[B]


def mapRaw[A, B](
  f: (A => (B => Expr[Unit]) => Expr[Unit]),
  stream: StreamShape[A]
): StreamShape[B] = {
    stream match {
        case Linear(producer) => {
            val prod: Producer[B] = ???
            Linear(prod)
        }
        case nested: Nested[A, bt] => {
            Nested(nested.producer, (a: bt) => mapRaw[A, B](f, nested.nestedf(a)))
        }
    }
}
