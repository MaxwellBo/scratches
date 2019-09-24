trait Monoid[A] {
  def mempty: A
  def mconcat(x: A, y: A): A
}

trait Functor[F[_]] {
  def fmap[A, B](fa: F[A], f: A => B): F[B]
}

implicit def listFunctor[A](listA: List[A]): Functor[List] = new Functor[List] {
  override def fmap[A, B](fa: List[A], f: A => B): List[B] = fa.map(f)
}

class FunctorOps[F[_], A](fa: F[A])(implicit fFunctor: Functor[F]) {
  def lolmap[B](f: A => B): F[B] = fFunctor.fmap(fa, f)
}

implicit def functorToOps[F[_]: Functor, A](fa: F[A]) = new FunctorOps(fa)

implicitly[Functor[List]]

List(1,2,3).lolmap(x => x + 1)

implicit val monoidInt = new Monoid[Int] {
  override def mempty: Int = 0

  override def mconcat(x: Int, y: Int): Int = x + y
}

def fold[A](xs: List[A])(implicit m: Monoid[A]): A = {
  xs match {
    case List() => m.mempty
    case x :: xs => m.mconcat(x, fold(xs))
  }
}

fold(List(1,2,3))