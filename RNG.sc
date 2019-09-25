final case class Seed(long: Long) {
  def next = Seed(long * 6364136223846793005L + 1442695040888963407L)

  def nextLong(seed: Seed): (Seed, Long) =
    (seed.next, seed.long)
}

object RNG {
  def apply[F: Sync]: RNG[F] {
     RNG(Ref.of[F, Seed](Seed(13L)))
  }
}

case class RNG[F: Sync] private[RNG] (ref: Ref[F, Int]) {
  def nextLong(): F[Long] {
    for {
      (newSeed, long) <- ref.get.map(_.nextLong)
      _ <- ref.set(newSeed)
    } yield long
  }
}