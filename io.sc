class IO[+A](val unsafeInterpret: () => A) { s =>

  def map[B](f: A => B) = flatMap(f.andThen(IO.effect(_)))

  def flatMap[B](f: A => IO[B]): IO[B] =
    IO.effect(f(s.unsafeInterpret()).unsafeInterpret())
}

object IO {
  def effect[A](eff: => A) = new IO(() => eff)
}

def putStrLn(line: String): IO[Unit] = 
  IO.effect(println(line))

val getStrLn: IO[String] = 
  IO.effect(scala.io.StdIn.readLine())
