// https://www.stackage.org/haddock/lts-8.13/ghc-prim-0.5.0.0/src/GHC-Types.html#IO
// https://stackoverflow.com/questions/3124591/is-haskell-really-a-purely-functional-language-considering-unsafeperformio/3124776
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
