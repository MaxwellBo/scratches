sealed trait Something
case class Subsomething(x: String) extends Something
case class Othersomething(x: String, y: String) extends Something

val x: Something = Subsomething("words")

val result = x match {
  case Subsomething(z) => z
  case Othersomething(z, y) => y
}

List(1, 2, 3).map(_ + 1)

implicit val aString = "somestring"
implicit val aNumber = 1

def f[A](implicit x: A): A = {
  x
}

f[String]
