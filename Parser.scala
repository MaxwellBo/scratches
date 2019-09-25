import $file.Day0
import $ivy.`com.lihaoyi::fastparse:1.0.0`
import fastparse.all._

sealed trait Thing
case class Group(contents: Seq[Thing]) extends Thing
case class Garbage(contents: String) extends Thing

val things: P[Thing] = P( group | garbage )
val garbage: P[Thing] = P( "<" ~ P( negate | chomp ).rep ~ ">").map(x => Garbage(x.mkString("")))
val negate: P[String] = P( "!" ~ AnyChar ).map(_ => "") 
val chomp: P[String] = P( CharsWhile(_ != '>').! )
val group: P[Thing] = P( "{" ~ things.rep(sep=",") ~ "}").map(Group(_))

def score(level: Int)(thing: Thing): Int = thing match {
  case Garbage(_) => 0
  case Group(contents) => level + contents.map(score(level + 1)).sum
}

// Day0.getDayInput(9)
val Parsed.Success(thing, _) = things.parse("{{<a!>},{<a!>},{<a!>},{<ab>}}")
assert(score(1)(thing) == (1 + 2))
