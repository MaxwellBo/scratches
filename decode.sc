import $ivy.`io.circe::circe-core:0.10.0`
import $ivy.`io.circe::circe-generic:0.10.0`
import $ivy.`io.circe::circe-parser:0.10.0`

import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.parser.{decode => pdecode} // don't ask

case class Foo(a: Int, b: String, c: Boolean)
case class Bar(d: Int)

val json = """{
  "a": 1,
  "b": "hello",
  "c": true
}
"""

implicit val fooDecoder: Decoder[Foo] = deriveDecoder[Foo]

val foo = pdecode[Foo](json)
println(foo) // Right(Foo(1,hello,true))

implicit val barDecoder: Decoder[Bar] = fooDecoder.map(foo => Bar(foo.a)) // our derived decoder

val bar = pdecode[Bar](json)
println(bar) // Right(Bar(1))
