///////////////////////////////////////////////////////////////////////////////

trait RowA[F[_]] {
  def x: F[String]
  def y: F[Int]
}

// def id(f) = f

type InsertRowA = RowA[Id]
type PatchRowA = RowA[Option]

val insert = new InsertRowA {
  def x = "hello"
  def y = 5
}

val patch = new PatchRowA {
  def x = Some("hello")
  def y = None
}

///////////////////////////////////////////////////////////////////////////////

// trait RowIncompat[F[_]] {
//   def y: F[String]
//   def z: F[Boolean]
// }

trait RowB[F[_]] {
  def y: F[Int]
  def z: F[Boolean]
}

type Join = RowA[Id] with RowB[Id]
// type Join = RowA[Id] with RowIncompat[Id]

// can't construct with typealias
val join = new RowA[Id] with RowB[Id] {
// val join = new RowA[Id] with RowIncompat[Id] {
  def x = "hello"
  def y = 5
  def z = true
}

type Anon = { def x: String; def y: Int; def z: Boolean }

def acceptsAJoin(join: Anon) = {
  println(join.x)
  println(join.y)
  println(join.z)
}

// acceptsAJoin(join)

type HasX = { def x: Int }
type HasY = { def y: Int }
type HasZ = { def z: Int }

case class Coordinate(x: Int, y: Int)

val origin = Coordinate(0, 0)

def acceptsX(w: HasX with HasY) = {
  println(w)
}

// acceptsX(origin)
