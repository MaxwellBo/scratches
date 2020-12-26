val xs = List(5, "string", true)

val ys = xs.collect {
  case x: Int => x
}

println(ys)

trait Animal
case class Cat() extends Animal
case class Dog() extends Animal

val xss: List[Option[Animal]] =
  List(Some(Cat()), Some(Dog()))

val yss = xss.collect {
  case x: Option[Cat] => x
}

println(yss)

import reflect.runtime.universe._

def z[T: TypeTag](zss: List[T]) = zss.collect {
  case x if typeOf[T] <:< typeOf[Option[Cat]] => x
}


println(z(xss))
