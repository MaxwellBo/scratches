class Add(x: Int) {
  def apply(y: Int) = x + y
}

val add2 = new Add(2)
val result = add2(4) // equivalent to add2.apply(4)

print(result) // 6


val xs = List(1, 2, 3)

print(xs(0)) // 1

