import scala.collection.immutable.NumericRange.Inclusive
type Id = (first: Long, last: Long)

extension (i: Id) def range: Inclusive[Long] = i.first to i.last
extension (i: Long)
  def splitBy(n: Int): List[List[Long]]      =
    def loop(digitLength: Int, digits: List[Long]): List[List[Long]] =
      if digits.isEmpty then Nil
      else List(digits.take(digitLength)) ::: loop(digitLength, digits.drop(digitLength))

    val digits = i.toString.map(_.toString.toLong).toList
    loop(digits.length / n, digits)

def parse(input: String): Array[Id] =
  input.split(",").map(l => l.split("-")).map(l => (l(0).toLong, l(1).toLong))

def part1(input: String): Long =
  def isInvalid(id: Long): Boolean =
    val (l, r) = id.toString.splitAt(id.toString.length / 2)
    l == r

  parse(input)
    .flatMap(id => id.range.toArray.filter(digit => digit.toString.length % 2 == 0))
    .filter(isInvalid)
    .sum

def part2(input: String) =
  def isInvalid(id: Long): Boolean =
    def loop(digitLength: Int, index: Int): Boolean =
      if index == 1 then false
      else if digitLength % index != 0 then loop(digitLength, index - 1)
      else if id.splitBy(index).distinct.length == 1 then true
      else loop(digitLength, index - 1)

    val digitLength = id.toString.length
    loop(digitLength, digitLength)

  parse(input)
    .flatMap(id => id.range)
    .filter(isInvalid)
    .sum

// main
@main def main(input: String): Unit =
  val res1 = part1(input)
  val res2 = part2(input)
  println(res1)
  println(res2)
