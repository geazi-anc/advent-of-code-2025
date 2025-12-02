enum Rotation:
  case Left(clicks: Int)
  case Right(clicks: Int)

case class Dial(var current: Int = 50):
  var password = 0
  import Rotation.*

  def part1(r: Rotation): Unit =
    def loop(position: Int, limit: Int, index: Int = 0): Int =
      if limit == index then position
      else
        r match
          case Left(clicks)  => loop(if position == 0 then 99 else position - 1, limit, index + 1)
          case Right(clicks) => loop(if position == 99 then 0 else position + 1, limit, index + 1)

    current = r match
      case Right(clicks) => loop(current, clicks)
      case Left(clicks)  => loop(current, clicks)

    if current == 0 then password += 1
  end part1

  def part2(r: Rotation): Unit =
    def loop(position: Int, onZero: Int, limit: Int, index: Int = 0): (Int, Int) =
      if limit == index then (onZero, position)
      else
        r match
          case Left(clicks)  =>
            loop(
              if position == 0 then 99 else position - 1,
              if position - 1 == 0 then onZero + 1 else onZero,
              limit,
              index + 1,
            )
          case Right(clicks) =>
            loop(
              if position == 99 then 0 else position + 1,
              if position + 1 == 100 then onZero + 1 else onZero,
              limit,
              index + 1,
            )

    val (onZero, point) = r match
      case Left(clicks)  => loop(current, 0, clicks)
      case Right(clicks) => loop(current, 0, clicks)

    current = point
    password += onZero + (if current == 0 && onZero == 0 then 1 else 0)
  end part2

def parse(input: String): List[Rotation] = input.linesIterator.toList.map { i =>
  import Rotation.*

  val rotation = i.head
  val clicks   = i.tail.toInt

  if rotation == 'R' then Right(clicks) else Left(clicks)
}

// main
val input = """
...
""".strip

val dial      = Dial()
val rotations = parse(input)

rotations.foreach(rotation => dial.part2(rotation))

println(dial.password)
