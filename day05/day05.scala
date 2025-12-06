import scala.collection.immutable.TreeSet

type RangeId  = (Long, Long)
type Interval = List[RangeId]

extension (r1: RangeId) infix def *+(r2: RangeId): RangeId = (Math.min(r1(0), r2(0)), Math.max(r1(1), r2(1)))

extension (s1: TreeSet[RangeId])
  infix def *+(r: RangeId): TreeSet[RangeId] =
    if s1.isEmpty then s1 + r
    else if (s1.last(1) in r) || (r(1) in s1.last) then s1.dropRight(1) + (s1.last *+ r)
    else s1 + r

extension (l: Long)
  infix def in(r1: RangeId): Boolean = l >= r1(0) && l <= r1(1)
  infix def in(i: Interval): Boolean = i match
    case Nil                         => false
    case head :: tail if l < head(0) => false
    case head :: tail if l > head(1) => l in tail
    case _ :: _                      => true

def parse(input: String): (List[RangeId], List[Long]) =
  val (rawRangeIds, rawIngredientIds) = input.splitAt(input.indexOf("\n\n"))
  val rangeIds = rawRangeIds.linesIterator.toList.map(_.split("-").map(_.toLong)).map(pair => (pair(0), pair(1))).sorted
  val ingredientIds = rawIngredientIds.linesIterator.toList.filterNot(_.isBlank).map(_.toLong)
  (rangeIds, ingredientIds)

def part1(input: String): Int =
  val (rangeIds, ingredientIds) = parse(input)
  ingredientIds.filter(_ in rangeIds).length

def part2(input: String): Long =
  val (rangeIds, _) = parse(input)
  rangeIds
    .foldLeft(TreeSet.empty[RangeId])((interval, rangeId) => interval *+ rangeId)
    .toList
    .map(rangeId => (rangeId(1) - rangeId(0)) + 1)
    .sum

@main def Day05(input: String): Unit =
  val res1=part1(input)
  val res2 = part2(input)

  println(s"Part1 = $res1")
  println(s"Part2 = $res2")
