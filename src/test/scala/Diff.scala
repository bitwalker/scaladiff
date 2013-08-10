import org.scalatest.{FunSuite, PrivateMethodTester}
import org.scalatest.matchers.ShouldMatchers._

import net.ironforged.scaladiff._
import net.ironforged.scaladiff.OperationType._

class DiffSuite extends FunSuite with PrivateMethodTester {
  val original = "bills boards"
  val modified = "bills swords"
  val longestCommon = "bills ords"
  val prefix   = 6
  val suffix   = 3
  val ops = List(
    Operation(Equals, "b"),
    Operation(Equals, "i"),
    Operation(Equals, "l"),
    Operation(Equals, "l"),
    Operation(Equals, "s"),
    Operation(Equals, " "),
    Operation(Insert, "s"),
    Operation(Insert, "w"),
    Operation(Delete, "b"),
    Operation(Equals, "o"),
    Operation(Delete, "a"),
    Operation(Equals, "r"),
    Operation(Equals, "d"),
    Operation(Equals, "s")
  )

  test("a stringified Diff is the logical sum of it's Operations") {
    val diff = Diff(original, modified, ops)
    diff.toString should equal("bills +s+w-bo-ards")
  }

  test("a humanized diff merges like operations and equalities") {
    val diff = Diff(original, modified, ops)
    diff.humanized should equal("bills -boa+swords")
  }

  test("a diff when converted to html is also humanized") {
    val diff = Diff(original, modified, ops)
    diff.html should equal("<span>bills </span><del>boa</del><ins>swo</ins><span>rds</span>")
  }

  test("a diff can be created from an original and modified string") {
    val diff = Diff.create(original, modified)
    diff should equal(Diff(original, modified, ops))
  }

  test("lcs gets the longest common subsequence between two strings") {
    val lcs = PrivateMethod[String]('lcs)
    val result = Diff invokePrivate lcs(original, modified)
    result should equal(longestCommon)
  }

  test("commonPrefix gets the number of characters shared between two strings from the beginning") {
    val commonPrefix = PrivateMethod[Int]('commonPrefix)
    val result = Diff invokePrivate commonPrefix(original, modified)
    result should equal(prefix)
  }

  test("commonSuffix gets the number of characters shared between two strings from the end") {
    val commonSuffix = PrivateMethod[Int]('commonSuffix)
    val result = Diff invokePrivate commonSuffix(original, modified)
    result should equal(suffix)
  }
}