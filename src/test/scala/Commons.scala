import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers._

import net.ironforged.scaladiff.commons._

class CommonsSuite extends FunSuite {
  test("can insert an element in a list at the desired position") {
    var li = List(1, 2, 3, 4)
    li = insert(li, 2)(80)
    li should equal(List(1, 2, 80, 3, 4))
  }

  test("can insert multiple elements in a list at the desired position") {
    var li = List(1, 2, 3, 4)
    li = insert(li, 2)(80, 90, 100)
    li should equal(List(1, 2, 80, 90, 100, 3, 4))
  }

  test("can replace an element in a list with a new element") {
    var li = List(1, 2, 3, 4)
    li = replace(li, 2, 1)(80)
    li should equal(List(1, 2, 80, 4))
  }

  test("can replace multiple elements in a list with a single element") {
    var li = List(1, 2, 3, 4)
    li = replace(li, 2, 2)(80)
    li should equal(List(1, 2, 80))
  }

  test("can replace multiple elements in a list with more than one element") {
    var li = List(1, 2, 3, 4)
    li = replace(li, 2, 2)(80, 90, 100)
    li should equal(List(1, 2, 80, 90, 100))
  }

  test("can safely slice a string from any starting position for any number of characters") {
    val str = "testing"
    slice(str, -1, 4)   should equal("")
    slice(str, 0, -1)   should equal("")
    slice(str, 0, 4)    should equal("test")
    slice(str, 2, 4)    should equal("stin")
    slice(str, 10, 4)   should equal("")
    slice(str, 4, 10)   should equal("ing")
    slice(str, 10, 10)  should equal("")
  }

  test("can safely slice a string from the beginning regardless of it's length") {
    val str = "testing"
    sliceLeft(str, -1)  should equal("")
    sliceLeft(str, 0)   should equal("")
    sliceLeft(str, 4)   should equal("test")
    sliceLeft(str, 100) should equal("testing")
  }

  test("can safely slice a string from the end regardless of it's length") {
    val str = "testing"
    sliceRight(str, -1)  should equal("")
    sliceRight(str, 0)   should equal("")
    sliceRight(str, 4)   should equal("ting")
    sliceRight(str, 100) should equal("testing")
  }
}

