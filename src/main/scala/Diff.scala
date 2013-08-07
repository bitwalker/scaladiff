package net.ironforged.scaladiff

import net.ironforged.scaladiff._
import net.ironforged.scaladiff.OperationType._

case class Diff(original: String, modified: String, diffs: List[Operation]) {
  /**
   * Create a nice HTML report of the diff
   */
  def html: String = {
    diffs.foldLeft("") { (html, diff) =>
      val text = diff.text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\n", "&para;<br>")
      val tag = diff.op match {
        case Insert => s"<ins>$text</ins>"
        case Delete => s"<del>$text</ins>"
        case Equals => s"<span>$text</span>"
      }
      html + tag
    }
  }

  /**
   * Create a unidiff
   */
  def unidiff: String = {
    // TODO: Make this an actual unidiff
    diffs.foldLeft("") { (res, diff) =>
      val text = diff.text
      val op = diff.op match {
        case Insert => s"+$text"
        case Delete => s"-$text"
        case Equals => text
      }
      res + op
    }
  }
}

object Diff {
  /**
   * Creates a new Diff
   * @param a First string
   * @param b Second string
   * @return Diff
   */
  def create(a: String, b: String): Diff = {
    import scala.util.control.Breaks._

    var (original: String, modified: String) = (a, b)
    var subsequence: String = lcs(original, modified)
    var result = List.empty[Operation]

    while (subsequence.length > 0) {
      val sFirst = subsequence(0)
      subsequence = subsequence.drop(1)

      breakable { while (modified.length > 0) {
        val mFirst = modified(0)
        modified = modified.drop(1)
        if (mFirst == sFirst) break

        result = result :+ Operation(Insert, mFirst.toString)
      } }
      breakable { while (original.length > 0) {
        val oFirst = original(0)
        original = original.drop(1)
        if (oFirst == sFirst) break
        result = result :+ Operation(Delete, oFirst.toString)
      } }
      result = result :+ Operation(Equals, sFirst.toString)
    }
    while (modified.length > 0) {
      val mFirst = modified(0)
      modified = modified.drop(1)
      result = result :+ Operation(Insert, mFirst.toString)
    }
    while (original.length > 0) {
      val oFirst = original(0)
      original = original.drop(1)
      result = result :+ Operation(Delete, oFirst.toString)
    }

    Diff(a, b, result)
  }

  /**
   * Generate the longest common subsequence between two strings
   * @param a First string
   * @param b Second string
   * @return Longest common subsequence
   */
  private def lcs(a: String, b: String): String = {
    if (a.size == 0 || b.size == 0) {
      ""
    }
    else if (a == b) {
      a
    }
    else {
      if (a(a.size - 1) == b(b.size - 1)) {
        lcs(a.substring(0, a.size - 1), b.substring(0, b.size - 1)) + a(a.size - 1)
      }
      else {
        val x = lcs(a, b.substring(0, b.size - 1))
        val y = lcs(a.substring(0, a.size - 1), b)
        if (x.size > y.size) x else y
      }
    }
  }
}
