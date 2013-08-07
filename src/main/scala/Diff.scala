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
   * using the traceback approach to solving this problem
   * @param a First string
   * @param b Second string
   * @return Longest common subsequence
   */
  def lcs(a: String, b: String): String = {
    // Empty pair of strings? No LCS...
    if (a.size==0 || b.size==0) {
      return ""
    }
    else {
      // Same string? LCS is the string itself..
      if (a==b) {
        return a
      }
      else {
        // Construct the LCS matrix using the lengths of the subsequences,
        // this is done to reduce the memory needed to solve the problem
        val lengths = Array.ofDim[Int](a.size + 1,b.size + 1)
        for (i <- 0 until a.size) {
          for (j <- 0 until b.size) {
            if (a(i) == b(j)) {
              lengths(i + 1)(j + 1) = lengths(i)(j) + 1
            }
            else {
              lengths(i + 1)(j + 1) = Math.max(lengths(i + 1)(j),lengths(i)(j + 1))
            }
          }
        }

        // Starting from the last cell in the matrix, trace back towards the origin, accumulating commonalities
        val builder = new StringBuilder()
        var x = a.size
        var y = b.size
        do {
          if (lengths(x)(y) == lengths(x - 1)(y)) {
            x -= 1
          }
          else if (lengths(x)(y) == lengths(x)(y - 1)) {
            y -= 1
          }
          else {
            builder += a(x-1)
            x -= 1
            y -= 1
          }
        } while (x!=0 && y!=0)

        // Due to the traceback approach, we built the result in reverse
        builder.toString.reverse
      }
    }
  }
}
