package net.ironforged.scaladiff

import net.ironforged.scaladiff._
import net.ironforged.scaladiff.Commons._
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

  /**
   * Convert the diff into a more human-readable format
   */
  /*private def humanize(): List[Operation] = {
    var result = List.empty[Operation]

    for (i <- 0 until diffs.indices.last) {
      val current  = diffs(i)
      val previous = if (diffs.indices.head == i) Operation(Equals, "") else diffs(i - 1)

      current.op match {
        case Insert => {
          if (previous.op == Equals) {

          }
        }
      }
    }

    result
  }*/
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
    if (a.size == 0 || b.size == 0) { return "" }
    else {
      // Same string? LCS is the string itself..
      if (a == b) { return a }
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
        var (x, y) = (a.size, b.size)
        do {
          if      (lengths(x)(y) == lengths(x - 1)(y)) { x -= 1 }
          else if (lengths(x)(y) == lengths(x)(y - 1)) { y -= 1 }
          else {
            builder += a(x-1)
            x -= 1
            y -= 1
          }
        } while (x != 0 && y != 0)

        // Due to the traceback approach, we built the result in reverse
        builder.toString.reverse
      }
    }
  }

  /**
   * Reorder and merge edits, equalities. Any edit section can move as long as it doesn't cross an equality.
   * @param diffs List of Operations
   */
  def cleanup(diffs: List[Operation]): String = {
    var buffer = List(Operation(Equals, ""))

    var deletes      = 0
    var inserts      = 0
    var commonLength = 0
    var deleted      = ""
    var inserted     = ""
    var previous: Operation = null
    var currentIndex = 0
    var current      = diffs.head

    while (current != null) {
      current.op match {
        case Insert => {
          inserts   = inc(inserts)
          inserted += current.text
          buffer    = buffer :+ current
          previous  = null
        }
        case Delete => {
          deletes  = inc(deletes)
          deleted += current.text
          buffer   = buffer :+ current
          previous = null
        }
        case Equals => {
          if (deletes + inserts > 1) {
            val bothTypes = deletes != 0 && inserts != 0
            // Delete

          }
        }
      }

      (currentIndex, current) =
        if (diffs.length == currentIndex) {
          (currentIndex, null)
        } else {
          (inc(currentIndex), diffs(inc(currentIndex)))
        }

      case EQUAL:
        if (count_delete + count_insert > 1) {
          boolean both_types = count_delete != 0 && count_insert != 0;
          // Delete the offending records.
          pointer.previous();  // Reverse direction.
          while (count_delete-- > 0) {
            pointer.previous();
            pointer.remove();
          }
          while (count_insert-- > 0) {
            pointer.previous();
            pointer.remove();
          }
          if (both_types) {
            // Factor out any common prefixies.
            commonlength = diff_commonPrefix(text_insert, text_delete);
            if (commonlength != 0) {
              if (pointer.hasPrevious()) {
                thisDiff = pointer.previous();
                assert thisDiff.operation == Operation.EQUAL
                  : "Previous diff should have been an equality.";
                thisDiff.text += text_insert.substring(0, commonlength);
                pointer.next();
              } else {
                pointer.add(new Diff(Operation.EQUAL,
                  text_insert.substring(0, commonlength)));
              }
              text_insert = text_insert.substring(commonlength);
              text_delete = text_delete.substring(commonlength);
            }
            // Factor out any common suffixies.
            commonlength = diff_commonSuffix(text_insert, text_delete);
            if (commonlength != 0) {
              thisDiff = pointer.next();
              thisDiff.text = text_insert.substring(text_insert.length()
                - commonlength) + thisDiff.text;
              text_insert = text_insert.substring(0, text_insert.length()
                - commonlength);
              text_delete = text_delete.substring(0, text_delete.length()
                - commonlength);
              pointer.previous();
            }
          }
          // Insert the merged records.
          if (text_delete.length() != 0) {
            pointer.add(new Diff(Operation.DELETE, text_delete));
          }
          if (text_insert.length() != 0) {
            pointer.add(new Diff(Operation.INSERT, text_insert));
          }
          // Step forward to the equality.
          thisDiff = pointer.hasNext() ? pointer.next() : null;
        } else if (prevEqual != null) {
          // Merge this equality with the previous one.
          prevEqual.text += thisDiff.text;
          pointer.remove();
          thisDiff = pointer.previous();
          pointer.next();  // Forward direction
        }
        count_insert = 0;
        count_delete = 0;
        text_delete = "";
        text_insert = "";
        prevEqual = thisDiff;
        break;
      }
      thisDiff = pointer.hasNext() ? pointer.next() : null;
    }
    if (diffs.getLast().text.length() == 0) {
      diffs.removeLast();  // Remove the dummy entry at the end.
    }

    /*
     * Second pass: look for single edits surrounded on both sides by equalities
     * which can be shifted sideways to eliminate an equality.
     * e.g: A<ins>BA</ins>C -> <ins>AB</ins>AC
     */
    boolean changes = false;
    // Create a new iterator at the start.
    // (As opposed to walking the current one back.)
    pointer = diffs.listIterator();
    Diff prevDiff = pointer.hasNext() ? pointer.next() : null;
    thisDiff = pointer.hasNext() ? pointer.next() : null;
    Diff nextDiff = pointer.hasNext() ? pointer.next() : null;
    // Intentionally ignore the first and last element (don't need checking).
    while (nextDiff != null) {
      if (prevDiff.operation == Operation.EQUAL &&
        nextDiff.operation == Operation.EQUAL) {
        // This is a single edit surrounded by equalities.
        if (thisDiff.text.endsWith(prevDiff.text)) {
          // Shift the edit over the previous equality.
          thisDiff.text = prevDiff.text
          + thisDiff.text.substring(0, thisDiff.text.length()
            - prevDiff.text.length());
          nextDiff.text = prevDiff.text + nextDiff.text;
          pointer.previous(); // Walk past nextDiff.
          pointer.previous(); // Walk past thisDiff.
          pointer.previous(); // Walk past prevDiff.
          pointer.remove(); // Delete prevDiff.
          pointer.next(); // Walk past thisDiff.
          thisDiff = pointer.next(); // Walk past nextDiff.
          nextDiff = pointer.hasNext() ? pointer.next() : null;
          changes = true;
        } else if (thisDiff.text.startsWith(nextDiff.text)) {
          // Shift the edit over the next equality.
          prevDiff.text += nextDiff.text;
          thisDiff.text = thisDiff.text.substring(nextDiff.text.length())
          + nextDiff.text;
          pointer.remove(); // Delete nextDiff.
          nextDiff = pointer.hasNext() ? pointer.next() : null;
          changes = true;
        }
      }
      prevDiff = thisDiff;
      thisDiff = nextDiff;
      nextDiff = pointer.hasNext() ? pointer.next() : null;
    }
    // If shifts were made, the diff needs reordering and another shift sweep.
    if (changes) {
      diff_cleanupMerge(diffs);
    }
  }
}
