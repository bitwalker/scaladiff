package net.ironforged.scaladiff

import java.util.{LinkedList, ListIterator}
import java.net.{URLDecoder, URLEncoder}

import net.ironforged.scaladiff.Operation._
import net.ironforged.scaladiff.helpers._

/**
 * Class representing one diff operation.
 */
case class Diff(operation: Operation.Value, text: String) {
  /**
   * Display a human-readable version of this Diff.
   * @return text version.
   */
  override def toString: String = {
    val prettyText = text.replace('\n', '\u00b6')
    s"Diff($operation, \"$prettyText\")"
  }
}

object Diff {
  /**
   * Number of seconds to map a diff before giving up (0 for infinity).
   */
  private val TIMEOUT = 1.0f
  /**
   * Cost of an empty edit operation in terms of edit characters.
   */
  private val EditCost = 4

  // Define some regex patterns for matching boundaries.
  private val BLANK_LINE_END   = "\\n\\r?\\n\\Z".r
  private val BLANK_LINE_START = "\\A\\r?\\n\\r?\\n".r

  /**
   * Find the differences between two texts.
   * Run a faster, slightly less optimal diff.
   * This method allows the 'checklines' of diff_main() to be optional.
   * Most of the time checklines is wanted, so default to true.
   * @param text1 Old string to be diffed.
   * @param text2 New string to be diffed.
   * @return Linked List of Diff objects.
   */
  def execute(text1: String, text2: String): LinkedList[Diff] = {
    execute(text1, text2, true)
  }

  /**
   * Find the differences between two texts.
   * @param text1 Old string to be diffed.
   * @param text2 New string to be diffed.
   * @param checklines Speedup flag.  If false, then don't run a
   *                   line-level diff first to identify the changed areas.
   *                   If true, then run a faster slightly less optimal diff.
   * @return Linked List of Diff objects.
   */
  def execute(text1: String, text2: String, checklines: Boolean): LinkedList[Diff] = {
    var deadline: Long = 0L
    if (TIMEOUT <= 0) {
      deadline = Long.MaxValue
    }
    else {
      deadline = System.currentTimeMillis + (TIMEOUT * 1000).asInstanceOf[Long]
    }
    execute(text1, text2, checklines, deadline)
  }

  /**
   * Find the differences between two texts.  Simplifies the problem by
   * stripping any common prefix or suffix off the texts before diffing.
   * @param text1 Old string to be diffed.
   * @param text2 New string to be diffed.
   * @param checklines Speedup flag.  If false, then don't run a
   *                   line-level diff first to identify the changed areas.
   *                   If true, then run a faster slightly less optimal diff.
   * @param deadline Time when the diff should be complete by.  Used
   *                 internally for recursive calls.  Users should set DiffTimeout instead.
   * @return Linked List of Diff objects.
   */
  private def execute(text1: String, text2: String, checklines: Boolean, deadline: Long): LinkedList[Diff] = {
    if (text1 == null || text2 == null) {
      throw new IllegalArgumentException("Null inputs. (diff_main)")
    }

    var first  = text1
    var second = text2

    var diffs = new LinkedList[Diff]
    if (first == second) {
      if (first.length != 0) {
        diffs.add(Diff(Equal, first))
      }
      return diffs
    }

    var commonLength = Diff.commonPrefix(first, second)
    val commonPrefix = first.substring(0, commonLength)
    first  = first.substring(commonLength)
    second = second.substring(commonLength)
    commonLength = Diff.commonSuffix(first, second)
    val commonSuffix = first.substring(first.length - commonLength)
    first = first.substring(0, first.length - commonLength)
    second = second.substring(0, second.length - commonLength)
    diffs = Diff.compute(first, second, checklines, deadline)
    if (commonPrefix.length != 0) {
      diffs.addFirst(Diff(Equal, commonPrefix))
    }
    if (commonSuffix.length != 0) {
      diffs.addLast(Diff(Equal, commonSuffix))
    }
    diff_cleanupMerge(diffs)
    diffs
  }

  /**
   * Find the differences between two texts.  Assumes that the texts do not
   * have any common prefix or suffix.
   * @param text1 Old string to be diffed.
   * @param text2 New string to be diffed.
   * @param checklines Speedup flag.  If false, then don't run a
   *                   line-level diff first to identify the changed areas.
   *                   If true, then run a faster slightly less optimal diff.
   * @param deadline Time when the diff should be complete by.
   * @return Linked List of Diff objects.
   */
  private def compute(text1: String, text2: String, checklines: Boolean, deadline: Long): LinkedList[Diff] = {
    var diffs = new LinkedList[Diff]

    if (text1.length == 0) {
      diffs.add(Diff(Insert, text2))
      return diffs
    }

    if (text2.length == 0) {
      diffs.add(Diff(Delete, text1))
      return diffs
    }

    val (longText, shortText) = if (text1.length > text2.length) (text1, text2) else (text2, text1)
    val i = longText.indexOf(shortText)
    if (i != -1) {
      val op = if ((text1.length > text2.length)) Delete else Insert
      diffs.add(Diff(op, longText.substring(0, i)))
      diffs.add(Diff(Equal, shortText))
      diffs.add(Diff(op, longText.substring(i + shortText.length)))
      return diffs
    }

    if (shortText.length == 1) {
      diffs.add(Diff(Delete, text1))
      diffs.add(Diff(Insert, text2))
      return diffs
    }

    val hm: Array[String] = diff_halfMatch(text1, text2)
    hm match {
      case (text1A, text1B, text2A, text2B, midCommon) => {
        val diffA = execute(text1A, text2A, checklines, deadline)
        val diffB = execute(text1B, text2B, checklines, deadline)
        diffs = diffA
        diffs.add(Diff(Equal, midCommon))
        diffs.addAll(diffB)
        return diffs
      }
    }

    if (checklines && text1.length > 100 && text2.length > 100) {
      return diff_lineMode(text1, text2, deadline)
    }

    diff_bisect(text1, text2, deadline)
  }

  /**
   * Do a quick line-level diff on both strings, then rediff the parts for
   * greater accuracy.
   * This speedup can produce non-minimal diffs.
   * @param text1 Old string to be diffed.
   * @param text2 New string to be diffed.
   * @param deadline Time when the diff should be complete by.
   * @return Linked List of Diff objects.
   */
  private def lineMode(text1: String, text2: String, deadline: Long): LinkedList[Diff] = {
    val b = linesToChars(text1, text2)
    text1 = b.chars1
    text2 = b.chars2
    val linearray: List[String] = b.lineArray
    val diffs: LinkedList[diff_match_patch.Diff] = diff_main(text1, text2, false, deadline)
    diff_charsToLines(diffs, linearray)
    diff_cleanupSemantic(diffs)
    diffs.add(new diff_match_patch.Diff(Operation.EQUAL, ""))
    var count_delete: Int = 0
    var count_insert: Int = 0
    var text_delete: String = ""
    var text_insert: String = ""
    val pointer: ListIterator[diff_match_patch.Diff] = diffs.listIterator
    var thisDiff: diff_match_patch.Diff = pointer.next
    while (thisDiff != null) {
      thisDiff.operation match {
        case INSERT =>
          count_insert += 1
          text_insert += thisDiff.text
          break //todo: break is not supported
        case DELETE =>
          count_delete += 1
          text_delete += thisDiff.text
          break //todo: break is not supported
        case EQUAL =>
          if (count_delete >= 1 && count_insert >= 1) {
            pointer.previous
            {
              var j: Int = 0
              while (j < count_delete + count_insert) {
                {
                  pointer.previous
                  pointer.remove
                }
                ({
                  j += 1; j - 1
                })
              }
            }
            import scala.collection.JavaConversions._
            for (newDiff <- diff_main(text_delete, text_insert, false, deadline)) {
              pointer.add(newDiff)
            }
          }
          count_insert = 0
          count_delete = 0
          text_delete = ""
          text_insert = ""
          break //todo: break is not supported
      }
      thisDiff = if (pointer.hasNext) pointer.next else null
    }
    diffs.removeLast
    return diffs
  }

  /**
   * Find the 'middle snake' of a diff, split the problem in two
   * and return the recursively constructed diff.
   * See Myers 1986 paper: An O(ND) Difference Algorithm and Its Variations.
   * @param text1 Old string to be diffed.
   * @param text2 New string to be diffed.
   * @param deadline Time at which to bail if not yet complete.
   * @return LinkedList of Diff objects.
   */
  protected def bisect(text1: String, text2: String, deadline: Long): LinkedList[Diff] = {
    val text1_length = text1.length
    val text2_length = text2.length
    val max_d = (text1_length + text2_length + 1) / 2
    val v_offset = max_d
    val v_length = 2 * max_d
    var v1 = List.empty[Int]
    var v2 = List.empty[Int]
    {
      var x: Int = 0
      while (x < v_length) {
        {
          v1. = -1
          v2(x) = -1
        }
        ({
          x += 1; x - 1
        })
      }
    }
    v1(v_offset + 1) = 0
    v2(v_offset + 1) = 0
    val delta: Int = text1_length - text2_length
    val front: Boolean = (delta % 2 != 0)
    var k1start: Int = 0
    var k1end: Int = 0
    var k2start: Int = 0
    var k2end: Int = 0
    {
      var d: Int = 0
      while (d < max_d) {
        {
          if (System.currentTimeMillis > deadline) {
            break //todo: break is not supported
          }
          {
            var k1: Int = -d + k1start
            while (k1 <= d - k1end) {
              {
                val k1_offset: Int = v_offset + k1
                var x1: Int = 0
                if (k1 == -d || (k1 != d && v1(k1_offset - 1) < v1(k1_offset + 1))) {
                  x1 = v1(k1_offset + 1)
                }
                else {
                  x1 = v1(k1_offset - 1) + 1
                }
                var y1: Int = x1 - k1
                while (x1 < text1_length && y1 < text2_length && text1.charAt(x1) == text2.charAt(y1)) {
                  x1 += 1
                  y1 += 1
                }
                v1(k1_offset) = x1
                if (x1 > text1_length) {
                  k1end += 2
                }
                else if (y1 > text2_length) {
                  k1start += 2
                }
                else if (front) {
                  val k2_offset: Int = v_offset + delta - k1
                  if (k2_offset >= 0 && k2_offset < v_length && v2(k2_offset) != -1) {
                    val x2: Int = text1_length - v2(k2_offset)
                    if (x1 >= x2) {
                      return bisectSplit(text1, text2, x1, y1, deadline)
                    }
                  }
                }
              }
              k1 += 2
            }
          }
          {
            var k2: Int = -d + k2start
            while (k2 <= d - k2end) {
              {
                val k2_offset: Int = v_offset + k2
                var x2: Int = 0
                if (k2 == -d || (k2 != d && v2(k2_offset - 1) < v2(k2_offset + 1))) {
                  x2 = v2(k2_offset + 1)
                }
                else {
                  x2 = v2(k2_offset - 1) + 1
                }
                var y2: Int = x2 - k2
                while (x2 < text1_length && y2 < text2_length && text1.charAt(text1_length - x2 - 1) == text2.charAt(text2_length - y2 - 1)) {
                  x2 += 1
                  y2 += 1
                }
                v2(k2_offset) = x2
                if (x2 > text1_length) {
                  k2end += 2
                }
                else if (y2 > text2_length) {
                  k2start += 2
                }
                else if (!front) {
                  val k1_offset: Int = v_offset + delta - k2
                  if (k1_offset >= 0 && k1_offset < v_length && v1(k1_offset) != -1) {
                    val x1: Int = v1(k1_offset)
                    val y1: Int = v_offset + x1 - k1_offset
                    x2 = text1_length - x2
                    if (x1 >= x2) {
                      return bisectSplit(text1, text2, x1, y1, deadline)
                    }
                  }
                }
              }
              k2 += 2
            }
          }
        }
        ({
          d += 1; d - 1
        })
      }
    }
    val diffs = new LinkedList[Diff]
    diffs.add(Diff(Delete, text1))
    diffs.add(Diff(Insert, text2))
    diffs
  }

  /**
   * Given the location of the 'middle snake', split the diff in two parts and recurse.
   * @param text1 Old string to be diffed.
   * @param text2 New string to be diffed.
   * @param x Index of split point in text1.
   * @param y Index of split point in text2.
   * @param deadline Time at which to bail if not yet complete.
   * @return LinkedList of Diff objects.
   */
  private def bisectSplit(text1: String, text2: String, x: Int, y: Int, deadline: Long): LinkedList[Diff] = {
    val text1a = text1.substring(0, x)
    val text1b = text1.substring(x)
    val text2a = text2.substring(0, y)
    val text2b = text2.substring(y)

    val diffs = execute(text1a, text2a, false, deadline)
    diffs addAll execute(text1b, text2b, false, deadline)
    diffs
  }

  /**
   * Split two texts into a list of strings.  Reduce the texts to a string of
   * hashes where each Unicode character represents one line.
   * @param text1 First string.
   * @param text2 Second string.
   * @return An object containing the encoded text1, the encoded text2 and
   *         the List of unique strings.  The zeroth element of the List of
   *         unique strings is intentionally blank.
   */
  protected def diff_linesToChars(text1: String, text2: String): diff_match_patch.LinesToCharsResult = {
    val lineArray: List[String] = new ArrayList[String]
    val lineHash: Map[String, Integer] = new HashMap[String, Integer]
    lineArray.add("")
    val chars1: String = diff_linesToCharsMunge(text1, lineArray, lineHash)
    val chars2: String = diff_linesToCharsMunge(text2, lineArray, lineHash)
    return new diff_match_patch.LinesToCharsResult(chars1, chars2, lineArray)
  }

  /**
   * Split a text into a list of strings.  Reduce the texts to a string of
   * hashes where each Unicode character represents one line.
   * @param text String to encode.
   * @param lineArray List of unique strings.
   * @param lineHash Map of strings to indices.
   * @return Encoded string.
   */
  private def linesToCharsMunge(text: String, lineArray: List[String], lineHash: Map[String, Integer]): String = {
    var lineStart: Int = 0
    var lineEnd: Int = -1
    var line: String = null
    val chars: StringBuilder = new StringBuilder
    while (lineEnd < text.length - 1) {
      lineEnd = text.indexOf('\n', lineStart)
      if (lineEnd == -1) {
        lineEnd = text.length - 1
      }
      line = text.substring(lineStart, lineEnd + 1)
      lineStart = lineEnd + 1
      if (lineHash.containsKey(line)) {
        chars.append(String.valueOf(lineHash.get(line).asInstanceOf[Int].asInstanceOf[Char]))
      }
      else {
        lineArray.add(line)
        lineHash.put(line, lineArray.size - 1)
        chars.append(String.valueOf((lineArray.size - 1).asInstanceOf[Char]))
      }
    }
    return chars.toString
  }

  /**
   * Rehydrate the text in a diff from a string of line hashes to real lines of text.
   * @param diffs LinkedList of Diff objects.
   * @param lines List of unique strings.
   */
  protected def charsToLines(diffs: LinkedList[Diff], lines: Seq[String]): LinkedList[Diff] = {
    diffs.toArray().map { diff =>
      val appending = diff.text.foldLeft("") { (text, c) => text + lines(c.toInt) }
      diff.copy(text = diff.text + appending)
    }
  }

  /**
   * Determine the common prefix of two strings
   * @param first First string.
   * @param second Second string.
   * @return The number of characters common to the start of each string.
   */
  def commonPrefix(first: String, second: String): Int = {
    // Performance analysis: http://neil.fraser.name/news/2007/10/09/
    val n = Math.min(first.length, second.length)
    for (i <- 0 to n) {
      if (first.charAt(i) != second.charAt(i)) {
        return i
      }
    }
    n
  }

  /**
   * Determine the common suffix of two strings
   * @param first  First string.
   * @param second Second string.
   * @return The number of characters common to the end of each string.
   */
  def commonSuffix(first: String, second: String): Int = {
    // Performance analysis: http://neil.fraser.name/news/2007/10/09/
    val n = Math.min(first.length, second.length)
    for (i <- 1 to n) {
      if (first.charAt(first.length - i) != second.charAt(second.length - i)) {
        return i - 1
      }
    }
    n
  }

  /**
   * Determine if the suffix of one string is the prefix of another.
   * @param text1 First string.
   * @param text2 Second string.
   * @return The number of characters common to the end of the first
   *         string and the start of the second string.
   */
  protected def diff_commonOverlap(text1: String, text2: String): Int = {
    val text1_length: Int = text1.length
    val text2_length: Int = text2.length
    if (text1_length == 0 || text2_length == 0) {
      return 0
    }
    if (text1_length > text2_length) {
      text1 = text1.substring(text1_length - text2_length)
    }
    else if (text1_length < text2_length) {
      text2 = text2.substring(0, text1_length)
    }
    val text_length: Int = Math.min(text1_length, text2_length)
    if (text1 == text2) {
      return text_length
    }
    var best: Int = 0
    var length: Int = 1
    while (true) {
      val pattern: String = text1.substring(text_length - length)
      val found: Int = text2.indexOf(pattern)
      if (found == -1) {
        return best
      }
      length += found
      if (found == 0 || (text1.substring(text_length - length) == text2.substring(0, length))) {
        best = length
        length += 1
      }
    }
  }

  /**
   * Do the two texts share a substring which is at least half the length of
   * the longer text?
   * This speedup can produce non-minimal diffs.
   * @param text1 First string.
   * @param text2 Second string.
   * @return Five element String array, containing the prefix of text1, the
   *         suffix of text1, the prefix of text2, the suffix of text2 and the
   *         common middle.  Or null if there was no match.
   */
  protected def diff_halfMatch(text1: String, text2: String): Array[String] = {
    if (Diff_Timeout <= 0) {
      return null
    }
    val longtext: String = if (text1.length > text2.length) text1 else text2
    val shorttext: String = if (text1.length > text2.length) text2 else text1
    if (longtext.length < 4 || shorttext.length * 2 < longtext.length) {
      return null
    }
    val hm1: Array[String] = diff_halfMatchI(longtext, shorttext, (longtext.length + 3) / 4)
    val hm2: Array[String] = diff_halfMatchI(longtext, shorttext, (longtext.length + 1) / 2)
    var hm: Array[String] = null
    if (hm1 == null && hm2 == null) {
      return null
    }
    else if (hm2 == null) {
      hm = hm1
    }
    else if (hm1 == null) {
      hm = hm2
    }
    else {
      hm = if (hm1(4).length > hm2(4).length) hm1 else hm2
    }
    if (text1.length > text2.length) {
      return hm
    }
    else {
      return Array[String](hm(2), hm(3), hm(0), hm(1), hm(4))
    }
  }

  /**
   * Does a substring of shorttext exist within longtext such that the
   * substring is at least half the length of longtext?
   * @param longtext Longer string.
   * @param shorttext Shorter string.
   * @param i Start index of quarter length substring within longtext.
   * @return Five element String array, containing the prefix of longtext, the
   *         suffix of longtext, the prefix of shorttext, the suffix of shorttext
   *         and the common middle.  Or null if there was no match.
   */
  private def diff_halfMatchI(longtext: String, shorttext: String, i: Int): Array[String] = {
    val seed: String = longtext.substring(i, i + longtext.length / 4)
    var j: Int = -1
    var best_common: String = ""
    var best_longtext_a: String = ""
    var best_longtext_b: String = ""
    var best_shorttext_a: String = ""
    var best_shorttext_b: String = ""
    while ((({
      j = shorttext.indexOf(seed, j + 1); j
    })) != -1) {
      val prefixLength: Int = diff_commonPrefix(longtext.substring(i), shorttext.substring(j))
      val suffixLength: Int = diff_commonSuffix(longtext.substring(0, i), shorttext.substring(0, j))
      if (best_common.length < suffixLength + prefixLength) {
        best_common = shorttext.substring(j - suffixLength, j) + shorttext.substring(j, j + prefixLength)
        best_longtext_a = longtext.substring(0, i - suffixLength)
        best_longtext_b = longtext.substring(i + prefixLength)
        best_shorttext_a = shorttext.substring(0, j - suffixLength)
        best_shorttext_b = shorttext.substring(j + prefixLength)
      }
    }
    if (best_common.length * 2 >= longtext.length) {
      return Array[String](best_longtext_a, best_longtext_b, best_shorttext_a, best_shorttext_b, best_common)
    }
    else {
      return null
    }
  }

  /**
   * Reduce the number of edits by eliminating semantically trivial equalities.
   * @param diffs LinkedList of Diff objects.
   */
  def diff_cleanupSemantic(diffs: LinkedList[diff_match_patch.Diff]) {
    if (diffs.isEmpty) {
      return
    }
    var changes: Boolean = false
    val equalities: Stack[diff_match_patch.Diff] = new Stack[diff_match_patch.Diff]
    var lastequality: String = null
    var pointer: ListIterator[diff_match_patch.Diff] = diffs.listIterator
    var length_insertions1: Int = 0
    var length_deletions1: Int = 0
    var length_insertions2: Int = 0
    var length_deletions2: Int = 0
    var thisDiff: diff_match_patch.Diff = pointer.next
    while (thisDiff != null) {
      if (thisDiff.operation eq Operation.EQUAL) {
        equalities.push(thisDiff)
        length_insertions1 = length_insertions2
        length_deletions1 = length_deletions2
        length_insertions2 = 0
        length_deletions2 = 0
        lastequality = thisDiff.text
      }
      else {
        if (thisDiff.operation eq Operation.INSERT) {
          length_insertions2 += thisDiff.text.length
        }
        else {
          length_deletions2 += thisDiff.text.length
        }
        if (lastequality != null && (lastequality.length <= Math.max(length_insertions1, length_deletions1)) && (lastequality.length <= Math.max(length_insertions2, length_deletions2))) {
          while (thisDiff ne equalities.lastElement) {
            thisDiff = pointer.previous
          }
          pointer.next
          pointer.set(new diff_match_patch.Diff(Operation.DELETE, lastequality))
          pointer.add(new diff_match_patch.Diff(Operation.INSERT, lastequality))
          equalities.pop
          if (!equalities.empty) {
            equalities.pop
          }
          if (equalities.empty) {
            while (pointer.hasPrevious) {
              pointer.previous
            }
          }
          else {
            thisDiff = equalities.lastElement
            while (thisDiff ne pointer.previous) {
            }
          }
          length_insertions1 = 0
          length_insertions2 = 0
          length_deletions1 = 0
          length_deletions2 = 0
          lastequality = null
          changes = true
        }
      }
      thisDiff = if (pointer.hasNext) pointer.next else null
    }
    if (changes) {
      diff_cleanupMerge(diffs)
    }
    diff_cleanupSemanticLossless(diffs)
    pointer = diffs.listIterator
    var prevDiff: diff_match_patch.Diff = null
    thisDiff = null
    if (pointer.hasNext) {
      prevDiff = pointer.next
      if (pointer.hasNext) {
        thisDiff = pointer.next
      }
    }
    while (thisDiff != null) {
      if (prevDiff.operation eq Operation.DELETE && thisDiff.operation eq Operation.INSERT) {
        val deletion: String = prevDiff.text
        val insertion: String = thisDiff.text
        val overlap_length1: Int = this.diff_commonOverlap(deletion, insertion)
        val overlap_length2: Int = this.diff_commonOverlap(insertion, deletion)
        if (overlap_length1 >= overlap_length2) {
          if (overlap_length1 >= deletion.length / 2.0 || overlap_length1 >= insertion.length / 2.0) {
            pointer.previous
            pointer.add(new diff_match_patch.Diff(Operation.EQUAL, insertion.substring(0, overlap_length1)))
            prevDiff.text = deletion.substring(0, deletion.length - overlap_length1)
            thisDiff.text = insertion.substring(overlap_length1)
          }
        }
        else {
          if (overlap_length2 >= deletion.length / 2.0 || overlap_length2 >= insertion.length / 2.0) {
            pointer.previous
            pointer.add(new diff_match_patch.Diff(Operation.EQUAL, deletion.substring(0, overlap_length2)))
            prevDiff.operation = Operation.INSERT
            prevDiff.text = insertion.substring(0, insertion.length - overlap_length2)
            thisDiff.operation = Operation.DELETE
            thisDiff.text = deletion.substring(overlap_length2)
          }
        }
        thisDiff = if (pointer.hasNext) pointer.next else null
      }
      prevDiff = thisDiff
      thisDiff = if (pointer.hasNext) pointer.next else null
    }
  }

  /**
   * Look for single edits surrounded on both sides by equalities
   * which can be shifted sideways to align the edit to a word boundary.
   * e.g: The c<ins>at c</ins>ame. -> The <ins>cat </ins>came.
   * @param diffs LinkedList of Diff objects.
   */
  def diff_cleanupSemanticLossless(diffs: LinkedList[diff_match_patch.Diff]) {
    var equality1: String = null
    var edit: String = null
    var equality2: String = null
    var commonString: String = null
    var commonOffset: Int = 0
    var score: Int = 0
    var bestScore: Int = 0
    var bestEquality1: String = null
    var bestEdit: String = null
    var bestEquality2: String = null
    val pointer: ListIterator[diff_match_patch.Diff] = diffs.listIterator
    var prevDiff: diff_match_patch.Diff = if (pointer.hasNext) pointer.next else null
    var thisDiff: diff_match_patch.Diff = if (pointer.hasNext) pointer.next else null
    var nextDiff: diff_match_patch.Diff = if (pointer.hasNext) pointer.next else null
    while (nextDiff != null) {
      if (prevDiff.operation eq Operation.EQUAL && nextDiff.operation eq Operation.EQUAL) {
        equality1 = prevDiff.text
        edit = thisDiff.text
        equality2 = nextDiff.text
        commonOffset = diff_commonSuffix(equality1, edit)
        if (commonOffset != 0) {
          commonString = edit.substring(edit.length - commonOffset)
          equality1 = equality1.substring(0, equality1.length - commonOffset)
          edit = commonString + edit.substring(0, edit.length - commonOffset)
          equality2 = commonString + equality2
        }
        bestEquality1 = equality1
        bestEdit = edit
        bestEquality2 = equality2
        bestScore = diff_cleanupSemanticScore(equality1, edit) + diff_cleanupSemanticScore(edit, equality2)
        while (edit.length != 0 && equality2.length != 0 && edit.charAt(0) == equality2.charAt(0)) {
          equality1 += edit.charAt(0)
          edit = edit.substring(1) + equality2.charAt(0)
          equality2 = equality2.substring(1)
          score = diff_cleanupSemanticScore(equality1, edit) + diff_cleanupSemanticScore(edit, equality2)
          if (score >= bestScore) {
            bestScore = score
            bestEquality1 = equality1
            bestEdit = edit
            bestEquality2 = equality2
          }
        }
        if (!(prevDiff.text == bestEquality1)) {
          if (bestEquality1.length != 0) {
            prevDiff.text = bestEquality1
          }
          else {
            pointer.previous
            pointer.previous
            pointer.previous
            pointer.remove
            pointer.next
            pointer.next
          }
          thisDiff.text = bestEdit
          if (bestEquality2.length != 0) {
            nextDiff.text = bestEquality2
          }
          else {
            pointer.remove
            nextDiff = thisDiff
            thisDiff = prevDiff
          }
        }
      }
      prevDiff = thisDiff
      thisDiff = nextDiff
      nextDiff = if (pointer.hasNext) pointer.next else null
    }
  }

  /**
   * Given two strings, compute a score representing whether the internal
   * boundary falls on logical boundaries.
   * Scores range from 6 (best) to 0 (worst).
   * @param first  First string.
   * @param second Second string.
   * @return The score.
   */
  private def cleanupSemanticScore(first: String, second: String): Int = {

    if (first.length == 0 || second.length == 0) {
      // Edges are the best.
      return 6;
    }

    val char1 = first.last
    val char2 = second.head
    val nonAlphaNumeric1 = !Character.isLetterOrDigit(char1)
    val nonAlphaNumeric2 = !Character.isLetterOrDigit(char2)
    val whitespace1 = nonAlphaNumeric1 && Character.isWhitespace(char1)
    val whitespace2 = nonAlphaNumeric2 && Character.isWhitespace(char2)
    val lineBreak1 = whitespace1 && Character.getType(char1) == Character.CONTROL
    val lineBreak2 = whitespace2 && Character.getType(char2) == Character.CONTROL
    val blankLine1 = lineBreak1 && first.matches(BLANK_LINE_END.pattern.pattern)
    val blankLine2 = lineBreak2 && second.matches(BLANK_LINE_START.pattern.pattern)

    if (blankLine1 || blankLine2) {
      // Five points for blank lines.
      return 5;
    } else if (lineBreak1 || lineBreak2) {
      // Four points for line breaks.
      return 4;
    } else if (nonAlphaNumeric1 && !whitespace1 && whitespace2) {
      // Three points for end of sentences.
      return 3;
    } else if (whitespace1 || whitespace2) {
      // Two points for whitespace.
      return 2;
    } else if (nonAlphaNumeric1 || nonAlphaNumeric2) {
      // One point for non-alphanumeric.
      return 1;
    }
    return 0;
  }

  /**
   * Reduce the number of edits by eliminating operationally trivial equalities.
   * @param diffs LinkedList of Diff objects.
   */
  def diff_cleanupEfficiency(diffs: LinkedList[diff_match_patch.Diff]) {
    if (diffs.isEmpty) {
      return
    }
    var changes: Boolean = false
    val equalities: Stack[diff_match_patch.Diff] = new Stack[diff_match_patch.Diff]
    var lastequality: String = null
    val pointer: ListIterator[diff_match_patch.Diff] = diffs.listIterator
    var pre_ins: Boolean = false
    var pre_del: Boolean = false
    var post_ins: Boolean = false
    var post_del: Boolean = false
    var thisDiff: diff_match_patch.Diff = pointer.next
    var safeDiff: diff_match_patch.Diff = thisDiff
    while (thisDiff != null) {
      if (thisDiff.operation eq Operation.EQUAL) {
        if (thisDiff.text.length < Diff_EditCost && (post_ins || post_del)) {
          equalities.push(thisDiff)
          pre_ins = post_ins
          pre_del = post_del
          lastequality = thisDiff.text
        }
        else {
          equalities.clear
          lastequality = null
          safeDiff = thisDiff
        }
        post_ins = ({
          post_del = false; post_del
        })
      }
      else {
        if (thisDiff.operation eq Operation.DELETE) {
          post_del = true
        }
        else {
          post_ins = true
        }
        if (lastequality != null && ((pre_ins && pre_del && post_ins && post_del) || ((lastequality.length < Diff_EditCost / 2) && ((if (pre_ins) 1 else 0) + (if (pre_del) 1 else 0) + (if (post_ins) 1 else 0) + (if (post_del) 1 else 0)) == 3))) {
          while (thisDiff ne equalities.lastElement) {
            thisDiff = pointer.previous
          }
          pointer.next
          pointer.set(new diff_match_patch.Diff(Operation.DELETE, lastequality))
          pointer.add(thisDiff = new diff_match_patch.Diff(Operation.INSERT, lastequality))
          equalities.pop
          lastequality = null
          if (pre_ins && pre_del) {
            post_ins = ({
              post_del = true; post_del
            })
            equalities.clear
            safeDiff = thisDiff
          }
          else {
            if (!equalities.empty) {
              equalities.pop
            }
            if (equalities.empty) {
              thisDiff = safeDiff
            }
            else {
              thisDiff = equalities.lastElement
            }
            while (thisDiff ne pointer.previous) {
            }
            post_ins = ({
              post_del = false; post_del
            })
          }
          changes = true
        }
      }
      thisDiff = if (pointer.hasNext) pointer.next else null
    }
    if (changes) {
      diff_cleanupMerge(diffs)
    }
  }

  /**
   * Reorder and merge like edit sections.  Merge equalities.
   * Any edit section can move as long as it doesn't cross an equality.
   * @param diffs LinkedList of Diff objects.
   */
  def diff_cleanupMerge(diffs: LinkedList[diff_match_patch.Diff]) {
    diffs.add(new diff_match_patch.Diff(Operation.EQUAL, ""))
    var pointer: ListIterator[diff_match_patch.Diff] = diffs.listIterator
    var count_delete: Int = 0
    var count_insert: Int = 0
    var text_delete: String = ""
    var text_insert: String = ""
    var thisDiff: diff_match_patch.Diff = pointer.next
    var prevEqual: diff_match_patch.Diff = null
    var commonlength: Int = 0
    while (thisDiff != null) {
      thisDiff.operation match {
        case INSERT =>
          count_insert += 1
          text_insert += thisDiff.text
          prevEqual = null
          break //todo: break is not supported
        case DELETE =>
          count_delete += 1
          text_delete += thisDiff.text
          prevEqual = null
          break //todo: break is not supported
        case EQUAL =>
          if (count_delete + count_insert > 1) {
            val both_types: Boolean = count_delete != 0 && count_insert != 0
            pointer.previous
            while (({
              count_delete -= 1; count_delete + 1
            }) > 0) {
              pointer.previous
              pointer.remove
            }
            while (({
              count_insert -= 1; count_insert + 1
            }) > 0) {
              pointer.previous
              pointer.remove
            }
            if (both_types) {
              commonlength = diff_commonPrefix(text_insert, text_delete)
              if (commonlength != 0) {
                if (pointer.hasPrevious) {
                  thisDiff = pointer.previous
                  assert(thisDiff.operation eq Operation.EQUAL, "Previous diff should have been an equality.")
                  thisDiff.text += text_insert.substring(0, commonlength)
                  pointer.next
                }
                else {
                  pointer.add(new diff_match_patch.Diff(Operation.EQUAL, text_insert.substring(0, commonlength)))
                }
                text_insert = text_insert.substring(commonlength)
                text_delete = text_delete.substring(commonlength)
              }
              commonlength = diff_commonSuffix(text_insert, text_delete)
              if (commonlength != 0) {
                thisDiff = pointer.next
                thisDiff.text = text_insert.substring(text_insert.length - commonlength) + thisDiff.text
                text_insert = text_insert.substring(0, text_insert.length - commonlength)
                text_delete = text_delete.substring(0, text_delete.length - commonlength)
                pointer.previous
              }
            }
            if (text_delete.length != 0) {
              pointer.add(new diff_match_patch.Diff(Operation.DELETE, text_delete))
            }
            if (text_insert.length != 0) {
              pointer.add(new diff_match_patch.Diff(Operation.INSERT, text_insert))
            }
            thisDiff = if (pointer.hasNext) pointer.next else null
          }
          else if (prevEqual != null) {
            prevEqual.text += thisDiff.text
            pointer.remove
            thisDiff = pointer.previous
            pointer.next
          }
          count_insert = 0
          count_delete = 0
          text_delete = ""
          text_insert = ""
          prevEqual = thisDiff
          break //todo: break is not supported
      }
      thisDiff = if (pointer.hasNext) pointer.next else null
    }
    if (diffs.getLast.text.length == 0) {
      diffs.removeLast
    }
    var changes: Boolean = false
    pointer = diffs.listIterator
    var prevDiff: diff_match_patch.Diff = if (pointer.hasNext) pointer.next else null
    thisDiff = if (pointer.hasNext) pointer.next else null
    var nextDiff: diff_match_patch.Diff = if (pointer.hasNext) pointer.next else null
    while (nextDiff != null) {
      if (prevDiff.operation eq Operation.EQUAL && nextDiff.operation eq Operation.EQUAL) {
        if (thisDiff.text.endsWith(prevDiff.text)) {
          thisDiff.text = prevDiff.text + thisDiff.text.substring(0, thisDiff.text.length - prevDiff.text.length)
          nextDiff.text = prevDiff.text + nextDiff.text
          pointer.previous
          pointer.previous
          pointer.previous
          pointer.remove
          pointer.next
          thisDiff = pointer.next
          nextDiff = if (pointer.hasNext) pointer.next else null
          changes = true
        }
        else if (thisDiff.text.startsWith(nextDiff.text)) {
          prevDiff.text += nextDiff.text
          thisDiff.text = thisDiff.text.substring(nextDiff.text.length) + nextDiff.text
          pointer.remove
          nextDiff = if (pointer.hasNext) pointer.next else null
          changes = true
        }
      }
      prevDiff = thisDiff
      thisDiff = nextDiff
      nextDiff = if (pointer.hasNext) pointer.next else null
    }
    if (changes) {
      diff_cleanupMerge(diffs)
    }
  }

  /**
   * loc is a location in the source, compute and return the equivalent location in the destination.
   * e.g. "The cat" vs "The big cat", 1->1, 5->8
   * @param diffs LinkedList of Diff objects.
   * @param loc Location within source.
   * @return Location within destination.
   */
  def xIndex(diffs: LinkedList[Diff], loc: Int): Int = {
    import scala.util.control.Breaks._

    var chars1 = 0
    var chars2 = 0
    var lastChars1 = 0
    var lastChars2 = 0
    var lastDiff: Diff = null
    breakable { for (diff <- diffs) {
      diff.operation match {
        case Operation.Insert => chars2 += diff.text.length
        case Operation.Delete => chars1 += diff.text.length
        case Operation.Equal  => {
          chars1 += diff.text.length
          chars2 += diff.text.length
        }
      }

      if (chars1 > loc) {
        lastDiff = diff
        break
      }

      lastChars1 = chars1
      lastChars2 = chars2
    } }

    if (lastDiff != null && lastDiff.operation == Operation.Delete) {
      // The location was deleted
      return lastChars2
    }

    // Add the remaining character length
    return lastChars2 + (loc - lastChars1)
  }

  /**
   * Convert a Diff list into an HTML report.
   * @param diffs LinkedList of Diff objects.
   * @return html as String.
   */
  def diffToHtml(diffs: LinkedList[Diff]): String = {
    import Operation._

    val html = new StringBuilder()
    for (diff <- diffs) {
      val text = diff.text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\n", "&para;<br>")
      diff.operation match {
        case Insert => html.append("<ins>").append(text).append("</ins>")
        case Delete => html.append("<del>").append(text).append("</del>")
        case Equal  => html.append("<span>").append(text).append("</span>")
      }
    }

    html.toString
  }

  /**
   * Compute and return the source text (all equalities and deletions).
   * @param diffs LinkedList of Diff objects.
   * @return Source text.
   */
  def diffSource(diffs: LinkedList[Diff]): String = {
    import Operation._

    diffs.toArray().foldLeft("") { (text, diff) =>
      if (diff.operation != Insert) (text + diff.text) else text
    }
  }

  /**
   * Compute and return the destination text (all equalities and insertions).
   * @param diffs LinkedList of Diff objects.
   * @return Destination text.
   */
  def diffDestination(diffs: LinkedList[Diff]): String = {
    import Operation._

    diffs.toArray().foldLeft("") { (text, diff) =>
      if (diff.operation != Delete) (text + diff.text) else text
    }
  }

  /**
   * Compute the Levenshtein distance; the number of inserted, deleted or substituted characters.
   * @param diffs LinkedList of Diff objects.
   * @return Levenshtein distance
   */
  def levenshtein(diffs: LinkedList[Diff]): Int = {
    import Operation._

    val (inserted, deleted, distance) = diffs.toArray().foldLeft((0, 0, 0)) { (counts, diff) =>
      val (insertions, deletions, dist) = counts
      diff.operation match {
        case Insert => (insertions + diff.text.length, deletions, dist)
        case Delete => (insertions, deletions + diff.text.length, dist)
        case Equal  => (0, 0, Math.max(insertions, deletions))
      }
    }

    distance + Math.max(inserted, deleted)
  }

  /**
   * Crush the diff into an encoded string which describes the operations
   * required to transform text1 into text2.
   * E.g. =3\t-2\t+ing  -> Keep 3 chars, delete 2 chars, insert 'ing'.
   * Operations are tab-separated.  Inserted text is escaped using %xx notation.
   * @param diffs Array of Diff objects.
   * @return Delta text.
   */
  def toDelta(diffs: LinkedList[Diff]): String = {

    val text = new StringBuilder()
    for (diff <- diffs) {
      diff.operation match {
        case Insert => {
          try { text.append('+').append(URLEncoder.encode(diff.text, "UTF-8").replace('+', ' ')).append('\t') }
          catch {
            // Super unlikely
            case ex: UnsupportedOperationException => throw new Error("This system does not support UTF-8", ex)
          }
        }
        case Delete => text.append('-').append(diff.text.length).append('\t')
        case Equal  => text.append('=').append(diff.text.length).append('\t')
      }
    }

    var delta = text.toString
    if (delta.length != 0) {
      // Strip off trailing tab character
      delta = delta.substring(0, delta.length - 1)
      delta = Strings.unescapeForEncodeUriCompatability(delta)
    }
    delta
  }

  /**
   * Given the original source text, and an encoded string which describes the
   * operations required to transform source into destination, compute the full diff.
   * @param source Source string for the diff.
   * @param delta Delta text.
   * @return LinkedList of Diff objects or null if invalid.
   * @throws IllegalArgumentException If invalid input.
   */
  def fromDelta(source: String, delta: String): LinkedList[Diff] = {
    import java.net.URLDecoder

    var cursor = 0
    val tokens = delta.split("\t")
    val diffs  = new java.util.LinkedList[Diff]()

    // Skip over blank tokens (blank tokens are ok (from a trailing \t))
    for (token <- tokens.filter(t => t.length > 0)) {
      // Each token begins with a one character parameter which specified
      // the operation of this token (delete, insert, equality)
      var param = token.substring(1)
      param.charAt(0) match {
        case '+' => {
          // decode would change all "+" to " "
          param = param.replace("+", "%2B")
          try {
            param = URLDecoder.decode(param, "UTF-8");
          } catch {
            // Not likely
            case e: UnsupportedOperationException => throw new Error("This system does not support UTF-8", e)
            // Malformed URI sequence
            case e: IllegalArgumentException => throw new IllegalArgumentException("Illegal escape in fromDelta: " + param, e)
          }
          diffs add Diff(Operation.Insert, param)
        }
        case '-' => {
          var n = 0
          try {
            n = Integer.parseInt(param)
          }
          catch {
            case e: NumberFormatException => throw new IllegalArgumentException("Invalid number in fromDelta: " + param, e)
          }

          if (n < 0) { throw new IllegalArgumentException("Negative number in fromDelta: " + param)}

          var text = ""
          try {
            val start = cursor
            val end   = cursor + n
            cursor    = end
            text = source.substring(start, end)
          }
          catch {
            case e: StringIndexOutOfBoundsException =>
              throw new IllegalArgumentException("Delta length(" + cursor + ") larger than source text length(" + source.length + ").", e)
          }

          diffs add Diff(Operation.Delete, text)
        }
        case '=' => {
          var n = 0
          try {
            n = Integer.parseInt(param)
          }
          catch {
            case e: NumberFormatException => throw new IllegalArgumentException("Invalid number in fromDelta: " + param, e)
          }

          if (n < 0) { throw new IllegalArgumentException("Negative number in fromDelta: " + param)}

          var text = ""
          try {
            val start = cursor
            val end   = cursor + n
            cursor    = end
            text = source.substring(start, end)
          }
          catch {
            case e: StringIndexOutOfBoundsException =>
              throw new IllegalArgumentException("Delta length(" + cursor + ") larger than source text length(" + source.length + ").", e)
          }

          diffs add Diff(Operation.Equal, text)
        }
        case _   => {
          // Anything else is an error.
          throw new IllegalArgumentException("Invalid diff operation in fromDelta: " + token.charAt(0));
        }
      }
    }
    if (cursor != source.length) {
      throw new IllegalArgumentException("Delta length (" + cursor + ") smaller than source text length (" + source.length + ").");
    }
    diffs
  }
}
