package helpers

/*
 * Diff Match and Patch
 *
 * Copyright 2006 Google Inc.
 * http://code.google.com/p/google-diff-match-patch/
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.io.UnsupportedEncodingException
import java.net.URLEncoder
import java.net.URLDecoder
import java.util.ArrayList
import java.util.Arrays
import java.util.HashMap
import java.util.LinkedList
import java.util.List
import java.util.ListIterator
import java.util.Map
import java.util.Stack
import java.util.regex.Matcher
import java.util.regex.Pattern
import java.lang.{Long, StringBuilder, String}
import scala.Predef.String
import scala.StringBuilder
import scala.Long

/*
 * Functions for diff, match and patch.
 * Computes the difference between two texts to create a patch.
 * Applies the patch onto another text, allowing for errors.
 *
 * @author fraser@google.com (Neil Fraser)
 */


class diff_match_patch {


  /**
   * Locate the best instance of 'pattern' in 'text' near 'loc'.
   * Returns -1 if no match found.
   * @param text The text to search.
   * @param pattern The pattern to search for.
   * @param loc The location to search around.
   * @return Best match index or -1.
   */
  def match_main(text: String, pattern: String, loc: Int): Int = {
    if (text == null || pattern == null) {
      throw new IllegalArgumentException("Null inputs. (match_main)")
    }
    loc = Math.max(0, Math.min(loc, text.length))
    if (text == pattern) {
      return 0
    }
    else if (text.length == 0) {
      return -1
    }
    else if (loc + pattern.length <= text.length && (text.substring(loc, loc + pattern.length) == pattern)) {
      return loc
    }
    else {
      return match_bitap(text, pattern, loc)
    }
  }

  /**
   * Locate the best instance of 'pattern' in 'text' near 'loc' using the
   * Bitap algorithm.  Returns -1 if no match found.
   * @param text The text to search.
   * @param pattern The pattern to search for.
   * @param loc The location to search around.
   * @return Best match index or -1.
   */
  protected def match_bitap(text: String, pattern: String, loc: Int): Int = {
    assert((Match_MaxBits == 0 || pattern.length <= Match_MaxBits), "Pattern too long for this application.")
    val s: Map[Character, Integer] = match_alphabet(pattern)
    var score_threshold: Double = Match_Threshold
    var best_loc: Int = text.indexOf(pattern, loc)
    if (best_loc != -1) {
      score_threshold = Math.min(match_bitapScore(0, best_loc, loc, pattern), score_threshold)
      best_loc = text.lastIndexOf(pattern, loc + pattern.length)
      if (best_loc != -1) {
        score_threshold = Math.min(match_bitapScore(0, best_loc, loc, pattern), score_threshold)
      }
    }
    val matchmask: Int = 1 << (pattern.length - 1)
    best_loc = -1
    var bin_min: Int = 0
    var bin_mid: Int = 0
    var bin_max: Int = pattern.length + text.length
    var last_rd: Array[Int] = new Array[Int](0)
    {
      var d: Int = 0
      while (d < pattern.length) {
        {
          bin_min = 0
          bin_mid = bin_max
          while (bin_min < bin_mid) {
            if (match_bitapScore(d, loc + bin_mid, loc, pattern) <= score_threshold) {
              bin_min = bin_mid
            }
            else {
              bin_max = bin_mid
            }
            bin_mid = (bin_max - bin_min) / 2 + bin_min
          }
          bin_max = bin_mid
          var start: Int = Math.max(1, loc - bin_mid + 1)
          val finish: Int = Math.min(loc + bin_mid, text.length) + pattern.length
          val rd: Array[Int] = new Array[Int](finish + 2)
          rd(finish + 1) = (1 << d) - 1
          {
            var j: Int = finish
            while (j >= start) {
              {
                var charMatch: Int = 0
                if (text.length <= j - 1 || !s.containsKey(text.charAt(j - 1))) {
                  charMatch = 0
                }
                else {
                  charMatch = s.get(text.charAt(j - 1))
                }
                if (d == 0) {
                  rd(j) = ((rd(j + 1) << 1) | 1) & charMatch
                }
                else {
                  rd(j) = (((rd(j + 1) << 1) | 1) & charMatch) | (((last_rd(j + 1) | last_rd(j)) << 1) | 1) | last_rd(j + 1)
                }
                if ((rd(j) & matchmask) != 0) {
                  val score: Double = match_bitapScore(d, j - 1, loc, pattern)
                  if (score <= score_threshold) {
                    score_threshold = score
                    best_loc = j - 1
                    if (best_loc > loc) {
                      start = Math.max(1, 2 * loc - best_loc)
                    }
                    else {
                      break //todo: break is not supported
                    }
                  }
                }
              }
              ({
                j -= 1; j + 1
              })
            }
          }
          if (match_bitapScore(d + 1, loc, loc, pattern) > score_threshold) {
            break //todo: break is not supported
          }
          last_rd = rd
        }
        ({
          d += 1; d - 1
        })
      }
    }
    return best_loc
  }

  /**
   * Compute and return the score for a match with e errors and x location.
   * @param e Number of errors in match.
   * @param x Location of match.
   * @param loc Expected location of match.
   * @param pattern Pattern being sought.
   * @return Overall score for match (0.0 = good, 1.0 = bad).
   */
  private def match_bitapScore(e: Int, x: Int, loc: Int, pattern: String): Double = {
    val accuracy: Float = e.asInstanceOf[Float] / pattern.length
    val proximity: Int = Math.abs(loc - x)
    if (Match_Distance == 0) {
      return if (proximity == 0) accuracy else 1.0
    }
    return accuracy + (proximity / Match_Distance.asInstanceOf[Float])
  }

  /**
   * Initialise the alphabet for the Bitap algorithm.
   * @param pattern The text to encode.
   * @return Hash of character locations.
   */
  protected def match_alphabet(pattern: String): Map[Character, Integer] = {
    val s: Map[Character, Integer] = new HashMap[Character, Integer]
    val char_pattern: Array[Char] = pattern.toCharArray
    for (c <- char_pattern) {
      s.put(c, 0)
    }
    var i: Int = 0
    for (c <- char_pattern) {
      s.put(c, s.get(c) | (1 << (pattern.length - i - 1)))
      i += 1
    }
    return s
  }

  /**
   * Increase the context until it is unique,
   * but don't let the pattern expand beyond Match_MaxBits.
   * @param patch The patch to grow.
   * @param text Source text.
   */
  protected def patch_addContext(patch: diff_match_patch.Patch, text: String) {
    if (text.length == 0) {
      return
    }
    var pattern: String = text.substring(patch.start2, patch.start2 + patch.length1)
    var padding: Int = 0
    while (text.indexOf(pattern) != text.lastIndexOf(pattern) && pattern.length < Match_MaxBits - Patch_Margin - Patch_Margin) {
      padding += Patch_Margin
      pattern = text.substring(Math.max(0, patch.start2 - padding), Math.min(text.length, patch.start2 + patch.length1 + padding))
    }
    padding += Patch_Margin
    val prefix: String = text.substring(Math.max(0, patch.start2 - padding), patch.start2)
    if (prefix.length != 0) {
      patch.diffs.addFirst(new diff_match_patch.Diff(Operation.EQUAL, prefix))
    }
    val suffix: String = text.substring(patch.start2 + patch.length1, Math.min(text.length, patch.start2 + patch.length1 + padding))
    if (suffix.length != 0) {
      patch.diffs.addLast(new diff_match_patch.Diff(Operation.EQUAL, suffix))
    }
    patch.start1 -= prefix.length
    patch.start2 -= prefix.length
    patch.length1 += prefix.length + suffix.length
    patch.length2 += prefix.length + suffix.length
  }

  /**
   * Compute a list of patches to turn text1 into text2.
   * A set of diffs will be computed.
   * @param text1 Old text.
   * @param text2 New text.
   * @return LinkedList of Patch objects.
   */
  def patch_make(text1: String, text2: String): LinkedList[diff_match_patch.Patch] = {
    if (text1 == null || text2 == null) {
      throw new IllegalArgumentException("Null inputs. (patch_make)")
    }
    val diffs: LinkedList[diff_match_patch.Diff] = diff_main(text1, text2, true)
    if (diffs.size > 2) {
      diff_cleanupSemantic(diffs)
      diff_cleanupEfficiency(diffs)
    }
    return patch_make(text1, diffs)
  }

  /**
   * Compute a list of patches to turn text1 into text2.
   * text1 will be derived from the provided diffs.
   * @param diffs Array of Diff objects for text1 to text2.
   * @return LinkedList of Patch objects.
   */
  def patch_make(diffs: LinkedList[diff_match_patch.Diff]): LinkedList[diff_match_patch.Patch] = {
    if (diffs == null) {
      throw new IllegalArgumentException("Null inputs. (patch_make)")
    }
    val text1: String = diff_text1(diffs)
    return patch_make(text1, diffs)
  }

  /**
   * Compute a list of patches to turn text1 into text2.
   * text2 is ignored, diffs are the delta between text1 and text2.
   * @param text1 Old text
   * @param text2 Ignored.
   * @param diffs Array of Diff objects for text1 to text2.
   * @return LinkedList of Patch objects.
   * @deprecated Prefer patch_make(String text1, LinkedList<Diff> diffs).
   */
  def patch_make(text1: String, text2: String, diffs: LinkedList[diff_match_patch.Diff]): LinkedList[diff_match_patch.Patch] = {
    return patch_make(text1, diffs)
  }

  /**
   * Compute a list of patches to turn text1 into text2.
   * text2 is not provided, diffs are the delta between text1 and text2.
   * @param text1 Old text.
   * @param diffs Array of Diff objects for text1 to text2.
   * @return LinkedList of Patch objects.
   */
  def patch_make(text1: String, diffs: LinkedList[diff_match_patch.Diff]): LinkedList[diff_match_patch.Patch] = {
    if (text1 == null || diffs == null) {
      throw new IllegalArgumentException("Null inputs. (patch_make)")
    }
    val patches: LinkedList[diff_match_patch.Patch] = new LinkedList[diff_match_patch.Patch]
    if (diffs.isEmpty) {
      return patches
    }
    var patch: diff_match_patch.Patch = new diff_match_patch.Patch
    var char_count1: Int = 0
    var char_count2: Int = 0
    var prepatch_text: String = text1
    var postpatch_text: String = text1
    import scala.collection.JavaConversions._
    for (aDiff <- diffs) {
      if (patch.diffs.isEmpty && aDiff.operation ne Operation.EQUAL) {
        patch.start1 = char_count1
        patch.start2 = char_count2
      }
      aDiff.operation match {
        case INSERT =>
          patch.diffs.add(aDiff)
          patch.length2 += aDiff.text.length
          postpatch_text = postpatch_text.substring(0, char_count2) + aDiff.text + postpatch_text.substring(char_count2)
          break //todo: break is not supported
        case DELETE =>
          patch.length1 += aDiff.text.length
          patch.diffs.add(aDiff)
          postpatch_text = postpatch_text.substring(0, char_count2) + postpatch_text.substring(char_count2 + aDiff.text.length)
          break //todo: break is not supported
        case EQUAL =>
          if (aDiff.text.length <= 2 * Patch_Margin && !patch.diffs.isEmpty && aDiff ne diffs.getLast) {
            patch.diffs.add(aDiff)
            patch.length1 += aDiff.text.length
            patch.length2 += aDiff.text.length
          }
          if (aDiff.text.length >= 2 * Patch_Margin) {
            if (!patch.diffs.isEmpty) {
              patch_addContext(patch, prepatch_text)
              patches.add(patch)
              patch = new diff_match_patch.Patch
              prepatch_text = postpatch_text
              char_count1 = char_count2
            }
          }
          break //todo: break is not supported
      }
      if (aDiff.operation ne Operation.INSERT) {
        char_count1 += aDiff.text.length
      }
      if (aDiff.operation ne Operation.DELETE) {
        char_count2 += aDiff.text.length
      }
    }
    if (!patch.diffs.isEmpty) {
      patch_addContext(patch, prepatch_text)
      patches.add(patch)
    }
    return patches
  }

  /**
   * Given an array of patches, return another array that is identical.
   * @param patches Array of Patch objects.
   * @return Array of Patch objects.
   */
  def patch_deepCopy(patches: LinkedList[diff_match_patch.Patch]): LinkedList[diff_match_patch.Patch] = {
    val patchesCopy: LinkedList[diff_match_patch.Patch] = new LinkedList[diff_match_patch.Patch]
    import scala.collection.JavaConversions._
    for (aPatch <- patches) {
      val patchCopy: diff_match_patch.Patch = new diff_match_patch.Patch
      import scala.collection.JavaConversions._
      for (aDiff <- aPatch.diffs) {
        val diffCopy: diff_match_patch.Diff = new diff_match_patch.Diff(aDiff.operation, aDiff.text)
        patchCopy.diffs.add(diffCopy)
      }
      patchCopy.start1 = aPatch.start1
      patchCopy.start2 = aPatch.start2
      patchCopy.length1 = aPatch.length1
      patchCopy.length2 = aPatch.length2
      patchesCopy.add(patchCopy)
    }
    return patchesCopy
  }

  /**
   * Merge a set of patches onto the text.  Return a patched text, as well
   * as an array of true/false values indicating which patches were applied.
   * @param patches Array of Patch objects
   * @param text Old text.
   * @return Two element Object array, containing the new text and an array of
   *         boolean values.
   */
  def patch_apply(patches: LinkedList[diff_match_patch.Patch], text: String): Array[AnyRef] = {
    if (patches.isEmpty) {
      return Array[AnyRef](text, new Array[Boolean](0))
    }
    patches = patch_deepCopy(patches)
    val nullPadding: String = patch_addPadding(patches)
    text = nullPadding + text + nullPadding
    patch_splitMax(patches)
    var x: Int = 0
    var delta: Int = 0
    val results: Array[Boolean] = new Array[Boolean](patches.size)
    import scala.collection.JavaConversions._
    for (aPatch <- patches) {
      val expected_loc: Int = aPatch.start2 + delta
      val text1: String = diff_text1(aPatch.diffs)
      var start_loc: Int = 0
      var end_loc: Int = -1
      if (text1.length > this.Match_MaxBits) {
        start_loc = match_main(text, text1.substring(0, this.Match_MaxBits), expected_loc)
        if (start_loc != -1) {
          end_loc = match_main(text, text1.substring(text1.length - this.Match_MaxBits), expected_loc + text1.length - this.Match_MaxBits)
          if (end_loc == -1 || start_loc >= end_loc) {
            start_loc = -1
          }
        }
      }
      else {
        start_loc = match_main(text, text1, expected_loc)
      }
      if (start_loc == -1) {
        results(x) = false
        delta -= aPatch.length2 - aPatch.length1
      }
      else {
        results(x) = true
        delta = start_loc - expected_loc
        var text2: String = null
        if (end_loc == -1) {
          text2 = text.substring(start_loc, Math.min(start_loc + text1.length, text.length))
        }
        else {
          text2 = text.substring(start_loc, Math.min(end_loc + this.Match_MaxBits, text.length))
        }
        if (text1 == text2) {
          text = text.substring(0, start_loc) + diff_text2(aPatch.diffs) + text.substring(start_loc + text1.length)
        }
        else {
          val diffs: LinkedList[diff_match_patch.Diff] = diff_main(text1, text2, false)
          if (text1.length > this.Match_MaxBits && diff_levenshtein(diffs) / text1.length.asInstanceOf[Float] > this.Patch_DeleteThreshold) {
            results(x) = false
          }
          else {
            diff_cleanupSemanticLossless(diffs)
            var index1: Int = 0
            import scala.collection.JavaConversions._
            for (aDiff <- aPatch.diffs) {
              if (aDiff.operation ne Operation.EQUAL) {
                val index2: Int = diff_xIndex(diffs, index1)
                if (aDiff.operation eq Operation.INSERT) {
                  text = text.substring(0, start_loc + index2) + aDiff.text + text.substring(start_loc + index2)
                }
                else if (aDiff.operation eq Operation.DELETE) {
                  text = text.substring(0, start_loc + index2) + text.substring(start_loc + diff_xIndex(diffs, index1 + aDiff.text.length))
                }
              }
              if (aDiff.operation ne Operation.DELETE) {
                index1 += aDiff.text.length
              }
            }
          }
        }
      }
      x += 1
    }
    text = text.substring(nullPadding.length, text.length - nullPadding.length)
    return Array[AnyRef](text, results)
  }

  /**
   * Add some padding on text start and end so that edges can match something.
   * Intended to be called only from within patch_apply.
   * @param patches Array of Patch objects.
   * @return The padding string added to each side.
   */
  def patch_addPadding(patches: LinkedList[diff_match_patch.Patch]): String = {
    val paddingLength: Short = this.Patch_Margin
    var nullPadding: String = ""
    {
      var x: Short = 1
      while (x <= paddingLength) {
        {
          nullPadding += String.valueOf(x.asInstanceOf[Char])
        }
        ({
          x += 1; x - 1
        })
      }
    }
    import scala.collection.JavaConversions._
    for (aPatch <- patches) {
      aPatch.start1 += paddingLength
      aPatch.start2 += paddingLength
    }
    var patch: diff_match_patch.Patch = patches.getFirst
    var diffs: LinkedList[diff_match_patch.Diff] = patch.diffs
    if (diffs.isEmpty || diffs.getFirst.operation ne Operation.EQUAL) {
      diffs.addFirst(new diff_match_patch.Diff(Operation.EQUAL, nullPadding))
      patch.start1 -= paddingLength
      patch.start2 -= paddingLength
      patch.length1 += paddingLength
      patch.length2 += paddingLength
    }
    else if (paddingLength > diffs.getFirst.text.length) {
      val firstDiff: diff_match_patch.Diff = diffs.getFirst
      val extraLength: Int = paddingLength - firstDiff.text.length
      firstDiff.text = nullPadding.substring(firstDiff.text.length) + firstDiff.text
      patch.start1 -= extraLength
      patch.start2 -= extraLength
      patch.length1 += extraLength
      patch.length2 += extraLength
    }
    patch = patches.getLast
    diffs = patch.diffs
    if (diffs.isEmpty || diffs.getLast.operation ne Operation.EQUAL) {
      diffs.addLast(new diff_match_patch.Diff(Operation.EQUAL, nullPadding))
      patch.length1 += paddingLength
      patch.length2 += paddingLength
    }
    else if (paddingLength > diffs.getLast.text.length) {
      val lastDiff: diff_match_patch.Diff = diffs.getLast
      val extraLength: Int = paddingLength - lastDiff.text.length
      lastDiff.text += nullPadding.substring(0, extraLength)
      patch.length1 += extraLength
      patch.length2 += extraLength
    }
    return nullPadding
  }

  /**
   * Look through the patches and break up any which are longer than the
   * maximum limit of the match algorithm.
   * Intended to be called only from within patch_apply.
   * @param patches LinkedList of Patch objects.
   */
  def patch_splitMax(patches: LinkedList[diff_match_patch.Patch]) {
    val patch_size: Short = Match_MaxBits
    var precontext: String = null
    var postcontext: String = null
    var patch: diff_match_patch.Patch = null
    var start1: Int = 0
    var start2: Int = 0
    var empty: Boolean = false
    var diff_type: diff_match_patch.Operation = null
    var diff_text: String = null
    val pointer: ListIterator[diff_match_patch.Patch] = patches.listIterator
    var bigpatch: diff_match_patch.Patch = if (pointer.hasNext) pointer.next else null
    while (bigpatch != null) {
      if (bigpatch.length1 <= Match_MaxBits) {
        bigpatch = if (pointer.hasNext) pointer.next else null
        continue //todo: continue is not supported
      }
      pointer.remove
      start1 = bigpatch.start1
      start2 = bigpatch.start2
      precontext = ""
      while (!bigpatch.diffs.isEmpty) {
        patch = new diff_match_patch.Patch
        empty = true
        patch.start1 = start1 - precontext.length
        patch.start2 = start2 - precontext.length
        if (precontext.length != 0) {
          patch.length1 = ({
            patch.length2 = precontext.length; patch.length2
          })
          patch.diffs.add(new diff_match_patch.Diff(Operation.EQUAL, precontext))
        }
        while (!bigpatch.diffs.isEmpty && patch.length1 < patch_size - Patch_Margin) {
          diff_type = bigpatch.diffs.getFirst.operation
          diff_text = bigpatch.diffs.getFirst.text
          if (diff_type eq Operation.INSERT) {
            patch.length2 += diff_text.length
            start2 += diff_text.length
            patch.diffs.addLast(bigpatch.diffs.removeFirst)
            empty = false
          }
          else if (diff_type eq Operation.DELETE && patch.diffs.size == 1 && patch.diffs.getFirst.operation eq Operation.EQUAL && diff_text.length > 2 * patch_size) {
            patch.length1 += diff_text.length
            start1 += diff_text.length
            empty = false
            patch.diffs.add(new diff_match_patch.Diff(diff_type, diff_text))
            bigpatch.diffs.removeFirst
          }
          else {
            diff_text = diff_text.substring(0, Math.min(diff_text.length, patch_size - patch.length1 - Patch_Margin))
            patch.length1 += diff_text.length
            start1 += diff_text.length
            if (diff_type eq Operation.EQUAL) {
              patch.length2 += diff_text.length
              start2 += diff_text.length
            }
            else {
              empty = false
            }
            patch.diffs.add(new diff_match_patch.Diff(diff_type, diff_text))
            if (diff_text == bigpatch.diffs.getFirst.text) {
              bigpatch.diffs.removeFirst
            }
            else {
              bigpatch.diffs.getFirst.text = bigpatch.diffs.getFirst.text.substring(diff_text.length)
            }
          }
        }
        precontext = diff_text2(patch.diffs)
        precontext = precontext.substring(Math.max(0, precontext.length - Patch_Margin))
        if (diff_text1(bigpatch.diffs).length > Patch_Margin) {
          postcontext = diff_text1(bigpatch.diffs).substring(0, Patch_Margin)
        }
        else {
          postcontext = diff_text1(bigpatch.diffs)
        }
        if (postcontext.length != 0) {
          patch.length1 += postcontext.length
          patch.length2 += postcontext.length
          if (!patch.diffs.isEmpty && patch.diffs.getLast.operation eq Operation.EQUAL) {
            patch.diffs.getLast.text += postcontext
          }
          else {
            patch.diffs.add(new diff_match_patch.Diff(Operation.EQUAL, postcontext))
          }
        }
        if (!empty) {
          pointer.add(patch)
        }
      }
      bigpatch = if (pointer.hasNext) pointer.next else null
    }
  }

  /**
   * Take a list of patches and return a textual representation.
   * @param patches List of Patch objects.
   * @return Text representation of patches.
   */
  def patch_toText(patches: List[diff_match_patch.Patch]): String = {
    val text: StringBuilder = new StringBuilder
    import scala.collection.JavaConversions._
    for (aPatch <- patches) {
      text.append(aPatch)
    }
    return text.toString
  }

  /**
   * Parse a textual representation of patches and return a List of Patch
   * objects.
   * @param textline Text representation of patches.
   * @return List of Patch objects.
   * @throws IllegalArgumentException If invalid input.
   */
  def patch_fromText(textline: String): List[diff_match_patch.Patch] = {
    val patches: List[diff_match_patch.Patch] = new LinkedList[diff_match_patch.Patch]
    if (textline.length == 0) {
      return patches
    }
    val textList: List[String] = Arrays.asList(textline.split("\n"))
    val text: LinkedList[String] = new LinkedList[String](textList)
    var patch: diff_match_patch.Patch = null
    val patchHeader: Pattern = Pattern.compile("^@@ -(\\d+),?(\\d*) \\+(\\d+),?(\\d*) @@$")
    var m: Matcher = null
    var sign: Char = 0
    var line: String = null
    while (!text.isEmpty) {
      m = patchHeader.matcher(text.getFirst)
      if (!m.matches) {
        throw new IllegalArgumentException("Invalid patch string: " + text.getFirst)
      }
      patch = new diff_match_patch.Patch
      patches.add(patch)
      patch.start1 = Integer.parseInt(m.group(1))
      if (m.group(2).length == 0) {
        patch.start1 -= 1
        patch.length1 = 1
      }
      else if (m.group(2) == "0") {
        patch.length1 = 0
      }
      else {
        patch.start1 -= 1
        patch.length1 = Integer.parseInt(m.group(2))
      }
      patch.start2 = Integer.parseInt(m.group(3))
      if (m.group(4).length == 0) {
        patch.start2 -= 1
        patch.length2 = 1
      }
      else if (m.group(4) == "0") {
        patch.length2 = 0
      }
      else {
        patch.start2 -= 1
        patch.length2 = Integer.parseInt(m.group(4))
      }
      text.removeFirst
      while (!text.isEmpty) {
        try {
          sign = text.getFirst.charAt(0)
        }
        catch {
          case e: IndexOutOfBoundsException => {
            text.removeFirst
            continue //todo: continue is not supported
          }
        }
        line = text.getFirst.substring(1)
        line = line.replace("+", "%2B")
        try {
          line = URLDecoder.decode(line, "UTF-8")
        }
        catch {
          case e: UnsupportedEncodingException => {
            throw new Error("This system does not support UTF-8.", e)
          }
          case e: IllegalArgumentException => {
            throw new IllegalArgumentException("Illegal escape in patch_fromText: " + line, e)
          }
        }
        if (sign == '-') {
          patch.diffs.add(new diff_match_patch.Diff(Operation.DELETE, line))
        }
        else if (sign == '+') {
          patch.diffs.add(new diff_match_patch.Diff(Operation.INSERT, line))
        }
        else if (sign == ' ') {
          patch.diffs.add(new diff_match_patch.Diff(Operation.EQUAL, line))
        }
        else if (sign == '@') {
          break //todo: break is not supported
        }
        else {
          throw new IllegalArgumentException("Invalid patch mode '" + sign + "' in: " + line)
        }
        text.removeFirst
      }
    }
    return patches
  }

  /**
   * Number of seconds to map a diff before giving up (0 for infinity).
   */
  var Diff_Timeout: Float = 1.0f
  /**
   * Cost of an empty edit operation in terms of edit characters.
   */
  var Diff_EditCost: Short = 4
  /**
   * At what point is no match declared (0.0 = perfection, 1.0 = very loose).
   */
  var Match_Threshold: Float = 0.5f
  /**
   * How far to search for a match (0 = exact location, 1000+ = broad match).
   * A match this many characters away from the expected location will add
   * 1.0 to the score (0.0 is a perfect match).
   */
  var Match_Distance: Int = 1000
  /**
   * When deleting a large block of text (over ~64 characters), how close do
   * the contents have to be to match the expected contents. (0.0 = perfection,
   * 1.0 = very loose).  Note that Match_Threshold controls how closely the
   * end points of a delete need to match.
   */
  var Patch_DeleteThreshold: Float = 0.5f
  /**
   * Chunk size for context length.
   */
  var Patch_Margin: Short = 4
  /**
   * The number of bits in an int.
   */
  private var Match_MaxBits: Short = 32
  private var BLANKLINEEND: Pattern = Pattern.compile("\\n\\r?\\n\\Z", Pattern.DOTALL)
  private var BLANKLINESTART: Pattern = Pattern.compile("\\A\\r?\\n\\r?\\n", Pattern.DOTALL)
}



