package net.ironforged.scaladiff

import java.io.UnsupportedEncodingException
import java.net.URLEncoder
import java.util.LinkedList

import net.ironforged.scaladiff.helpers._
import net.ironforged.scaladiff.Operation._

/**
 * Class representing one patch operation.
 */
case class Patch(diffs: LinkedList[Diff], start1: Int, length1: Int, start2: Int, length2: Int) {
  /**
   * Emulate GNU diff's format.
   * Header: @@ -382,8 +481,9 @@
   * Indicies are printed as 1-based, not 0-based.
   * @return The GNU diff string.
   */
  override def toString(): String = {
    val coords1 = length1 match {
      case 0 => start1 + ",0"
      case 1 => (start1 + 1).toString
      case _ => (start1 + 1) + "," + length1
    }

    val coords2 = length2 match {
      case 0 => start2 + ",0"
      case 1 => (start2 + 1).toString
      case _ => (start2 + 1) + "," + length2
    }

    val text = new StringBuilder()
    text.append("@@ -")
        .append(coords1)
        .append(" +")
        .append(coords2)
        .append(" @@\n")

    for (diff <- diffs) {
      diff.operation match {
        case Insert => text.append('+')
        case Delete => text.append('-')
        case Equal =>  text.append(' ')
      }

      try {
        text.append(URLEncoder.encode(diff.text, "UTF-8").replace('+', ' ')).append('\n')
      }
      catch {
        case e: UnsupportedEncodingException => throw new Error("This system does not support UTF-8.", e)
      }
    }
    Strings.unescapeForEncodeUriCompatability(text.toString)
  }
}

object Patch {

}
