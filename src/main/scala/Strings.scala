package net.ironforged.scaladiff.helpers

object Strings {
  /**
   * Unescape selected chars for compatability with JavaScript's encodeURI.
   * In speed critical applications this could be dropped since the
   * receiving application will certainly decode these fine.
   * Note that this function is case-sensitive.  Thus "%3f" would not be
   * unescaped.  But this is ok because it is only called with the output of
   * URLEncoder.encode which returns uppercase hex.
   *
   * Example: "%3F" -> "?", "%24" -> "$", etc.
   *
   * @param str The string to escape.
   * @return The escaped string.
   */
  def unescapeForEncodeUriCompatability(str: String): String = {
    str.replace("%21", "!")
       .replace("%7E", "~")
       .replace("%27", "'")
       .replace("%28", "(")
       .replace("%29", ")")
       .replace("%3B", ";")
       .replace("%2F", "/")
       .replace("%3F", "?")
       .replace("%3A", ":")
       .replace("%40", "@")
       .replace("%26", "&")
       .replace("%3D", "=")
       .replace("%2B", "+")
       .replace("%24", "$")
       .replace("%2C", ",")
       .replace("%23", "#")
  }
}
