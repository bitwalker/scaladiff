package net.ironforged.scaladiff

/**
 * Internal class for returning results from diff_linesToChars().
 * Other less paranoid languages just use a three-element array.
 */
protected case class LinesToCharsResult(chars1: String, chars2: String, lines: List[String])
