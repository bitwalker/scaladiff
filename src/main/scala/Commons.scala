package net.ironforged.scaladiff

object Commons {
  /**
   * Increment an integer value
   */
  def inc(x: Int): Int = x + 1

  /**
   * Decrement an integer value
   */
  def dec(x: Int): Int = x - 1

  /**
   * Insert an element within a list
   * @param xs The target list
   * @param index The index at which to insert the new element
   * @param element The new element to insert
   * @tparam T The type of the list
   * @return List[T]
   */
  def insert[T](xs: List[T], index: Int, element: T): List[T] = {
    (xs.take(index) :+ element) ++ xs.drop(index)
  }

  /**
   * Insert multiple elements within a list
   * @param xs The target list
   * @param index The index at which to insert the new element
   * @param elements The list of elements to insert
   * @tparam T The type of the list
   * @return List[T]
   */
  def insert[T](xs: List[T], index: Int, elements: => List[T]): List[T] = {
    xs.take(index) ++ elements ++ xs.drop(index)
  }

  /**
   * Replace elements within a list with a single element
   * @param xs The target list
   * @param start The index to begin the replacement
   * @param end The index at which to end replacment
   * @param element The element to swap in
   * @tparam T The type of the list
   * @return List[T]
   */
  def replace[T](xs: List[T], start: Int, end: Int, element: T): List[T] = {
    (xs.take(start) :+ element) ++ xs.drop(end)
  }

  /**
   * Replace elements within a list with another list of elements
   * @param xs The target list
   * @param start The index to begin replacement
   * @param end The index at which to end replacement
   * @param elements The elements to swap in
   * @tparam T The type of the list
   * @return List[T]
   */
  def replace[T](xs: List[T], start: Int, end: Int, elements: List[T]): List[T] = {
    xs.take(start) ++ elements ++ xs.drop(end)
  }

  /**
   * A safe substring operation that starts at the beginning of the string and selects towards the end
   * @param s The source string
   * @param x The number of characters to select
   * @return String
   */
  def sliceLeft(s: String, x: Int): String = {
    if (s == null)          return ""
    else if (x <= 0)        return ""
    else if (x > s.length)  return s
    else                    s.substring(0, x)
  }

  /**
   * A safe substring operation that starts at the end of the string and selects towards the beginning
   * @param s The source string
   * @param x The number of characters to select
   * @return String
   */
  def sliceRight(s: String, x: Int): String = {
    if (s == null)         return ""
    else if (x <= 0)       return ""
    else if (x > s.length) return s
    else                   s.substring(s.length - x)
  }
}
