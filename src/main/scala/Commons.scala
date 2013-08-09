package net.ironforged.scaladiff

object Commons {
  def inc(x: Int): Int = x + 1
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
   * @return List[T}
   */
  def insert[T](xs: List[T], index: Int, elements: => List[T]): List[T] = {
    xs.take(index) ++ elements ++ xs.drop(index)
  }
}
