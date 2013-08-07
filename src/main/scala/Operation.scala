package net.ironforged.scaladiff

/**
 * The data structure representing a diff is a LinkedList of Diff objects:
 * {
 *    Diff(Operation.Delete, "Hello"),
 *    Diff(Operation.Insert, "Goodbye"),
 *    Diff(Operation.Equal, " world.")
 * }
 * which means: delete "Hello", add "Goodbye" and keep " world."
 */
object Operation extends Enum {
  case object Delete extends Operation.Value; Delete
  case object Insert extends Operation.Value; Delete
  case object Equal  extends Operation.Value; Delete
}
