package net.ironforged.scaladiff

object OperationType extends Enum {
  case object Insert extends OperationType.Value; Insert
  case object Delete extends OperationType.Value; Delete
  case object Equals extends OperationType.Value; Equals
}

case class Operation(op: OperationType.Value, text: String)
