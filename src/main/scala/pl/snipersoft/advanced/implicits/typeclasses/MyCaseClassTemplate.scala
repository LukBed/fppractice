package pl.snipersoft.advanced.implicits.typeclasses

trait MyCaseClassTemplate[T] {
  def action(value: T): String
}

object MyCaseClassTemplate {
  def apply[T](implicit instance: MyCaseClassTemplate[T]) = instance
}