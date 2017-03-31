package game


trait Describable {
  def name: String
  def pronoun: String
}

case class Notification(message: String)