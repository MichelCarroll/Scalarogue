package game


import game.RelationshipWithUser.{Himself, HisBelonging, Other}
import game.being.{BodyEffect, BodyPartDamaged, BodyPartDestroyed, Damage}

sealed trait RelationshipWithUser {
  import game.RelationshipWithUser._
  val isThirdPerson = this match {
    case Himself => false
    case _ => true
  }
}
object RelationshipWithUser {
  case object Himself extends RelationshipWithUser
  case object HisBelonging extends RelationshipWithUser
  case object Other extends RelationshipWithUser
}

trait Named { self =>
  val name: String
  def ownedBy(owner: Describable) = new Describable {
    override val relationshipWithUser: RelationshipWithUser = owner.relationshipWithUser match {
      case Other => Other
      case _ => HisBelonging
    }
    override val name: String = self.name
  }
}

trait Describable extends Named {
  import game.RelationshipWithUser._

  val relationshipWithUser: RelationshipWithUser
  def subject = relationshipWithUser match {
    case Himself => "you"
    case HisBelonging => s"your $name"
    case Other => s"the $name"
  }
}

trait Notification {
  val message: String
}

case class OwnedItem(relationshipWithUser: RelationshipWithUser, item: Item) extends Describable {
  val name = item.name
}

case class TargetTaken(by: Describable, item: Item) extends Notification {
  val verb = if(by.relationshipWithUser.isThirdPerson) "picks up" else "pick up"
  val message = s"${by.subject} $verb ${item.amount} ${item.name}"
}

case class TargetOpened(by: Describable, target: Describable) extends Notification {
  val verb = if(by.relationshipWithUser.isThirdPerson) "opens" else "open"
  val message = s"${by.subject} $verb ${target.subject}"
}

case class TargetHit(by: Describable, target: Describable, bodyEffectOpt: Option[BodyEffect]) extends Notification {
  val verb = if(by.relationshipWithUser.isThirdPerson) "hits" else "hit"
  val prefixMessage = s"${by.subject} $verb ${target.subject}"

  val message = bodyEffectOpt match {
    case Some(BodyPartDamaged(bodyPart, damage)) =>
      s"$prefixMessage, and ${bodyPart.ownedBy(target).subject} received ${damage.value} damage"
    case Some(BodyPartDestroyed(bodyPart, damage)) =>
      s"$prefixMessage, and ${bodyPart.ownedBy(target).subject} received ${damage.value} damage, and got destroyed!"
    case None =>
      s"$prefixMessage, and has no effect!"
}
}

case class TargetDies(target: Describable) extends Notification {
  val verb = if(target.relationshipWithUser.isThirdPerson) "dies" else "die"
  val message = s"${target.subject} $verb!"
}

case class TargetDropsItem(by: Describable, item: Item) extends Notification {
  val verb = if(by.relationshipWithUser.isThirdPerson) "drops" else "drop"
  val message = s"${by.subject} $verb ${item.amount} ${item.name}!"
}