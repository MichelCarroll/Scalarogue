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
  def ownedBy(ownerRelationship: RelationshipWithUser) = new Describable {
    override val relationshipWithUser: RelationshipWithUser = ownerRelationship match {
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

case class TargetHit(by: Describable, target: Describable, damage: Damage) extends Notification {
  val verb = if(by.relationshipWithUser.isThirdPerson) "hits" else "hit"
  val message = s"${by.subject} $verb ${target.subject} for ${damage.value} damage!"
}

case class TargetDies(target: Describable) extends Notification {
  val verb = if(target.relationshipWithUser.isThirdPerson) "dies" else "die"
  val message = s"${target.subject} $verb!"
}

case class TargetDropsItem(by: Describable, item: Item) extends Notification {
  val verb = if(by.relationshipWithUser.isThirdPerson) "drops" else "drop"
  val message = s"${by.subject} $verb ${item.amount} ${item.name}!"
}

case class TargetGetsBodyEffect(target: Describable, bodyEffect: BodyEffect) extends Notification {
  val message = bodyEffect match {
    case BodyPartDamaged(bodyPart, damage) => s"${bodyPart.ownedBy(target.relationshipWithUser).subject} received ${damage.value} damage"
    case BodyPartDestroyed(bodyPart, damage) => s"${bodyPart.ownedBy(target.relationshipWithUser).subject} received ${damage.value} damage, and got destroyed!"
  }
}