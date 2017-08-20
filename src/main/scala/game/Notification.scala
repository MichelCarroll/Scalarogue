package game


import game.being.{BodyEffect, BodyDamaged, BodyDestroyed, Damage}


trait Named { self =>
  val name: String

  def ownedBy(o: Describable): Describable = new Describable {
    override val isProtagonist = false
    override val owner = Some(o)
    override val name = self.name
  }
}

trait Describable extends Named {
  val isProtagonist: Boolean = false
  val owner: Option[Describable] = None

  def subject = owner match {
    case Some(o) if o.isProtagonist => s"your $name"
    case Some(o) => s"the ${o.name}'s $name"
    case None if isProtagonist => s"you"
    case None => s"the $name"
  }

  def subjectUsingPronouns = owner match {
    case Some(o) if o.isProtagonist => s"your $name"
    case Some(o) => s"his $name"
    case None if isProtagonist => s"you"
    case None => s"he"
  }
}

trait Notification {
  val message: String
}

case class TargetTaken(by: Describable, item: Item) extends Notification {
  val verb = if(by.isProtagonist) "pick up" else "picks up"
  val message = s"${by.subject} $verb ${item.amount} ${item.name}"
}

case class TargetOpened(by: Describable, target: Describable) extends Notification {
  val verb = if(by.isProtagonist) "open" else "opens"
  val message = s"${by.subject} $verb ${target.subject}"
}

case class TargetHit(by: Describable, target: Describable, bodyEffectOpt: Option[BodyEffect]) extends Notification {
  val verb = if(by.isProtagonist) "hit" else "hits"
  val prefixMessage = s"${by.subject} $verb ${target.subject}"

  val message = bodyEffectOpt match {
    case Some(BodyDamaged(damage)) =>
      s"$prefixMessage, ${target.subjectUsingPronouns} received ${damage.value} damage"
    case Some(BodyDestroyed(damage)) =>
      s"$prefixMessage, ${target.subjectUsingPronouns} received ${damage.value} damage, and it got destroyed!"
    case None =>
      s"$prefixMessage, and has no effect"
  }
}

case class TargetDies(target: Describable) extends Notification {
  val verb = if(target.isProtagonist) "die" else "dies"
  val message = s"${target.subject} $verb!"
}

case class TargetDropsItem(by: Describable, item: Item) extends Notification {
  val verb = if(by.isProtagonist) "drop" else "drops"
  val message = s"${by.subject} $verb ${item.amount} ${item.name}!"
}