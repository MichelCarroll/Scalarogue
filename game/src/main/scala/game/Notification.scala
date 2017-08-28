package game




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

case class PotionDrank(by: Describable, potion: Potion) extends Notification {
  val verb = if(by.isProtagonist) "drink" else "drank"
  val message = s"${by.subject} $verb a ${potion.name}"
}

case class BeingAffected(by: Describable, beingEffect: BeingEffect) extends Notification {
  val verb = if(by.isProtagonist) "have been" else "has been"
  val message = s"${by.subject} $verb ${beingEffect.description}"
}

case class TargetTaken(by: Describable, amount: Int, item: Item) extends Notification {
  val verb = if(by.isProtagonist) "pick up" else "picks up"
  val message = s"${by.subject} $verb $amount ${item.name}"
}

case class TargetOpened(by: Describable, target: Describable) extends Notification {
  val verb = if(by.isProtagonist) "open" else "opens"
  val message = s"${by.subject} $verb ${target.subject}"
}

case class TargetHit(by: Describable, target: Describable, damage: Int) extends Notification {
  val verb = if(by.isProtagonist) "hit" else "hits"
  val prefixMessage = s"${by.subject} $verb ${target.subject}"

  val message = s"$prefixMessage, ${target.subjectUsingPronouns} received $damage damage"
}

case class TargetDies(target: Describable) extends Notification {
  val verb = if(target.isProtagonist) "die" else "dies"
  val message = s"${target.subject} $verb!"
}

case class TargetDropsItem(by: Describable, amount: Int, item: Item) extends Notification {
  val verb = if(by.isProtagonist) "drop" else "drops"
  val message = s"${by.subject} $verb $amount ${item.name}!"
}