package game.being

import game.{Named, Notification}
import random.RNG
import random.RNG._


trait Mortal {
  def dead: Boolean
}

trait Conscious {
  def unconscious: Boolean
}

trait BodyPart extends Damagable with Named

trait Damagable {
  val fullHealth: Health
  val health: Health
  val destroyed = health.value <= 0
  def damagePercentage: Double = health / fullHealth
}

trait Body extends Conscious with Mortal {
  def struckBy(damage: Damage): Rand[(Body, Option[BodyEffect])]
}

trait BodyFactory {
  def randomNewBody: Rand[Body]
}

class SimpleHumanoidGaussianBodyFactory(meanHealth: Int, variation: Int) extends BodyFactory {

  def randomNewBody: Rand[Body] = {

    def randomHealth = RNG.nextGaussianRatio(3)
      .map(ratio => Health((meanHealth - variation + variation * 2 * ratio).round.toInt))

    randomHealth.combine(randomHealth).combine(randomHealth)
      .combine(randomHealth).combine(randomHealth).combine(randomHealth)
      .map {
        case (((((h1, h2), h3), h4), h5), h6) =>
          HumanoidBody(
            leftArm = HumanoidArm(h1,h1),
            rightArm = HumanoidArm(h2,h2),
            leftLeg = HumanoidLeg(h3,h3),
            rightLeg = HumanoidLeg(h4,h4),
            torso = HumanoidTorso(h5,h5),
            head = HumanoidHead(h6,h6)
          )
      }

  }
}



sealed trait BodyEffect
case class BodyPartDamaged(bodyPart: BodyPart, damage: Damage) extends BodyEffect
case class BodyPartDestroyed(bodyPart: BodyPart, damage: Damage) extends BodyEffect

case class HumanoidBody(leftArm: HumanoidArm,
                         rightArm: HumanoidArm,
                         leftLeg: HumanoidLeg,
                         rightLeg: HumanoidLeg,
                         torso: HumanoidTorso,
                         head: HumanoidHead) extends Body {


    def dead: Boolean = head.destroyed || torso.destroyed
    def unconscious: Boolean = head.damagePercentage < 0.3
    def struckBy(damage: Damage) = {

      val undestroyedBodyParts = Set(
        (1, (leftArm, this.copy(leftArm = leftArm.damagedBy(damage)))),
        (1, (rightArm, this.copy(rightArm = rightArm.damagedBy(damage)))),
        (1, (leftLeg, this.copy(leftLeg = leftLeg.damagedBy(damage)))),
        (1, (rightLeg, this.copy(rightLeg = rightLeg.damagedBy(damage)))),
        (1, (head, this.copy(head = head.damagedBy(damage)))),
        (5, (torso, this.copy(torso = torso.damagedBy(damage))))
      ).filter(!_._2._1.destroyed)

      if(undestroyedBodyParts.isEmpty)
        unit((this, None))
      else
        RNG.nextInWeightedSet(undestroyedBodyParts)
          .map {
            case (bodyPart, newBody) =>
              if(bodyPart.destroyed)
                (newBody, Some(BodyPartDestroyed(bodyPart, damage)))
              else
                (newBody, Some(BodyPartDamaged(bodyPart, damage)))
          }
    }

}

case class HumanoidLeg(fullHealth: Health, health: Health) extends BodyPart {
  def damagedBy(damage: Damage) = this.copy(health = health - damage)
  val name = "leg"
}

case class HumanoidArm(fullHealth: Health, health: Health) extends BodyPart {
  def damagedBy(damage: Damage) = this.copy(health = health - damage)
  val name = "arm"
}

case class HumanoidTorso(fullHealth: Health, health: Health) extends BodyPart {
  def damagedBy(damage: Damage) = this.copy(health = health - damage)
  val name = "torso"
}

case class HumanoidHead(fullHealth: Health, health: Health) extends BodyPart {
  def damagedBy(damage: Damage) = this.copy(health = health - damage)
  val name = "head"
}
