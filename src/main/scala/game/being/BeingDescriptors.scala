package game.being

import game.{Gold, Items}
import game.being.ai.{IntelligenceFactory, NoIntelligence, SimpleAgroIntelligence}
import math.{Area, Position}
import random.RNG._
import random.SimpleRNG

trait BeingDescriptor {
  def name: String
  def pronoun: String
  def drop: Option[Items]
  def isThirdPerson = true
  val damageRange: DamageRange
  val bodyFactory: BodyFactory
  val intelligenceFactory: IntelligenceFactory

  def randomNewBeing: Rand[Being] = {
    println(name)
    println(bodyFactory.randomNewBody(SimpleRNG(125412)))
    println(intelligenceFactory.randomNewIntelligence(SimpleRNG(125412)))
    bodyFactory.randomNewBody
      .combine(intelligenceFactory.randomNewIntelligence)
      .map {
        case (body, intelligence) => {
          Being(this, body, intelligence)
        }
      }
  }
}

case object Player extends BeingDescriptor with Sighted {

  override def isThirdPerson = false
  val name = "you"
  val pronoun = "your"
  def drop = None
  val damageRange = DamageRange(Damage(2), Damage(4))

  private val viewportRange = 6
  val lineOfLightRange = Math.ceil(Math.sqrt(2 * Math.pow(viewportRange, 2)))

  def viewport(position: Position) = Area(
    Position(position.x - viewportRange + 1, position.y - viewportRange + 1),
    Position(position.x + viewportRange - 1, position.y + viewportRange - 1)
  )

  val bodyFactory = new BodyFactory {
    override def randomNewBody = unit(Body(Health(value = 20)))
  }

  val intelligenceFactory = new IntelligenceFactory {
    override def randomNewIntelligence = unit(NoIntelligence)
  }

}


case object Spider extends BeingDescriptor {
  def name = "a spider"
  def pronoun = "it"
  def drop = Some(Gold(5))
  val damageRange = DamageRange(Damage(1), Damage(2))

  val bodyFactory = new SimpleGaussianBodyFactory(meanHealth = 5, variation = 3)

  val intelligenceFactory = new IntelligenceFactory {
    override def randomNewIntelligence = unit(SimpleAgroIntelligence(maxRange = 4))
  }

}