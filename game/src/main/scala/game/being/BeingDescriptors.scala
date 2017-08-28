package game.being

import game._
import game.being.ai.{IntelligenceFactory, NoIntelligence, SimpleAgroIntelligence}
import math.{Area, Position}
import random.RNG._

trait BeingDescriptor extends Describable {
  def drop: ItemBag
  def isThirdPerson = true
  val bodyFactory: BodyFactory
  val intelligenceFactory: IntelligenceFactory

  def randomNewBeing: Rand[Being] =
    bodyFactory.randomNewBody
      .combine(intelligenceFactory.randomNewIntelligence)
      .map {
        case (body, intelligence) => Being(this, body, intelligence, drop)
      }
}


case object Player extends BeingDescriptor with Sighted {

  override def isThirdPerson = false
  val name = "you"
  override val isProtagonist = true
  def drop = ItemBag(HealthPotion -> 5)

  private val viewportRange = 6
  val lineOfLightRange = Math.ceil(Math.sqrt(2 * Math.pow(viewportRange, 2)))

  def viewport(position: Position) = Area(
    Position(position.x - viewportRange + 1, position.y - viewportRange + 1),
    Position(position.x + viewportRange - 1, position.y + viewportRange - 1)
  )

  val bodyFactory = new BodyFactory {
    override def randomNewBody = new SimpleHumanoidGaussianBodyFactory(meanHealth = 20, variation = 3).randomNewBody
  }

  val intelligenceFactory = new IntelligenceFactory {
    override def randomNewIntelligence = unit(NoIntelligence)
  }

}


case object Spider extends BeingDescriptor {
  val name = "spider"
  def drop = ItemBag(Gold -> 5)

  val bodyFactory = new SimpleArachnoidGaussianBodyFactory(meanHealth = 5, variation = 3)

  val intelligenceFactory = new IntelligenceFactory {
    override def randomNewIntelligence = unit(SimpleAgroIntelligence(maxRange = 4))
  }

}