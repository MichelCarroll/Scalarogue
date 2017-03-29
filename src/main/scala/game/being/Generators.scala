package game.being

import game.being.ai.{NoIntelligence, SimpleAgroIntelligence}


case object SpiderGenerator {
  def generate = Being(Spider, Spider.maxHealth, SimpleAgroIntelligence(maxRange = 4))
}

case object PlayerGenerator {
  def generate = Being(Player, Player.maxHealth, NoIntelligence())
}
