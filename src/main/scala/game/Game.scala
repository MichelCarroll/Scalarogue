package game

import game.being.Spider
import math.Position
import org.scalajs.dom
import org.scalajs.dom.ext.Color
import random.SimpleRNG
import ui.GameDisplayAdapter


/**
  * Created by MichelCarroll on 3/12/2017.
  */
class Game(seed: Int, displayAdapter: GameDisplayAdapter) {

  import scala.concurrent.ExecutionContext.Implicits.global

  private val (initialGameState, newRng) = GameState.start(SimpleRNG(seed))
  private var gameState = initialGameState

  displayAdapter.mainViewportDrawingContext.ready.map(_ =>
    gameState.dungeon.positionedPlayer.foreach(positionedPlayer => redraw(gameState, positionedPlayer.position))
  )

  dom.document.onkeydown = (e: dom.KeyboardEvent) => {
    Command.fromKeyCode(e.keyCode).foreach(executeTurn)
  }

  def executeTurn(playerCommand: Command) = {

    gameState.dungeon.positionedPlayer.foreach(positionedPlayer => {
      val transition = gameState.applyCommand(positionedPlayer, playerCommand)
      val t2 = RefreshRevealedPositionsTransition(transition.newState)
      val postAITransition = t2.newState.dungeon.positionedBeings(Spider)
        .foldLeft((transition.notifications, t2.newState))((last, positionedBeing) => {
          val (commandOpt, newRng) = positionedBeing.being.intelligence.nextCommand(positionedBeing.position, last._2.dungeon)(last._2.rng)
          val newGameState = GameState(last._2.dungeon, newRng, last._2.revealedPositions)

          commandOpt match {
            case Some(command) =>
              val transition = newGameState.applyCommand(positionedBeing, command)
              (last._1 ++ transition.notifications, transition.newState)
            case None =>
              (last._1, newGameState)
          }
        })
      gameState = postAITransition._2
      postAITransition._1.foreach(notification => displayAdapter.notificationContext.notify(notification.message, Color.White))
    })

    gameState.dungeon.positionedPlayer.foreach(positionedPlayer => redraw(gameState, positionedPlayer.position))
  }

  private def redraw(gameState: GameState, cameraPosition: Position): Unit = {
    displayAdapter.mainViewportDrawingContext.drawFromPerspective(gameState, cameraPosition)
    displayAdapter.minimapDrawingContext.draw(gameState, cameraPosition)
  }

}
