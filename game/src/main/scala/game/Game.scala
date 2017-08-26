package game

import dungeon.OpenCell
import game.being.Spider
import math.Position
import org.scalajs.dom
import org.scalajs.dom.ext.Color
import random.SimpleRNG
import ui.GameDisplayAdapter


/**
  * Created by MichelCarroll on 3/12/2017.
  */
class Game(seed: Long, displayAdapter: GameDisplayAdapter) {

  import scala.concurrent.ExecutionContext.Implicits.global

  private val (initialGameState, newRng) = GameState.start(SimpleRNG(seed))
  private var gameState = initialGameState

  displayAdapter.mainViewportDrawingContext.ready.map(_ =>
    gameState.dungeon.playerPosition.foreach(position => redraw(gameState, position))
  )

  dom.document.onkeydown = (e: dom.KeyboardEvent) => {
    Command.fromKeyCode(e.keyCode).foreach(executeTurn)
  }

  displayAdapter.updateState(gameState)

  def executeTurn(playerCommand: Command) = {

    gameState.dungeon.playerPosition.foreach(playerPosition => {
      val transition = gameState.applyCommand(playerPosition, playerCommand)
      val t2 = RefreshRevealedPositionsTransition(transition.newState)
      val postAITransition = t2.newState.dungeon
        .beingOfTypePositions(Spider)
        .foldLeft(t2.newState)((last, beingPosition) => last.dungeon.cells(beingPosition) match {
          case OpenCell(Some(being), _, _) =>
            val (commandOpt, newRng) = being.intelligence.nextCommand(beingPosition, last.dungeon)(last.rng)
            val newGameState = GameState(last.dungeon, newRng, last.revealedPositions, last.notificationHistory)

            commandOpt match {
              case Some(command) =>
                val transition = newGameState.applyCommand(beingPosition, command)
                transition.newState
              case None =>
                newGameState
            }
          case _ => last
        })
      gameState = postAITransition
    })

    gameState.dungeon.playerPosition.foreach(position => redraw(gameState, position))
    displayAdapter.updateState(gameState)
  }

  private def redraw(gameState: GameState, cameraPosition: Position): Unit = {
    displayAdapter.mainViewportDrawingContext.drawFromPerspective(gameState, cameraPosition)
    displayAdapter.minimapDrawingContext.draw(gameState, cameraPosition)
  }

}
