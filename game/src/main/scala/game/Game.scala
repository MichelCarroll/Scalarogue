package game

import dungeon.Cell
import game.being.{Player, Spider}
import math.Position
import org.scalajs.dom
import org.scalajs.dom.ext.Color
import random.SimpleRNG
import ui.GameDisplayAdapter
import com.softwaremill.quicklens._

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

      gameState = gameState.applyCommand(playerPosition, playerCommand)

      gameState = gameState.dungeon.playerPosition match {
        case Some(position) =>
          gameState
            .modify(_.revealedPositions)
            .using(_ ++ Player.positionsWithinRangeTouchedByPerimeterRay(position, gameState.dungeon))
        case _ => gameState
      }

      gameState = gameState.dungeon
        .beingOfTypePositions(Spider)
        .foldLeft(gameState)((last, beingPosition) => last.dungeon.cells(beingPosition) match {
          case Cell(Some(being), _, _) =>
            val (commandOpt, newRng) = being.intelligence.nextCommand(beingPosition, last.dungeon)(last.rng)
            val newGameState = GameState(last.dungeon, newRng, last.revealedPositions, last.notificationHistory)

            commandOpt match {
              case Some(command) => newGameState.applyCommand(beingPosition, command)
              case None => newGameState
            }
          case _ => last
        })

    })

    gameState.dungeon.playerPosition.foreach(position => redraw(gameState, position))
    displayAdapter.updateState(gameState)
  }

  private def redraw(gameState: GameState, cameraPosition: Position): Unit = {
    displayAdapter.mainViewportDrawingContext.drawFromPerspective(gameState, cameraPosition)
    displayAdapter.minimapDrawingContext.draw(gameState, cameraPosition)
  }

}
