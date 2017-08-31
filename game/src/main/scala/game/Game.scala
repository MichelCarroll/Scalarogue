package game

import dungeon.Cell
import game.being.{Player, Spider}
import math.Position
import org.scalajs.dom
import org.scalajs.dom.ext.Color
import random.{RNG, SimpleRNG}
import ui.GameDisplayAdapter
import com.softwaremill.quicklens._

/**
  * Created by MichelCarroll on 3/12/2017.
  */
class Game(seed: Long, displayAdapter: GameDisplayAdapter) {

  import scala.concurrent.ExecutionContext.Implicits.global

  private val (initialGameState, initialRng) = GameState.start(SimpleRNG(seed))
  private var rng = initialRng
  private var gameState = initialGameState

  displayAdapter.mainViewportDrawingContext.ready.map(_ =>
    gameState.dungeon.playerPosition.foreach(position => redraw(gameState, position))
  )

  dom.document.onkeydown = (e: dom.KeyboardEvent) => {
    Command.fromKeyCode(e.keyCode).foreach(executeTurn)
  }

  displayAdapter.updateState(gameState)

  def executeTurn(playerCommand: Command) = {

    def withProcessedPlayerTurn(playerPosition: Position): GameState =
      gameState.actionTargetMapping(playerPosition).get(playerCommand) match {
        case Some(actionTarget) =>
          val (outcome, newRng) = RNG.nextInWeightedSet(actionTarget)(rng)
          rng = newRng
          outcome
            .foldLeft(gameState)(_.materialize(_))
            .withUpdatedRevealedPositions

        case None => gameState
      }


    def withProcessedEnemyTurns: GameState =
      gameState.dungeon
        .beingOfTypePositions(Spider)
        .foldLeft(gameState)((lastState, beingPosition) => lastState.dungeon.cells(beingPosition) match {
          case Cell(Some(being), _, _) =>

            val (commandOpt, newRng) = being.intelligence.nextCommand(beingPosition, lastState.dungeon)(rng)
            this.rng = newRng

            commandOpt.flatMap(lastState.actionTargetMapping(beingPosition).get) match {
              case Some(actionTarget) =>
                val (outcome, newRng) = RNG.nextInWeightedSet(actionTarget)(rng)
                rng = newRng
                outcome.foldLeft(lastState)(_.materialize(_))

              case None => lastState
            }
          case _ => lastState
        })

    gameState.dungeon.playerPosition.foreach(playerPosition => {
      gameState = withProcessedPlayerTurn(playerPosition)
      gameState = withProcessedEnemyTurns
    })

    gameState.dungeon.playerPosition.foreach(redraw(gameState, _))
    displayAdapter.updateState(gameState)
  }

  private def redraw(gameState: GameState, cameraPosition: Position): Unit = {
    displayAdapter.mainViewportDrawingContext.drawFromPerspective(gameState, cameraPosition)
    displayAdapter.minimapDrawingContext.draw(gameState, cameraPosition)
  }

}
