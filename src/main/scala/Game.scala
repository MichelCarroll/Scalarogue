import math.SimpleRNG
import org.scalajs.dom
import org.scalajs.dom.ext.Color
import org.scalajs.dom.html

import scala.concurrent.ExecutionContext.Implicits.global

case class GameDisplayAdapter(
                               viewportCanvas: html.Canvas,
                               minimapCanvas: html.Canvas,
                               messagesList: html.UList,
                               messageContainer: html.Div
                             ) {
  val mainViewportDrawingContext = new MainViewportDrawingContext(
    renderingContext = viewportCanvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  )

  val minimapDrawingContext = new MinimapViewportDrawingContext(
    renderingContext = minimapCanvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  )

  val notificationContext = new NotificationContext(
    messagesList = messagesList,
    messageContainer = messageContainer
  )
}

/**
  * Created by MichelCarroll on 3/12/2017.
  */
class Game(seed: Int, displayAdapter: GameDisplayAdapter) {


  private val (initialGameState, newRng) = GameState.start(SimpleRNG(seed))
  private var gameState = initialGameState

  displayAdapter.mainViewportDrawingContext.ready.map(_ =>
    gameState.dungeon.positionedPlayer.foreach(positionedPlayer =>
      redraw(gameState, positionedPlayer)
    )
  )

  dom.document.onkeydown = (e: dom.KeyboardEvent) => {
    Command.fromKeyCode(e.keyCode).foreach(executeTurn)
  }

  def executeTurn(playerCommand: Command) = {

    gameState.dungeon.positionedPlayer.foreach(positionedPlayer => {
      val transition = gameState.applyCommand(positionedPlayer, playerCommand)
      val postAITransition = transition.newState.dungeon.positionedBeings(Spider)
        .foldLeft((transition.notifications, transition.newState))((last, positionedBeing) => {
          val ((commandOpt, newBeing), newRng) = positionedBeing.being.withNextCommand(positionedBeing.position, last._2.dungeon)(last._2.rng)
          val updatedDungeon = last._2.dungeon.withUpdatedBeing(PositionedBeing(positionedBeing.position, newBeing))
          val newGameState = GameState(updatedDungeon, newRng)

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

    gameState.dungeon.positionedPlayer.foreach(positionedPlayer => {
      redraw(gameState, positionedPlayer)
    })
  }

  private def redraw(gameState: GameState, positionedPlayer: PositionedBeing): Unit = {
    displayAdapter.mainViewportDrawingContext.drawFromPerspective(gameState, positionedPlayer.position)
    displayAdapter.minimapDrawingContext.draw(gameState, positionedPlayer.position)
  }

}
