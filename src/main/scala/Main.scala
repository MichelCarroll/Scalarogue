
import org.scalajs.dom
import org.scalajs.dom.ext.Color
import org.scalajs.dom.html

import scala.scalajs.js.annotation.JSExport
import scala.concurrent.ExecutionContext.Implicits.global


class NotificationContext(messagesList: html.UList, messageContainer: html.Div) {

  def notify(text: String, color: Color): Unit = {
    val message = dom.document.createElement("li")
    message.textContent = text
    message.setAttribute("style", s"color: ${color.toString}")
    messagesList.appendChild(message)
    messageContainer.scrollTop = messageContainer.scrollHeight
  }
}

@JSExport
object Main {

  @JSExport
  def main(viewportCanvas: html.Canvas, minimapCanvas: html.Canvas, messagesList: html.UList, messageContainer: html.Div): Unit = {

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

    val (initialGameState, newRng) = GameState.start(SimpleRNG(1512512))
    var gameState = initialGameState

    mainViewportDrawingContext.ready.map(_ =>
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
          postAITransition._1.foreach(notification => notificationContext.notify(notification.message, Color.White))
        })

      gameState.dungeon.positionedPlayer.foreach(positionedPlayer => {
        redraw(gameState, positionedPlayer)
      })
    }

    def redraw(gameState: GameState, positionedPlayer: PositionedBeing): Unit = {
      mainViewportDrawingContext.drawFromPerspective(gameState, positionedPlayer.position)
      minimapDrawingContext.draw(gameState, positionedPlayer.position)
    }


  }
}