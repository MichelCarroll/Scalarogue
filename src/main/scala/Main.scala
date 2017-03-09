
import org.scalajs
import org.scalajs.dom
import org.scalajs.dom.ext.Color
import org.scalajs.dom.html
import org.w3c.dom.html.HTMLLIElement

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

    var gameState = GameState.start(1512512)

    def redraw(): Unit = {
      mainViewportDrawingContext.draw(gameState)
      minimapDrawingContext.draw(gameState)
    }

    dom.document.onkeydown = (e: dom.KeyboardEvent) => {
      val (notifications, newGameState) = PlayerCommand.fromKeyCode(e.keyCode)
        .map(gameState.applyPlayerCommand(_))
        .getOrElse((List(), gameState))
      gameState = newGameState
      notifications.foreach(notification => notificationContext.notify(notification.message, Color.White))
      redraw()
    }

    mainViewportDrawingContext.ready.map(_ => redraw())

  }
}