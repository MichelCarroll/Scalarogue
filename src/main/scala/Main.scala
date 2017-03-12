
import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js.annotation.JSExport


@JSExport
object Main {

  @JSExport
  def main(viewportCanvas: html.Canvas, minimapCanvas: html.Canvas, messagesList: html.UList, messageContainer: html.Div): Unit = {

    val game = new Game(GameDisplayAdapter(viewportCanvas, minimapCanvas, messagesList, messageContainer))

    dom.document.onkeydown = (e: dom.KeyboardEvent) => {
      Command.fromKeyCode(e.keyCode).foreach(game.executeTurn)
    }

  }
}