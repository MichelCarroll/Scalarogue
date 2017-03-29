package ui

import org.scalajs.dom
import org.scalajs.dom.html

/**
  * Created by MichelCarroll on 3/28/2017.
  */
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
