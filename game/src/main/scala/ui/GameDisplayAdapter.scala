package ui

import game.GameState
import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js

/**
  * Created by MichelCarroll on 3/28/2017.
  */
case class GameDisplayAdapter(
                               viewportCanvas: html.Canvas,
                               minimapCanvas: html.Canvas,
                               onUpdateState: js.Function1[Any, Any]
                             ) {
  val mainViewportDrawingContext = new MainViewportDrawingContext(
    renderingContext = viewportCanvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  )

  val minimapDrawingContext = new MinimapViewportDrawingContext(
    renderingContext = minimapCanvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  )

  def updateState(state: GameState) = {
    val gameState = js.Dictionary(
      "notifications" -> js.Array(state.notificationHistory.map(_.message): _*),
      "stats" -> js.Dictionary(
        "health" -> js.Dictionary(
          "max" -> state.dungeon.player.map(_.body.fullHealth.value).getOrElse(1.0),
          "current" -> state.dungeon.player.map(_.body.health.value).getOrElse(0.0)
        )
      )
    )
    onUpdateState(gameState)
  }
}