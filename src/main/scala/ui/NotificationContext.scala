package ui

import org.scalajs.dom
import org.scalajs.dom.ext.Color
import org.scalajs.dom.html


class NotificationContext(messagesList: html.UList, messageContainer: html.Div) {

  def notify(text: String, color: Color): Unit = {
    val message = dom.document.createElement("li")
    message.textContent = text
    message.setAttribute("style", s"color: ${color.toString}")
    messagesList.appendChild(message)
    messageContainer.scrollTop = messageContainer.scrollHeight
  }
}