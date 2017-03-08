import org.scalajs.dom
import org.scalajs.dom.raw.{Event, HTMLImageElement}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

case class ImageSourceSize(width: Double, height: Double)
case class Image(element: HTMLImageElement, sourceSize: ImageSourceSize)

class ImageRepository(canvasContext: dom.CanvasRenderingContext2D) {

  private def imageWithSrc(src: String, imageSourceSize: ImageSourceSize): Image = {
    val image = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
    image.src = src
    Image(image, imageSourceSize)
  }

  private def onLoadFuture(img: HTMLImageElement) = {
    if (img.complete) {
      Future.successful(img.src)
    } else {
      val p = Promise[String]()
      img.onload = { (e: Event) =>
        p.success(img.src)
      }
      p.future
    }
  }

  val alphabet = imageWithSrc("images/alphabet.png", ImageSourceSize(1800, 50))
  val closed_door = imageWithSrc("images/closed_door.png", ImageSourceSize(333, 334))
  val open_door = imageWithSrc("images/open_door.png", ImageSourceSize(313, 312))
  val wall = imageWithSrc("images/darkness.png", ImageSourceSize(333, 305))
  val floor = imageWithSrc("images/floor.png", ImageSourceSize(320, 320))
  val upstairs = imageWithSrc("images/upstairs.png", ImageSourceSize(370, 370))
  val downstairs = imageWithSrc("images/downstairs.png", ImageSourceSize(400, 400))
  val nugget = imageWithSrc("images/nugget.png", ImageSourceSize(607, 600))
  val spider = imageWithSrc("images/spider.png", ImageSourceSize(596, 594))


  val loaded: Future[Unit] = Future.sequence(Set(
    alphabet, closed_door, open_door, wall, floor,
    upstairs, downstairs, nugget, spider
  ).map(_.element).map(onLoadFuture(_))).map(_ => Unit)
}