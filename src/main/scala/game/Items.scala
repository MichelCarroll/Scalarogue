package game

/**
  * Created by MichelCarroll on 3/28/2017.
  */
sealed trait Items {
  val amount: Int
  val name: String
}

/**
  * Created by MichelCarroll on 3/28/2017.
  */
case class Gold(amount: Int) extends Items {
  val name = "gold"
}
