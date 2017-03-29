package random

import scala.util.Random

trait RNG {
  def nextInt:(Int, RNG)
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, newRng) = s(rng)
    (f(a), newRng)
  }

  def nextPositiveInt: Rand[Int] = rng => {
    val (int, newRng) = rng.nextInt
    val posInt = int match {
      case x if x >= 0 => int
      case x if x == Int.MinValue => 0
      case x if x < 0 => -int
    }
    (posInt, newRng)
  }

  def nextPositiveInt(max: Int): Rand[Int] = RNG.map(nextPositiveInt) { _ % (max + 1) }

  def nextRatio: Rand[Double] = RNG.map(nextPositiveInt) { _.toDouble / Int.MaxValue }

  def nextBoolean: Rand[Boolean] =  RNG.map(nextPositiveInt) { _ % 2 == 0 }

  def nextGaussian: Rand[Double] = RNG.map(nextPositiveInt)(new Random(_).nextGaussian())

  def nextCappedGaussian(maxStd: Double): Rand[Double] = RNG.map(nextPositiveInt)(
    new Random(_).nextGaussian() match {
      case x if x >  maxStd => maxStd
      case x if x < -maxStd => -maxStd
      case x => x
    }
  )

  def nextGaussianRatio(maxStd: Double): Rand[Double] = RNG.map(nextCappedGaussian(maxStd)) { x =>
    (x + maxStd) / maxStd / 2
  }

  def nextPositiveGaussianRatio(maxStd: Double): Rand[Double] = RNG.map(nextCappedGaussian(maxStd)) { x =>
    Math.abs(x) / maxStd
  }

  def sequence[A](s: List[Rand[A]]): Rand[List[A]] = rng => s match {
    case Nil     => (List(), rng)
    case x :: xs =>
      val (a, newRng) = x(rng)
      val (j, k) = sequence(xs)(newRng)
      (a :: j, k)
  }

  def nextFromSet[T](s: Set[T]): Rand[Option[T]] = rng => {
    if(s.isEmpty)
      (None, rng)
    else {
      val (i, newRng) = RNG.nextPositiveInt(s.size - 1)(rng)
      (Some(s.iterator.drop(i).next), newRng)
    }
  }

  def nextsFromSet[T](s: Set[T], n: Int): Rand[Set[T]] = rng => {
    if(n == 0)
      (Set(), rng)
    else {
      val (i, newRng) = RNG.nextPositiveInt(s.size - 1)(rng)
      val item = s.iterator.drop(i).next
      RNG.map(RNG.nextsFromSet(s - item, n - 1))(_ + item)(newRng)
    }
  }
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed =  (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}