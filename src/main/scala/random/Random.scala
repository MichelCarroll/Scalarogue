package random

import scala.util.Random

trait RNG {
  def nextInt:(Int, RNG)
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  implicit class randOps[A](rand: Rand[A]) {

    def flatMap[B](f: A => Rand[B]): Rand[B] = (rng: RNG) => {
      val (x, rng2) = rand(rng)
      val (y, rng3) = f(x)(rng2)
      (y, rng3)
    }

    def map[B](f: A => B): Rand[B] = (rng: RNG) => {
      val (x, rng2) = rand(rng)
      (f(x), rng2)
    }

    def combine[B](other: Rand[B]): Rand[(A,B)] = (rng: RNG) => {
      val (a, rng2) = rand(rng)
      val (b, rng3) = other(rng2)
      ((a, b), rng3)
    }
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def nextPositiveInt: Rand[Int] = rng => {
    val (int, newRng) = rng.nextInt
    val posInt = int match {
      case x if x >= 0 => int
      case x if x == Int.MinValue => 0
      case x if x < 0 => -int
    }
    (posInt, newRng)
  }

  def nextPositiveInt(max: Int): Rand[Int] = nextPositiveInt map { _ % (max + 1) }

  def nextRatio: Rand[Double] = nextPositiveInt map { _.toDouble / Int.MaxValue }

  def nextBoolean: Rand[Boolean] =  nextPositiveInt map { _ % 2 == 0 }

  def nextGaussian: Rand[Double] = nextPositiveInt map (new Random(_).nextGaussian())

  def nextCappedGaussian(maxStd: Double): Rand[Double] = nextPositiveInt map (
    new Random(_).nextGaussian() match {
      case x if x >  maxStd => maxStd
      case x if x < -maxStd => -maxStd
      case x => x
    }
  )

  def nextGaussianRatio(maxStd: Double): Rand[Double] = nextCappedGaussian(maxStd) map { x =>
    (x + maxStd) / maxStd / 2
  }

  def nextPositiveGaussianRatio(maxStd: Double): Rand[Double] = nextCappedGaussian(maxStd) map { x =>
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
      RNG.nextsFromSet(s - item, n - 1).map(_ + item)(newRng)
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