package com.boldradius.bundlepricing

object Bag {
  def empty[T] = new Bag(Map.empty[T, Int])
  def fromMap[T](values: Map[T, Int]) = new Bag(values)
  def fromList[T](xs: List[T]): Bag[T] = Bag(xs.groupBy(identity).mapValues(_.size))
  def apply[T](values: T*): Bag[T] = Bag.fromList(values.toList)
}

case class Bag[T](values: Map[T, Int]) {
  def isEmpty = values.isEmpty
  def contains(bag: Bag[T]): Boolean =
    fullOuterJoin(values, bag.values)(_ >= _)(_ => true)(_ => false).values.forall(identity)

  def -(v: T): Bag[T] = {
    values.get(v) match {
      case None => this
      case Some(1) => Bag.fromMap(values - v)
      case Some(n) => Bag.fromMap(values.updated(v, n - 1))
    }
  }
  def +(v: T): Bag[T] = Bag.fromMap(upsert(values)(v, _ + 1, 1))
  def --(bag: Bag[T]): Bag[T] = {
    val updated = values.map{ case (k, v) => k -> Math.max(0, v - bag.values.get(k).getOrElse(0))}
    Bag.fromMap(updated.filter{case (k, v) => v > 0})
  }
  def ++(bag: Bag[T]): Bag[T] = {
    val updated =
      (bag.values.keySet ++ values.keySet).map(k =>
        k -> (bag.values.get(k).getOrElse(0) + values.get(k).getOrElse(0))
      ).toMap
    Bag.fromMap(updated)
  }

  def join[A, B](m: Map[T, A])(f: (Int, A) => B): Map[T, B] = innerJoin(values, m)(f)

  override def toString(): String =
    values.flatMap{ case (k, n) => List.fill(n)(k) }.mkString("Bag(", ", ", ")")
}