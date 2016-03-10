package com.boldradius.bundlepricing

trait MapHelpers {
  def innerJoin[K, A, B, Z](m1: Map[K, A], m2: Map[K, B])(f: (A, B) => Z): Map[K, Z] = {
    m1.flatMap{ case (k, a) => 
      m2.get(k).map(b => Map(k -> f(a, b))).getOrElse(Map.empty[K, Z])
    }
  }

  def fullOuterJoin[K, A, B, Z](m1: Map[K, A], m2: Map[K, B])(f: (A, B) => Z)(da: A => Z)(db: B => Z): Map[K, Z] = {
    val km1 = m1.keySet
    val km2 = m2.keySet

    (km2 -- km1).map(k => k -> db(m2(k))).toMap ++      // missing in m1
    (km1 -- km2).map(k => k -> da(m1(k))).toMap ++      // missing in m2
    (km1.intersect(km2)).map(k => k -> f(m1(k), m2(k))) // in m1 and m2
  }

  def upsert[A, B](m: Map[A, B])(k: A, f: B => B, d: => B): Map[A, B] = {
    m.updated(k,
      m.get(k) match {
        case None => d
        case Some(b) => f(b)
      }
    )
  }

  def bagRemove[K](m: Map[K, Int], k: K): Map[K, Int] = {
    m.get(k) match {
      case None => m
      case Some(1) => m - k
      case Some(n) => m.updated(k, n - 1)
    }
  }

  def bagAdd[K](m: Map[K, Int], k: K): Map[K, Int] =
    upsert(m)(k, _ + 1, 1)

  def toBag[K](xs: List[K]): Map[K, Int] =
    xs.groupBy(identity).mapValues(_.size)

  def bagContains[K](m1: Map[K, Int], m2: Map[K, Int]): Boolean = 
    fullOuterJoin(m1, m2)(_ >= _)(_ => true)(_ => false).values.forall(identity)
}