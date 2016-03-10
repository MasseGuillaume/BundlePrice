package com.boldradius.bundlepricing

trait MapHelpers {
  def innerJoin[K, A, B, Z](m1: Map[K, A], m2: Map[K, B])(f: (A, B) => Z): Map[K, Z] = {
    m1.flatMap{ case (k, a) => 
      m2.get(k).map(b => Map(k -> f(a, b))).getOrElse(Map.empty[K, Z])
    }
  }

  def upsert[A, B](m: Map[A, B])(k: A, f: B => B, d: => B): Map[A, B] = {
    m.updated(k,
      m.get(k) match {
        case None => d
        case Some(b) => f(b)
      }
    )
  }

  // TODO: Bag implementation
  def bagRemove[K](m: Map[K, Int], k: K): Map[K, Int] = {
    m.get(k) match {
      case None => m
      case Some(1) => m - k
      case Some(n) => m.updated(k, n - 1)
    }
  }

  def bagAdd[K](m: Map[K, Int], k: K): Map[K, Int] =
    upsert(m)(k, _ + 1, 1)
}