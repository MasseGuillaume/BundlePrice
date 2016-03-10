package com.boldradius

package object bundlepricing extends SelectionSyntax {

  // TODO: Bag implementation
  type Bag[T] = List[T]
  type Product = Symbol
  type Quantity = Int
  type Cost = BigDecimal
  type Cart = Map[Product, Quantity]

  // collection.immutable.HashMap has merged but the mergef returns any for some reason
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

  def bagRemove[K](m: Map[K, Int], k: K): Map[K, Int] = {
    m.get(k) match {
      case None => m
      case Some(1) => m - k
      case Some(n) => m.updated(k, n - 1)
    }
  }
}