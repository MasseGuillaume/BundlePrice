package com.boldradius

package object bundlepricing extends SelectionSyntax {

  // TODO: Bag implementation
  type Bag[T] = List[T]
  
  type Cart = Any
  type Product = Symbol

  // collection.immutable.HashMap has merged but the mergef returns any for some reason
  def innerJoin[K, A, B, Z](m1: Map[K, A], m2: Map[K, B])(f: (A, B) => Z): Map[K, Z] = {
    m1.flatMap{ case (k, a) => 
      m2.get(k).map(b => Map(k -> f(a, b))).getOrElse(Map.empty[K, Z])
    }
  }
}