package com.boldradius

package object bundlepricing extends SelectionSyntax with MapHelpers {
  type Product = Symbol
  type Cost = BigDecimal
  
  // TODO: Bag implementation
  type Bag[T] = List[T]
  type Cart = Map[Product, Int]
}