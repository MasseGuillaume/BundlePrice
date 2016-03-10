package com.boldradius.bundlepricing

class DiscountSpecs extends org.specs2.Specification { def is = s2"""
  A discount is a buisness rule to be applied on a selection of products
    the following discount strategies are implemented
      an arbitrary free product $free
      a fixed price $fixedPrice
    or selections will produce more permutations $orSelectionPrice
"""
  
  def free = {
    val bundle = Bundle('a & 'b, Free('a))
    val cart = CartState(Map('a -> 2, 'b -> 1))
    
    bundle(cart) ==== Set(CartState(Map('b -> 1, 'a -> 1), Map('a -> 1)))
  }
  def fixedPrice = {
    val bundle = Bundle('a & 'b, Price(BigDecimal(1)))
    val cart = CartState(Map('a -> 1, 'b -> 1))

    bundle(cart) ==== Set(CartState(Map(), Map('a -> 1, 'b -> 1), BigDecimal(1)))
  }
  def orSelectionPrice = {
    val bundle = Bundle(2 * ('a | 'b | 'c), Price(BigDecimal(5)))
    val cart = CartState(Map('a -> 2, 'b -> 2, 'c -> 3))
    bundle(cart) ==== Set(
      CartState(Map('a -> 2, 'c -> 3),         Map('b -> 2)         ,5), 
      CartState(Map('a -> 2, 'b -> 2, 'c -> 1),Map('c -> 2)         ,5), 
      CartState(Map('a -> 1, 'b -> 2, 'c -> 2),Map('a -> 1, 'c -> 1),5), 
      CartState(Map('b -> 2, 'c -> 3),         Map('a -> 2)         ,5), 
      CartState(Map('a -> 1, 'b -> 1, 'c -> 3),Map('a -> 1, 'b -> 1),5), 
      CartState(Map('a -> 2, 'b -> 1, 'c -> 2),Map('b -> 1, 'c -> 1),5)
    )
  }
}