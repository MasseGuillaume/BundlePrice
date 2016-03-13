package com.boldradius.bundlepricing

class DiscountSpecs extends org.specs2.Specification { def is = s2"""
  A discount is a buisness rule to be applied on a selection of products
    the following discount strategies are implemented
      an arbitrary free product $free
        a product selection can only be used once $freeOnce
      a fixed price $fixedPrice
    or selections will produce more permutations $orSelectionPrice
    a selection must always be satisfied to get a discount $selectionNotSatisfied
"""
  
  def free = {
    val bundle = Bundle('a & 'b & 'c, Free('a))
    val cart = CartState(Bag('a, 'a, 'b, 'c))
    
    bundle(cart) ==== Set(CartState(
      items = Bag('b, 'a, 'c),
      paid = Bag('a),
      consummed = Map(Free('a) -> Bag('b, 'a, 'c))
    ))
  }

  def freeOnce = {
    val bundle = Bundle('a & 'b, Free('f))
    val cart = CartState(Bag('a, 'a, 'b))
    val bundleOnce = bundle(cart)
    bundleOnce ==== Set(CartState(
      items = Bag('a, 'a, 'b),
      paid = Bag('f), 
      consummed = Map(Free('f) -> Bag('a, 'b))
    )) &&
    bundleOnce.flatMap(s => bundle(s)) ==== Set()
  }

  def fixedPrice = {
    val bundle = Bundle('a & 'b, Price(BigDecimal(1)))
    val cart = CartState(Bag('a, 'b))

    bundle(cart) ==== Set(CartState(
      items = Bag(), 
      paid = Bag('a, 'b), 
      runningTotal = BigDecimal(1)
    ))
  }

  def orSelectionPrice = {
    val bundle = Bundle(2 * ('a | 'b | 'c), Price(BigDecimal(5)))
    val cart = 
      CartState(Bag('a, 'a, 'b, 'b, 'c, 'c, 'c))

    bundle(cart) ==== Set(
      CartState(Bag('a, 'a,         'c, 'c, 'c), Bag('b, 'b), 5), 
      CartState(Bag('a, 'a, 'b, 'b, 'c        ), Bag('c, 'c), 5), 
      CartState(Bag('a,     'b, 'b, 'c, 'c    ), Bag('a, 'c), 5), 
      CartState(Bag(        'b, 'b, 'c, 'c, 'c), Bag('a, 'a), 5), 
      CartState(Bag('a,     'b,     'c, 'c, 'c), Bag('a, 'b), 5), 
      CartState(Bag('a, 'a, 'b,     'c, 'c    ), Bag('b, 'c), 5)
    )
  }

  def selectionNotSatisfied = {
    val bundle = Bundle(2 * ('a | 'b | 'c), Price(BigDecimal(5)))
    val cart = CartState(Bag('a))
    bundle(cart) ==== Set()
  }
}