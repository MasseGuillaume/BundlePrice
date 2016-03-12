package com.boldradius.bundlepricing

class CartStateSpecs extends org.specs2.Specification { def is = s2"""
  total cost $totalCost
"""

  def totalCost = {
    val unitCost = Map(
      'a -> BigDecimal(1),
      'b -> BigDecimal(1),
      'c -> BigDecimal(1)
    )
    CartState(Bag('a, 'a, 'b, 'c)).total(unitCost) ==== BigDecimal(4)
  } 
}