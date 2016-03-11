package com.boldradius.bundlepricing

class CartStateSpecs extends org.specs2.Specification { def is = s2"""
  total cost $totalCost
"""

  def totalCost = {
    CartState(Map('a -> 2, 'b -> 1, 'c -> 1)).total(
      Map('a -> BigDecimal(1), 'b -> BigDecimal(1), 'c -> BigDecimal(1))
    ) ==== BigDecimal(4)
  } 
}