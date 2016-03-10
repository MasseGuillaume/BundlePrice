package com.boldradius.bundlepricing

class SolveSpecs extends org.specs2.Specification { def is = s2"""
  solve $solve
"""

  def solve = {
    CartState(Map('a -> 2, 'b -> 1, 'c -> 1)).total(
      Map('a -> BigDecimal(1), 'b -> BigDecimal(1), 'c -> BigDecimal(1))
    ) ==== BigDecimal(4)
  } 
}