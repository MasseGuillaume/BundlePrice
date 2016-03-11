package com.boldradius.bundlepricing

class AlgorithmSpecs extends org.specs2.Specification { def is = s2"""
  Algorithm
    ex1 $ex1
"""

  def ex1 = {

    val cart = Map(
      'a -> 5,
      'b -> 2,
      'c -> 3
    )

    val unitCost = Map(
      'a -> BigDecimal(1),
      'b -> BigDecimal(3),
      'c -> BigDecimal(3)
    )

    Algorithm.minimizeCost(
      cart,
      unitCost,
      Set(
        Bundle('a & 'b & 'c, Free('a)),
        Bundle('a & 'b, Price(BigDecimal(3))),
        Bundle(2 * ('a | 'b | 'c), Price(BigDecimal(5)))
      )
    ) must be_<(CartState(cart).total(unitCost))
  }
}