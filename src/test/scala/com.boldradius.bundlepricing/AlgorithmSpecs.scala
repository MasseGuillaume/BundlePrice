package com.boldradius.bundlepricing

class AlgorithmSpecs extends org.specs2.mutable.Specification {

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
  val bundles = Set(
    Bundle('a & 'b & 'c, Free('a)),
    Bundle('a & 'b, Price(BigDecimal(3))),
    Bundle(2 * ('a | 'b | 'c), Price(BigDecimal(5)))
  )
  val cartUnitCost = CartState(cart).total(unitCost)

  s"Bundle Pricing Algorithms (Cost: $cartUnitCost)" >> {
    "Exhaustive" >> {
      val costExhaustive = Exhaustive.minimizeCost(
        cart,
        unitCost,
        bundles
      )
      s"Cost: $costExhaustive" >> {
        costExhaustive must be_<(cartUnitCost)  
      }      
    }

    "GRAB" >> {
      val costGRAB = GRAB.minimizeCost(
        cart,
        unitCost,
        bundles   
      ) 
      s"Cost: $costGRAB" >> {
        costGRAB must be_<(cartUnitCost)  
      }
    }
  }
}