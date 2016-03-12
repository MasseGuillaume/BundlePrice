package com.boldradius.bundlepricing

@SuppressWarnings(Array("org.brianmckenna.wartremover.warts.NonUnitStatements"))
class AlgorithmSpecs extends org.specs2.mutable.Specification {

  "Bundle Pricing Algorithms" >> {
    val unitCost = Map(
      'a -> BigDecimal(1),
      'b -> BigDecimal(3),
      'c -> BigDecimal(3)
    )
    val bundles = Set(
      Bundle('a & 'b & 'c, Free('a)),
      Bundle('a & 'a & 'b & 'b, Price(BigDecimal(7))),
      Bundle(2 * ('a | 'b | 'c), Price(BigDecimal(5)))
    )
    val smallCart = Bag.fromMap(Map(
      'a -> 8,
      'b -> 2,
      'c -> 3
    ))
    val smallCartUnitCost = CartState(smallCart).total(unitCost)
    s"small cart (Cost: $smallCartUnitCost)" >> {
      "Exhaustive" >> {
        val costExhaustive = Exhaustive.minimizeCost(
          smallCart,
          unitCost,
          bundles
        )
        s"Cost: $costExhaustive" >> {
          costExhaustive must be_<(smallCartUnitCost)  
        }      
      }

      "GRAB" >> {
        val costGRAB = GRAB.minimizeCost(
          smallCart,
          unitCost,
          bundles   
        ) 
        s"Cost: $costGRAB" >> {
          costGRAB must be_<(smallCartUnitCost)  
        }
      }
    }

    val largeCart = Bag.fromMap(Map(
      'a -> 50,
      'b -> 20,
      'c -> 30
    ))
    val largeCartUnitCost = CartState(largeCart).total(unitCost)
    s"large cart (Cost: $largeCartUnitCost)" >> {
      // Does not terminate:
      // "Exhaustive" >> {
      //   val costExhaustive = Exhaustive.minimizeCost(
      //     largeCart,
      //     unitCost,
      //     bundles
      //   )
      //   s"Cost: $costExhaustive" >> {
      //     costExhaustive must be_<(smallCartUnitCost)  
      //   }      
      // }
      "GRAB" >> {
        val costGRAB = GRAB.minimizeCost(
          largeCart,
          unitCost,
          bundles   
        ) 
        s"Cost: $costGRAB" >> {
          costGRAB must be_<(largeCartUnitCost)  
        }
      }
    }
  }
}