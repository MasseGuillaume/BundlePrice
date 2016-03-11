package com.boldradius.bundlepricing

trait BundlePricingAlgorithm {
  def minimizeCost(cart: Cart, unitCost: Map[Product, Cost], bundles: Set[Bundle]): Cost
}

object Exhaustive extends BundlePricingAlgorithm {
  def minimizeCost(cart: Cart, unitCost: Map[Product, Cost], bundles: Set[Bundle]): Cost = {
    def loop(state: CartState): Cost = {
      if(state.items.isEmpty) state.total(unitCost)
      else {
        val next = bundles.flatMap(bundle => bundle(state))
        if(next.isEmpty) state.total(unitCost)
        else {
          val state2 = next.minBy(loop)
          val state2Cost = state2.total(unitCost)
          val stateCost = state.total(unitCost)

          if(stateCost < state2Cost) stateCost
          else loop(state2)
        }
      }
    }
    loop(CartState(cart))
  }
}

object GRAB extends BundlePricingAlgorithm {

  /*
    GRAB algorithm
    page 9 http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.90.1663&rep=rep1&type=pdf
    > [A]t each step the algorithm chooses a bundle that includes at least one desired item
    > not purchased at previous steps. The chosen bundle has the lowest ratio of cost to the individual
    > costs of these desired items in the bundle.
  
    We assume we know the unit cost of all items
    We may end up with items we didn't want to purchase in the beggining, we discard them
    ex: Cart: AAB, Bundle A & B => Free C
  */
  def minimizeCost(cart: Cart, unitCost: Map[Product, Cost], bundles: Set[Bundle]): Cost = {
    val initialUnitTotalCost = CartState(cart).total(unitCost)
    def loop(state: CartState): Cost = {
      if(state.items.isEmpty) state.total(unitCost)
      else {
        val next = bundles.flatMap(bundle => bundle(state))
        if(next.isEmpty) state.total(unitCost)
        else {
          // for simplicity we consider the global unitCost / bundle price ratio
          val stateCost = state.total(unitCost)
          val state2 = next.minBy(_.total(unitCost) / stateCost)         
          val state2Cost = state2.total(unitCost)

          if(stateCost < state2Cost) stateCost
          else loop(state2)
        }
      }
    }
    loop(CartState(cart))
  }
}