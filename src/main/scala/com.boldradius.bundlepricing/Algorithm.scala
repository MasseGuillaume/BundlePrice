package com.boldradius.bundlepricing

trait BundlePricingAlgorithm {
  def minimizeCost(cart: Bag[Product], unitCost: Map[Product, Cost], bundles: Set[Bundle]): Cost
}

object Exhaustive extends BundlePricingAlgorithm {
  def minimizeCost(cart: Bag[Product], unitCost: Map[Product, Cost], bundles: Set[Bundle]): Cost = {
    def loop(state: CartState): Cost = {
      val currentCost = state.total(unitCost)
      if(state.items.isEmpty) currentCost
      else {
        val next = bundles.flatMap(bundle => bundle(state))
        if(next.isEmpty) currentCost
        else List(loop(next.minBy(loop)), currentCost).min
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
    ex: cart: AAB, bundle A & B => Free C
  */
  def minimizeCost(cart: Bag[Product], unitCost: Map[Product, Cost], bundles: Set[Bundle]): Cost = {
    val initialUnitTotalCost = CartState(cart).total(unitCost)
    def loop(state: CartState): Cost = {
      val currentCost = state.total(unitCost)
      if(state.items.isEmpty) currentCost
      else {
        val next = bundles.flatMap(bundle => bundle(state))
        if(next.isEmpty) currentCost
        else {
          // for simplicity we consider the global unitCost / bundle price ratio
          val state2 = next.minBy(_.total(unitCost) / currentCost)
          val state2Cost = state2.total(unitCost)

          if(currentCost < state2Cost) currentCost
          else loop(state2)
        }
      }
    }
    loop(CartState(cart))
  }
}