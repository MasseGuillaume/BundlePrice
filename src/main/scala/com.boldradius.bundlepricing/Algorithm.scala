package com.boldradius.bundlepricing

trait BundlePricing {
  def minimizeCost(cart: Bag[Product], unitCost: Map[Product, Cost], bundles: Set[Bundle]): Cost
}

trait BundlePricingAlgorithm extends BundlePricing {
  def minimizeCost(cart: Bag[Product], unitCost: Map[Product, Cost], bundles: Set[Bundle]): Cost = {
    def loop(state: CartState): Cost = {
      val currentCost = state.total(unitCost)
      if(state.items.isEmpty) currentCost
      else {
        val nexts = bundles.flatMap(bundle => bundle(state))
        if(nexts.isEmpty) currentCost
        else List(loop(heuristic(nexts, unitCost, loop)), currentCost).min
      }
    }
    loop(CartState(cart))
  }
  def heuristic(
    states: Set[CartState], 
    unitCost: Map[Product, Cost],
    search: CartState => Cost): CartState
}

object Exhaustive extends BundlePricingAlgorithm {
  def heuristic(
    states: Set[CartState],
    unitCost: Map[Product, Cost],
    search: CartState => Cost): CartState = states.minBy(search)
}

object Greedy extends BundlePricingAlgorithm {
  def heuristic(
    states: Set[CartState],
    unitCost: Map[Product, Cost],
    search: CartState => Cost): CartState = states.minBy(_.total(unitCost))
}

class MonteCarlo(rnd: util.Random) extends BundlePricingAlgorithm {
 def heuristic(
    states: Set[CartState],
    unitCost: Map[Product, Cost],
    search: CartState => Cost): CartState = {
    states.toVector(rnd.nextInt(states.size))
  }
}

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
// object Grab extends BundlePricing {
//   def minimizeCost(cart: Bag[Product], unitCost: Map[Product, Cost], bundles: Set[Bundle]): Cost = {

//   }
// }