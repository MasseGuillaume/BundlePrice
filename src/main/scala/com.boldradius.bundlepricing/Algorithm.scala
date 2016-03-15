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

object LinearProgramming extends BundlePricing {
  def minimizeCost(cart: Bag[Product], unitCost: Map[Product, Cost], bundles: Set[Bundle]): Cost = {
    import breeze.optimize.linear._
    val lp = new LinearProgram()
    import lp._

    // we relax the problem from the Integer space to the Real space

    // a a -> 3 
    val bundleVariables: Set[(Bag[Product], Cost, Real)] =
      bundles.flatMap {
        case Bundle(selection, Price(p)) => selection.kselections.map((_, p, Real()))
        case _ => Set.empty[(Bag[Product], Cost, Real)] // Free, Discount and other bundles are not part of the linear programming
      }

    // a -> 2
    val unitVariables: Set[(Bag[Product], Cost, Real)] =
      unitCost.map{ case (product, cost) =>
        (Bag(product), cost, Real())
      }.toSet

    val variables = bundleVariables ++ unitVariables

    // we want a positive quantity of bundle/unit cost
    val domainConstraints = variables.map{ case (_, _, variable) => variable >= 0.0 }

    val quantityConstraints = cart.values.map{ case (product, quantity) =>
      val lhs =
        variables.map{ case (products, _, variable) => 
          variable * products.values.get(product).map(_.toDouble).getOrElse(0.0)
        }.reduceLeft((_: Expression) + _)

      lhs =:= quantity.toDouble
    }

    val cost = variables.map{ case(_, cost, variable) =>
      variable * cost.toDouble
    }.reduceLeft((_: Expression) + _)

    val solution =
      minimize(cost subjectTo((domainConstraints ++ quantityConstraints).toSeq: _*))

    println(solution)

    // We need to approximate this solution in the Integer space
    ???
  }
}