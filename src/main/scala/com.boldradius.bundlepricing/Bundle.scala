package com.boldradius.bundlepricing

final case class CartState(
  items: Bag[Product],
  paid: Bag[Product] = Bag(),
  runningTotal: Cost = BigDecimal(0)) {
  def total(unitCost: Map[Product, Cost]): Cost =
    runningTotal + items.join(unitCost){ case (quantity, cost) =>
      cost * quantity
    }.values.sum
}

// Select some items from the cart and apply a discount
final case class Bundle(selection: Selection[Product], discount: Discount) {
  def apply(state: CartState): Set[CartState] = {
    selection.kselections
      .filter(combination => state.items.contains(combination))
      .map(combination => discount(combination, state))
  }
}

sealed trait Discount {
  def apply(selected: Bag[Product], state: CartState): CartState
}

final case class Free(product: Product) extends Discount {
  def apply(selected: Bag[Product], state: CartState): CartState = {
    state.copy(
      items = state.items - product,
      paid = state.paid + product
    )
  }
}

final case class Price(cost: BigDecimal) extends Discount {
  def apply(selected: Bag[Product], state: CartState): CartState = {
    state.copy(
      items = state.items -- selected,
      paid = state.paid ++ selected,
      runningTotal = state.runningTotal + cost
    )
  }
}

// and more ...
