package com.boldradius.bundlepricing

case class CartState(items: Cart, paid: Cart = Map(), runningTotal: Cost = BigDecimal(0)) {
  def total(unitCost: Map[Product, Cost]): Cost =
    runningTotal + innerJoin(items, unitCost){ case (quantity, cost) =>
      cost * quantity
    }.values.sum
}

// Select some items from the cart and apply a discount
case class Bundle(selection: Selection[Product], discount: Discount) {
  def apply(state: CartState): Set[CartState] = {
    selection.kselections
      .filter(combination => bagContains(state.items, toBag(combination)))
      .map(combination => discount(combination, state))
  }
}

sealed trait Discount {
  def apply(selected: Bag[Product], state: CartState): CartState
}

case class Free(product: Product) extends Discount {
  def apply(selected: Bag[Product], state: CartState): CartState = {
    state.copy(
      items = bagRemove(state.items, product),
      paid = bagAdd(state.paid, product)
    )
  }
}

case class Price(cost: BigDecimal) extends Discount {
  def apply(selected: Bag[Product], state: CartState): CartState = {
    state.copy(
      items = selected.foldLeft(state.items)(bagRemove),
      paid = selected.foldLeft(state.paid)(bagAdd),
      runningTotal = state.runningTotal + cost
    )
  }
}

// and many more ...
