package com.boldradius.bundlepricing

final case class CartState(
  items: Bag[Product],
  paid: Bag[Product] = Bag(),
  runningTotal: Cost = BigDecimal(0),
  consummed: Map[Discount, Bag[Product]] = Map.empty[Discount, Bag[Product]]) {
  def total(unitCost: Map[Product, Cost]): Cost =
    runningTotal + items.join(unitCost){ case (quantity, cost) =>
      cost * quantity
    }.values.sum
}

// Select some items from the cart and apply a discount
final case class Bundle(selection: Selection[Product], discount: Discount) {
  def applicable(selected: Bag[Product], state: CartState): Boolean = {
    val consummed = state.consummed.get(discount).getOrElse(Bag())
    (state.items -- consummed).contains(selected)
  }
  def apply(state: CartState): Set[CartState] = {
    selection.kselections
      .filter(applicable(_, state))
      .map(discount(_, state))
  }
}

sealed trait Discount {
  def apply(selected: Bag[Product], state: CartState): CartState
}

final case class Free(product: Product) extends Discount {
  def apply(selected: Bag[Product], state: CartState): CartState = {
    state.copy(
      items = state.items - product,
      paid = state.paid + product,
      consummed = upsert(state.consummed)(this,_ ++ selected, selected)
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
