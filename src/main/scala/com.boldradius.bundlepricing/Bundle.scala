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
      .filter(combination => combination.forall(state.items.contains))
      .map(combination => discount(combination, state))
  }
}

case class Percentage(value: BigDecimal)
object Percentage {
  def apply(v: Int): Percentage = Percentage(v.toDouble)
  def apply(v: Double): Percentage = Percentage(BigDecimal(v / 100))
}

sealed trait Discount {
  def apply(selected: Bag[Product], state: CartState): CartState
}

case class Free(product: Product) extends Discount {
  def apply(selected: Bag[Product], state: CartState): CartState = {
    state.copy(
      items = bagRemove(state.items, product),
      paid = upsert(state.paid)(product, _ + 1, 1)
    )
  }
}

case class Price(cost: BigDecimal) extends Discount {
  def apply(selected: Bag[Product], state: CartState): CartState = {
    state.copy(
      items = selected.foldLeft(state.items)(bagRemove),
      paid = selected.foldLeft(state.paid){ case (paid, item) =>
        upsert(paid)(item, _ + 1, 1)
      },
      runningTotal = state.runningTotal + cost
    )
  }
}

// and many more ...
