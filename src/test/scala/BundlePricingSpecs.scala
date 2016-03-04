class BundlePricingSpecs extends org.specs2.Specification { def is = s2"""
  BundlePricingSpecs
    a naive implementation sum the unit cost x price    $naiveImplementation
      any missing unit cost is reported                 $missingUnitCost
"""
  import BundlePricing._

  val unitPrices: Map[GroceryProduct, Cost] = Map(
    Apple -> BigDecimal(1.99),
    Bread -> BigDecimal(3.00),
    Margarine -> BigDecimal(0.50)
  )

  val promotions: Set[PricePromotion] = Set(
    BulkPrice(2, Apple, 2.15),
    Freebie(Set(Bread, Margarine, Margarine), Margarine)
  )

  def naiveImplementation = {
    val cart: Cart = List(
      (2, Apple),
      (1, Bread),
      (2, Margarine)
    )

    minimizeCost(
      cart,
      unitPrices,
      promotions
    ) ==== Right(
      2 * BigDecimal(1.99) +
      1 * BigDecimal(3.00) +
      2 * BigDecimal(0.50)
    )
  }

  def missingUnitCost = {
    minimizeCost(
      cart = List(
        (1, Apple),
        (1, Bread),
        (1, Margarine)
      ),
      unitPrices = Map(
        Apple -> BigDecimal(1.00)
        // missing Bread
        // missing Margarine
      ),
      promotions = Set()
    ) ==== Left("Not all products unit cost are defined. Missing: Bread, Margarine")
  }
}