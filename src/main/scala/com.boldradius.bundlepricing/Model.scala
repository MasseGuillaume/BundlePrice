package com.boldradius.bundlepricing

object BundlePricing {
  type Cost = BigDecimal
  type Quantity = Int
  type Cart = List[(Quantity, GroceryProduct)]

  trait GroceryProduct
  case object Apple extends GroceryProduct
  case object Bread extends GroceryProduct
  case object Margarine extends GroceryProduct

  trait PricePromotion
  case class BulkPrice(bulkSize: Int, product: GroceryProduct, price: Cost) extends PricePromotion
  case class Freebie(products: Set[GroceryProduct], freeProduct: GroceryProduct) extends PricePromotion

  def minimizeCost(
    cart: Cart,
    unitPrices: Map[GroceryProduct, Cost],
    promotions: Set[PricePromotion]): Either[String, Cost] = {
    
    val unitCosts =
      cart.map{ case (quantity, product) =>
        unitPrices.get(product).map(_ * quantity)
      }.flatten
    
    if(unitCosts.size != cart.size) {
      println(cart.map{ case (_, product) => product}.toSet)
      println(unitPrices.keys)

      val missings = 
        cart.map{ case (_, product) => product}.toSet --
        unitPrices.keys
      Left(s"""Not all products unit cost are defined. Missing: ${missings.mkString(", ")}""")
    }
    else Right(unitCosts.sum)
  }
}