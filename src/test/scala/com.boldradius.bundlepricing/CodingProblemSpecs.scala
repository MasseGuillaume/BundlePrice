package com.boldradius.bundlepricing

class CodingProblemSpecs extends org.specs2.Specification { def is = s2"""

This exercise is a common problem in e­commerce and brick­and­mortar retail systems.
A customer shops from some form of catalog, selecting items and quantities they wish to
purchase. When they are ready, they "check out", that is, complete their purchase, at which
point they are given a total amount of money they owe for the purchase of a specific set of
items.

In the bounds of this problem, certains groups of items can be taken together as a "bundle" with
a different price. 
  E.g.
    if I buy a single apple in isolation it costs $$1.99 $singleApple
    if I buy two apples it's $$2.15. $bundleApples

More complex combinations are possible
  E.g. 
    a loaf of bread "A" purchased with two sticks of margarine "B" 
    and the second stick of margarine is free (e.g. $$0). $breadAndMargarine

The same item may appear in more than one bundle
  E.g. 
    any one "cart" of items might be able to be combined in more than one way. $bundleCombines

For this exercise, produce an API and implementation for a service that accepts a collection of
items the customer wishes to purchase (e.g. items and quantities), and produces the lowest
possible price for that collection of items.
  $apiExample

The API is to be called by other applications in the same JVM, e.g. don't worry about providing a 
REST or other remote interface to the API, just the actual method calls is fine. The API is 
initialized with information that provides all the possible bundles and the catalog of
items and their prices. Once it is initialized, many calls can be made at once to the API to
produce a total price for collections of items, and it should be able to handle multiple
simultaneous calls without errors (e.g. if I'm computing the price for one call, that should not
interfere with computing the price for another call).
  $apiConcurent
"""

  val algorithm = Greedy
  val appleUnitCost = BigDecimal(1.99)
  def singleApple = {
    algorithm.minimizeCost(
      cart = Bag('apple),
      unitCost = Map('apple -> appleUnitCost),
      bundles = Set()
    ) ==== appleUnitCost
  }
  def bundleApples = {
    val appleBundleCost = BigDecimal(2.15)
    algorithm.minimizeCost(
      cart = Bag('apple, 'apple),
      unitCost = Map('apple -> appleUnitCost),
      bundles = Set(Bundle('apple & 'apple, Price(appleBundleCost)))
    ) ==== appleBundleCost
  }
  def breadAndMargarine = {
    val breadCost = BigDecimal(2)
    val margarineCost = BigDecimal(1)
    algorithm.minimizeCost(
      cart = Bag('bread, 'margarine, 'margarine),
      unitCost = Map('bread -> breadCost, 'margarine -> margarineCost),
      bundles = Set(Bundle('bread & 'margarine & 'margarine, Free('margarine)))
    ) ==== breadCost + margarineCost
  }
  def bundleCombines = {
    val one = BigDecimal(1)
    algorithm.minimizeCost(
      cart = Bag('a, 'b, 'c, 'd),
      unitCost = Map('a -> one, 'b -> one, 'c -> one, 'd -> one),
      bundles = Set(
        Bundle('a & 'b, Free('c)),
        Bundle('a & 'b, Free('d))
      )
    ) ==== one + one // 'c & 'd free
  }
  def example = 
    algorithm.minimizeCost(
      cart = Bag.fromMap(Map(
        'a -> 8,
        'b -> 2,
        'c -> 3
      )),
      unitCost = Map(
        'a -> BigDecimal(1),
        'b -> BigDecimal(3),
        'c -> BigDecimal(3)
      ),
      bundles = Set(
        Bundle('a & 'b & 'c, Free('a)),
        Bundle('a & 'a & 'b & 'b, Price(BigDecimal(7))),
        Bundle(2 * ('a | 'b | 'c), Price(BigDecimal(5)))
      )
    )

  def apiExample = {
    example must be_>=(BigDecimal(0))
  }
  def apiConcurent = {
    val result = example
    // all algorithms converge to the same result
    (1 to 1000).par.map( _ => example ).forall(_ == result)
  }
}