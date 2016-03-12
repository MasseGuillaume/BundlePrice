package com.boldradius.bundlepricing

class BagSpecs extends org.specs2.Specification { def is = s2"""
  Bag
    constructors
      fromMap
        empty $fromMapEmpty
        single $fromMapSingle
      fromList
        empty $fromListEmpty
        singles $fromListSingles
        multiples $fromListMultiples
    contains
      disctinct $containsDisctinct
      intersect $containsIntersect
      contained $containsContained
      nested $containsNested
      size differs $containsSizeDiffer
    -
      empty $removeEmpty
      one element $removeOne
      two elements $removeTwo
    +
      empty $addEmpty
      one element $addOne
    --
      excess $removeExcess
      sufficient $removeSufficient
      empty $removeEmptyMore
    ++
      empty $addEmptyMultiple
      add various elements $addMultiple

"""
  // constructors
  def fromMapEmpty     = Bag.fromMap(Map.empty[Symbol, Int]) ==== Bag.empty[Symbol]
  def fromMapSingle    = Bag.fromMap(Map('a -> 1, 'b -> 1))  ==== Bag('a, 'b)
  def fromMapMultiples = Bag.fromMap(Map('a -> 2, 'b -> 2))  ==== Bag('a, 'b, 'a, 'b)

  def fromListEmpty     = Bag.fromList(List.empty[Symbol])               ==== Bag.empty[Symbol]
  def fromListSingles   = Bag.fromList(List('a, 'b, 'c))                 ==== Bag('a, 'b, 'c)
  def fromListMultiples = Bag.fromList(List('a, 'b, 'c, 'a, 'b, 'c, 'b)) ==== Bag('a, 'b, 'c, 'a, 'b, 'c, 'b)

  // contains
  def containsDisctinct  = Bag('a).contains(Bag('b, 'b)) ==== false
  def containsIntersect  = Bag('a, 'b).contains(Bag('a, 'c)) ==== false
  def containsContained  = Bag('a, 'b).contains(Bag('a)    ) ==== true
  def containsNested     = Bag('a).contains(Bag('a, 'b)) ==== false
  def containsSizeDiffer = Bag('a).contains(Bag('a, 'a)) ==== false

  // -
  val empty = Bag.empty[Symbol]
  def removeEmpty = empty - 'a        ==== empty
  def removeOne   = Bag('a)  - 'a     ==== empty
  def removeTwo   = Bag('a, 'a) - 'a  ==== Bag('a)

  // +
  def addEmpty = empty + 'a   ==== Bag('a)
  def addOne   = Bag('a) + 'a ==== Bag('a, 'a)

  // --
  def removeExcess     = Bag('a, 'b)         -- Bag('a, 'a, 'b, 'b) ==== empty
  def removeSufficient = Bag('a, 'a, 'b, 'b) -- Bag('a, 'b)         ==== Bag('a, 'b)
  def removeEmptyMore  = empty               -- Bag('a, 'b)         ==== empty

  // ++
  def addEmptyMultiple = empty       ++ Bag('a, 'b) ==== Bag('a, 'b)
  def addMultiple      = Bag('a, 'b) ++ Bag('a, 'b) ==== Bag('a, 'a, 'b, 'b)
}