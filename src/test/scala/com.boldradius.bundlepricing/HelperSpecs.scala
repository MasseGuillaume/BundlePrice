package com.boldradius.bundlepricing

class HelperSpecs extends org.specs2.Specification { def is = s2"""
  Helpers
    innerJoin $example
      discard missing left keys $innerJoinMissingLeftKeys
      discard missing right keys $innerJoinMissingRightKeys
      left empty $leftEmpty
      right empty $rightEmpty
      empty empty $emptyEmpty
    upsert
      insert if key is not found $upsertInsertEmpty
      update if key is found $upsertUpate
    bagRemove
      empty $bagRemoveEmpty
      bag with one element k $bagRemoveOne
      bag with two elements k $bagRemoveTwo
    bagAdd
      empty $bagAddEmpty
      bag with one element $bagAddOne
    fullOuterJoin
      left empty $fullOuterJoinLeftEmpty
      right empty $fullOuterJoinRightEmpty
      empty empty $fullOuterJoinEmptyEmpty
      example $fullOuterJoinExample
    toBag
      empty $toBagEmpty
      singles $toBagSingles
      example $toBagExample
    contains
      disctinct $containsDisctinct
      intersect $containsIntersect
      contained $containsContained
      nested $containsNested
      size differs $containsSizeDiffer
"""

  val left = Map(
    'a -> "la",
    'b -> "lb",
    'c -> "lc"
  )

  val right = Map(
    'a -> "ra",
    'b -> "rb",
    'c -> "rc"
  )

  def example = {
    innerJoin(left, right)((_, _)) ====
      Map('a -> (("la", "ra")), 'b -> (("lb", "rb")), 'c -> (("lc", "rc")))
  }

  def innerJoinMissingLeftKeys = {
    innerJoin(left - 'a, right)((_, _)) ====
      Map('b -> (("lb", "rb")), 'c -> (("lc", "rc")))
  }

  def innerJoinMissingRightKeys = {
    innerJoin(left, right - 'a)((_, _)) ====
      Map('b -> (("lb", "rb")), 'c -> (("lc", "rc")))
  }

  val empty = Map.empty[Symbol, String]

  def leftEmpty = {
    innerJoin(empty, right)((_, _)) ==== Map()
  }

  def rightEmpty = {
    innerJoin(left, empty)((_, _)) ==== Map()
  }

  def emptyEmpty = {
    innerJoin(empty, empty)((_, _)) ==== Map()
  }

  val ma = Map('a -> 1)

  def upsertInsertEmpty = {
    upsert(ma)('a, _ + 1, 1) ====
      Map('a -> 2)
  }

  def upsertUpate = {
    upsert(ma)('b, _ + 1, 1) ====
      Map('a -> 1, 'b -> 1)
  }

  val emptyBag = Map.empty[Symbol, Int]

  def bagRemoveEmpty = {
    bagRemove(emptyBag, 'a) ==== emptyBag
  }

  def bagRemoveOne = {
    bagRemove(Map('a -> 1), 'a) ==== emptyBag
  }

  def bagRemoveTwo = {
    bagRemove(Map('a -> 2), 'a) ==== Map('a -> 1)
  }

  def bagAddEmpty = {
    bagAdd(emptyBag, 'a) ==== Map('a -> 1)
  }

  def bagAddOne = {
    bagAdd(Map('a -> 1), 'a) ==== Map('a -> 2)
  }

  val fullLeft = Map(
    'a -> "la",
    'b -> "lb",
    'c -> "lc"
  )

  val fullRight = Map(
    'b -> "rb",
    'c -> "rc",
    'd -> "rd"
  )

  def fullOuterJoinLeftEmpty = {
    fullOuterJoin(empty, fullRight)(_ + _)(l => l)(r => r) ==== fullRight
  }
  def fullOuterJoinRightEmpty = {
    fullOuterJoin(fullLeft, empty)(_ + _)(l => l)(r => r) ==== fullLeft
  }
  def fullOuterJoinEmptyEmpty = {
    fullOuterJoin(empty, empty)(_ + _)(l => l)(r => r) ==== empty
  }
  def fullOuterJoinExample = {
    fullOuterJoin(fullLeft, fullRight)(_ + _)(l => l)(r => r) ==== Map(
      'a -> "la",
      'b -> "lbrb",
      'c -> "lcrc",
      'd -> "rd"
    )
  }

  def toBagEmpty = {
    toBag(List.empty[Symbol]) ==== Map.empty[Symbol, Int]
  }
  def toBagSingles = {
    toBag(List('a, 'b, 'c)) ==== Map('a -> 1, 'b -> 1, 'c -> 1)
  }
  def toBagExample = {
    toBag(List('a, 'b, 'c, 'a, 'b, 'c, 'b)) ==== Map('a -> 2, 'b -> 3, 'c -> 2)
  }

  def containsDisctinct = {
    bagContains(Map('a -> 1), Map('b -> 2)) ==== false
  }

  def containsIntersect = {
    bagContains(Map('a -> 1, 'b -> 1), Map('a -> 1, 'c -> 1)) ==== false
  }

  def containsContained = {
    bagContains(Map('a -> 1, 'b -> 1), Map('a -> 1)) ==== true
  }

  def containsNested = {
    bagContains(Map('a -> 1), Map('a -> 1, 'b -> 1)) ==== false
  }

  def containsSizeDiffer = {
    bagContains(Map('a -> 1), Map('a -> 2)) ==== false
  }
}