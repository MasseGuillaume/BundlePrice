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
}