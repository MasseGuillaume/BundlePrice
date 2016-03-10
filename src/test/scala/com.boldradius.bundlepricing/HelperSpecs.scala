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
}
