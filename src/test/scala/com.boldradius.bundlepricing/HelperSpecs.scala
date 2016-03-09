package com.boldradius.bundlepricing

class HelperSpecs extends org.specs2.Specification { def is = s2"""
  Helpers
    innerJoin $example
      discard missing left keys $innerJoinMissingLeftKeys
      discard missing right keys $innerJoinMissingRightKeys
      left empty $leftEmpty
      right empty $rightEmpty
      empty empty $emptyEmpty
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
}
