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
    fullOuterJoin
      left empty $fullOuterJoinLeftEmpty
      right empty $fullOuterJoinRightEmpty
      empty empty $fullOuterJoinEmptyEmpty
      example $fullOuterJoinExample
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
    innerJoin(empty, right)(_ + _) ==== empty
  }

  def rightEmpty = {
    innerJoin(left, empty)(_ + _) ==== empty
  }

  def emptyEmpty = {
    innerJoin(empty, empty)(_ + _) ==== empty
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
}