package com.boldradius.bundlepricing

class SelectionSpecs extends org.specs2.Specification { def is = s2"""
  selections
    and $kSelectionAnd
    or $kSelectionOr
"""

  def kSelectionAnd = {
    (2 * ('a & 'b)).kselections ==== Set(
      Bag('a, 'b, 'a, 'b)
    )
  }
  
  def kSelectionOr = {
    (2 * ('a | 'b | 'c)).kselections ==== Set(
      Bag('a, 'a), Bag('a, 'b), Bag('a, 'c),
      Bag('b, 'b), Bag('b, 'c),
      Bag('c, 'c)
    )
  }
}