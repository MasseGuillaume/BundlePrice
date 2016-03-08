package com.boldradius.bundlepricing

class SelectionSpecs extends org.specs2.Specification { def is = s2"""
  selections
    and $kSelectionAnd
    or $kSelectionOr
"""

  def kSelectionAnd = {
    (2 * ('a & 'b)).kselections ==== Set(
      List('a, 'b, 'a, 'b)
    )
  }
  
  def kSelectionOr = {
    (2 * ('a | 'b | 'c)).kselections ==== Set(
      List('a, 'a), List('a, 'b), List('a, 'c),
      List('b, 'b), List('b, 'c),
      List('c, 'c)
    )
  }
}