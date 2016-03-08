package com.boldradius.bundlepricing

trait SelectionSyntax {
  implicit class SelectionExtensions[T](val a: T) {
    def &(b: T) = And(List(a, b))
    def |(b: T) = Or(Set(a, b))
  }

  implicit class SelectionIntExtensions(val v: Int) {
    def *[T](sel: Selection[T]) = sel * v
  }
}

// we dont support mixing And and Or yet
// ex: (A | B) & (C | D)
sealed trait Selection[T] {
  def *(n: Int): Selection[T]
  def kselections: Set[Bag[T]]
}
case class And[T](xs: Bag[T], k: Int = 1) extends Selection[T] {
  def *(x: Int): Selection[T] = copy(k = x)

  // 2 * (A & B) => ABAB
  def kselections: Set[Bag[T]] =
    Set(List.fill(k)(xs).flatten)

  def &(x: T) = And(x :: xs)
}

// An Or selection will generate various selections
case class Or[T](xs: Set[T], k: Int = 1) extends Selection[T] {
  def *(x: Int): Selection[T] = copy(k = x)

  // 2 * (A | B | C) => AA, AB, AC, BB, BC, CC
  def kselections: Set[Bag[T]] =
    List.fill(k)(xs).flatten.combinations(k).toSet

  def |(x: T) = Or(xs + x)
}