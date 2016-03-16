package com.boldradius.bundlepricing

import breeze.linalg.{norm, DenseVector}
import breeze.optimize.linear._

class LinearProgramExample extends org.specs2.Specification { def is = s2"""
  example $example
"""
  def example = {

    /*
      We want to solve the following:

        Minimise the cost of a cart

        Cart
        A -> 20
        B -> 20
        C -> 10

        Bundles
        
        2(A | B | C)  -> 5$
        2A & 2B       -> 7$

        Unit prices

        A -> 1$
        B -> 3$
        B -> 3$

      We can re write the bundles and unit price has a linear program

        Variables (amount of time a bundle is used)
      
          A A     -> 5$ (x1)
          A B     -> 5$ (x2)
          A C     -> 5$ (x3)
          B B     -> 5$ (x4)
          B C     -> 5$ (x5)
          C C     -> 5$ (x6)
          A A B B -> 7$ (x7)
          A       -> 1$ (x8)
          B       -> 3$ (x9)
          C       -> 3$ (x10)

        Constraints (satisfy cart quantities and problem domain)

          Domain

            for all i, xi >= 0 (a quantity is always positive)

          Quantities (we sum bundle/unit price ammount to satisfy the cart quantity)

               x1  x2  x3  x4  x5  x6  x7  x8  x9  x10  =  n
            A   2   1   1               2   1             20
            B       1       2   1       2       1         10
            C           1       1   2                1    10

        Equation (we sum bundle/unit price ammount with their respective cost)

                x1  x2  x3  x4  x5  x6  x7  x8  x9  x10
          Cost:  5   5   5   5   5   5   7   1   3    3

        Relaxation

          we allow bundle/unit quantities to be in the real space (R^n)

        Approximation (not shown in the example)

          We round down to make sure we respect qauntity constraints.
          We redistribute the excess via a Greedy algorithm or simply via unit prices
     */

    val lp = new LinearProgram()
    import lp._
    
    // Variables
    val x1 = Real()
    val x2 = Real()
    val x3 = Real()
    val x4 = Real()
    val x5 = Real()
    val x6 = Real()
    val x7 = Real()
    val x8 = Real()
    val x9 = Real()
    val x10 = Real()

    val cost = (x1 + x2 + x3 + x4 + x5 + x6) * 5 + x7 * 7 + x8 * 1 + x9 * 3 + x10 * 3

    val domain = Set(
      x1 >= 0,
      x2 >= 0,
      x3 >= 0,
      x4 >= 0,
      x5 >= 0,
      x6 >= 0,
      x7 >= 0,
      x8 >= 0,
      x9 >= 0,
      x10 >= 0
    )

    val quantities = Set(
      (x1 * 2) + x2 + x3 + (x7 * 2) + x8 =:= 20, // a
      x2 + (x4 * 2) + x5 + (x7 * 2) + x9 =:= 10, // b
      x3 + x5 + (x6 * 2) + x10           =:= 10  // c
    )

    val solution = minimize(cost.subjectTo((domain ++ quantities).toList: _*)).result

    //                          x1   x2   x3   x4   x5   x6   x7    x8   x9  x10
    val expected = DenseVector(0.0, 0.0, 0.0, 0.0, 0.0, 5.0, 5.0, 10.0, 0.0, 0.0)
    
    norm(solution - expected, 2) must be_<=(1E-4)
  }
}