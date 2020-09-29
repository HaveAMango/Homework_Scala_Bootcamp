package com.evolutiongaming.bootcamp.basics

object BasicsHomework {
  // Homework. Implement functions that calculate https://en.wikipedia.org/wiki/Lowest_common_denominator and
  // https://en.wikipedia.org/wiki/Greatest_common_divisor for integers.

  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a%b)
  }

  def lcm(a: Int, b: Int): Int = {
    (a*b)/gcd(a, b)
  }


  def main(args: Array[String]): Unit = {
    println(gcd(1, 10))
    println(gcd(3, 27))
    println(gcd(3, 5))
    println(gcd(16, 24))

    println(gcd(10, 1))
    println(gcd(27, 3))
    println(gcd(5, 3))
    println(gcd(24, 16))

    println(lcm(1, 10)) //10
    println(lcm(3, 27)) //27
    println(lcm(3, 5)) //15
    println(lcm(16, 24)) //48

    println(lcm(10, 1))
    println(lcm(27, 3))
    println(lcm(5, 3))
    println(lcm(24, 16))


  }
  // Create a new Git public repository for your homework solutions, use `basics` package for this homework.
  // You can use `sbt new scala/hello-world.g8` to start a new bare-bones Scala SBT project.

}
