package com.escalatesoft.util

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TupleSubsetSpec extends AnyFunSuite with Matchers:
  case class A(a: Int)
  case class B(b: String)
  case class C(c: Double)
  case class D(d: Boolean)
  case class E(e: Double)

  test("Subset a tuple using types") {
    val t1: (A, B, C, D, E) = A(1) *: B("a") *: C(1.0) *: D(true) *: E(2.0) *: EmptyTuple
    val t2: (C, E, B) = subset(t1)
    t2 shouldBe (C(1.0) *: E(2.0) *: B("a") *: EmptyTuple)
  }

  test("Cope with two of the same type in a tuple by always using the first") {
    val t1: (A, B, C, D, E, A) = A(1) *: B("a") *: C(1.0) *: D(true) *: E(2.0) *: A(2) *: EmptyTuple
    val t2: (C, E, C, B, A, A) = subset(t1)
    t2 shouldBe (C(1.0) *: E(2.0) *: C(1.0) *: B("a") *: A(1) *: A(1) *: EmptyTuple)
  }

  test("Not compile when a type is missing") {
    val t1: (A, B, C, D) = A(1) *: B("a") *: C(1.0) *: D(true) *: EmptyTuple
    "val t2: (C, E, B) = subset(t1)" shouldNot compile // E is not in the tuple
    "val t2: (C, E, B) = subset(t1)" shouldNot typeCheck // E is not in the tuple
    "val t3: (C, B) = subset(t1)" should compile
  }

  case class Duration(v: Double)
  case class Flow(v: Double)
  case class Yield(v: Double)
  case class Price(v: Double)
  case class Profit(v: Double)

  def yieldFor(f: Flow, d: Duration): Yield = Yield(f.v * d.v)
  def profitFor(f: Flow, d: Duration, p: Price): Profit = Profit(f.v * d.v * p.v)

  test("Prepend the result of calling a function to a tuple of parameters") {
    val t1 = (Price(10.0), Flow(1.5), Duration(2.0), A(1), B("foo"))
    val t2 = prependResult(yieldFor.tupled, t1)
    t2 shouldBe ((Yield(3.0), Price(10.0), Flow(1.5), Duration(2.0), A(1), B("foo")))
    val t3 = prependResult(profitFor.tupled, t2)
    t3 shouldBe ((Profit(30.0), Yield(3.0), Price(10.0), Flow(1.5), Duration(2.0), A(1), B("foo")))
  }
