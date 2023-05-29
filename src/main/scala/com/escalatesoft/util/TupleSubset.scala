package com.escalatesoft.util

opaque type Find[T <: Tuple, E] = T => E
object Find:
  given [T <: Tuple]: Find[T, EmptyTuple] = _ => EmptyTuple
  given [A, T <: Tuple]: Find[A *: T, A] = 
    case a *: _ => a
  given [A, H, T <: Tuple](using f: Find[T, A]): Find[H *: T, A] =
    case _ *: t => f(t)

opaque type Subset[T1 <: Tuple, T2 <: Tuple] = T1 => T2
object Subset:
  given [T1 <: Tuple]: Subset[T1, EmptyTuple] = _ => EmptyTuple
  given [A, T1 <: Tuple, T2 <: Tuple](using 
      s: Subset[T1, T2], 
      f: Find[T1, A]
  ): Subset[T1, A *: T2] =
    t => f(t) *: s(t)

def subset[T1 <: Tuple, T2 <: Tuple](t: T1)(using s: Subset[T1, T2]): T2 = s(t)

def prependValueToTuple[T <: Tuple, A](a: A, t: T): A *: T = a *: t

def prependResult[T1 <: Tuple, T2 <: Tuple, A](f: T2 => A, t: T1)(using s: Subset[T1, T2]): A *: T1 =
  f(subset(t)) *: t
