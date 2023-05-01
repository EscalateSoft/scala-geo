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

def fn1ToTuple[A, B](f: A => B): Tuple1[A] => B = a => f(a._1)
def fn2ToTuple[A, B, C](f: (A, B) => C): ((A, B)) => C = f.tupled
def fn3ToTuple[A, B, C, D](f: (A, B, C) => D): ((A, B, C)) => D = f.tupled
def fn4ToTuple[A, B, C, D, E](f: (A, B, C, D) => E): ((A, B, C, D)) => E = f.tupled
def fn5ToTuple[A, B, C, D, E, F](f: (A, B, C, D, E) => F): ((A, B, C, D, E)) => F = f.tupled
def fn6ToTuple[A, B, C, D, E, F, G](f: (A, B, C, D, E, F) => G): ((A, B, C, D, E, F)) => G = f.tupled
def fn7ToTuple[A, B, C, D, E, F, G, H](f: (A, B, C, D, E, F, G) => H): ((A, B, C, D, E, F, G)) => H = f.tupled
def fn8ToTuple[A, B, C, D, E, F, G, H, I](f: (A, B, C, D, E, F, G, H) => I): ((A, B, C, D, E, F, G, H)) => I = f.tupled
def fn9ToTuple[A, B, C, D, E, F, G, H, I, J](f: (A, B, C, D, E, F, G, H, I) => J): ((A, B, C, D, E, F, G, H, I)) => J = f.tupled