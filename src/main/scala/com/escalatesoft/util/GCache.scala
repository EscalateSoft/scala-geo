package com.escalatesoft.util

import java.util.concurrent.TimeUnit

import com.google.common.base.Ticker
import com.google.common.cache.{RemovalNotification => GuavaRemovalNotification, _}
import scala.concurrent.duration._
import java.util.concurrent.Callable

/**
 * Very scala-esque no-frills wrapper around Guava's CacheBuilder, ejecting
 * a Cache (or LoadingCache) when finished
 */

class GCache[K,V] private (builder: CacheBuilder[K, V]):
  private def updated[NK, NV](builder: CacheBuilder[NK, NV]) = new GCache[NK, NV](builder)

  def maximumSize(max: Long) = updated(builder.maximumSize(max))
  def softValues() = updated(builder.softValues())

  /**
   * Ejects the built cache
   * @tparam NK
   * @tparam NV
   * @return
   */
  def build[NK <: K, NV <: V]: Cache[NK, NV] = builder.build[NK, NV]


case class RemovalNotification[K, V](key: Option[K], value: Option[V], cause: RemovalCause)

object GCache:
  def apply[K, V]() = new GCache[K, V](CacheBuilder.newBuilder().asInstanceOf[CacheBuilder[K,V]])
