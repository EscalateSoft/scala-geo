package com.escalatesoft.geo.crs

import org.opengis.referencing.operation.MathTransform
import com.escalatesoft.util.GCache
import com.google.common.cache.Cache

object CRSTransform:
  import org.geotools.referencing.{CRS => GT_CRS}

  private case class TransformKey(
      crs1: CRST[_],
      crs2: CRST[_],
      lenient: Boolean
  )

  private val transformCache: Cache[TransformKey, MathTransform] =
    GCache().maximumSize(1000).softValues().build

  def apply[OLD_CRS: CRST, NEW_CRS: CRST](
      lenient: Boolean = true
  ): MathTransform =

    val oldCRS = summon[CRST[OLD_CRS]]
    val newCRS = summon[CRST[NEW_CRS]]

    transformCache.get(
      TransformKey(oldCRS, newCRS, lenient),
      () => GT_CRS.findMathTransform(oldCRS.crs, newCRS.crs, lenient)
    )

  def sameCRS[OLD_CRS: CRST, NEW_CRS: CRST]: Boolean =
    val oldCRS = summon[CRST[OLD_CRS]]
    val newCRS = summon[CRST[NEW_CRS]]

    oldCRS == newCRS
