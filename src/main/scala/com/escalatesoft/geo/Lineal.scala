package com.escalatesoft.geo

import com.escalatesoft.geo.crs.CRST

/**
  * A 1D (line or lineal) categorization for geometries. Known
  * subtypes include LineString2D and MultiLineString2D
  * @tparam CRS the CRS of the 1D geometry
  */
abstract class Lineal[CRS: CRST] extends Geometry[CRS]:
  @inline override def defaultTolerance: Double = ccrs.pointTolerance
  override def assignId(id: String): Lineal[CRS] with GeometryWithID[CRS]
