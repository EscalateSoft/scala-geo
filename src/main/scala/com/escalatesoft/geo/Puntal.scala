package com.escalatesoft.geo

import com.escalatesoft.geo.crs.CRST

/**
  * A 0D or point category for geometries.
  * @tparam CRS the CRS for the Puntal geometry
  */
abstract class Puntal[CRS: CRST] extends Geometry[CRS]:
  def transformCRS[NEW_CRS: CRST]: Puntal[NEW_CRS]
  def assignId(id: String): Puntal[CRS] with GeometryWithID[CRS]

  @inline override def defaultTolerance: Double = ccrs.pointTolerance
