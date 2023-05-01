package com.escalatesoft.geo

import org.locationtech.jts.geom.Coordinate

/**
  * A coordinate with no CRS. These are intended for convenient construction of
  * other geometries with CRSs attached, and should not be used generally for
  * anything else. In particular these do not (and should not) extend the Geometry
  * type, and cannot be used in features or anywhere else a Geometry is required.
  *
  * @param x the x coordinate
  * @param y the y coordinate
  */
class RawCoordinate2D private (x: Double, y: Double) extends Coordinate(x, y)

object RawCoordinate2D:

  /**
    * Create a new raw coordinate 2D from given x and y doubles
    * @param x the x coordinate
    * @param y the y coordinate
    * @return
    */
  def apply(x: Double, y: Double): RawCoordinate2D = new RawCoordinate2D(x, y)
