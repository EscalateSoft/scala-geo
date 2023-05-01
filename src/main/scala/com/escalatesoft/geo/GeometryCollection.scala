package com.escalatesoft.geo

import org.locationtech.jts.{geom => jts}

private[geo] trait GeometryCollection[CRS, COMP <: Geometry[CRS], JTS <: jts.Geometry]
    extends Geometry[CRS]:
  protected def jtsComponents: Seq[JTS] =
    val jtsGeom = asJTS
    for i <- 0 until jtsGeom.getNumGeometries yield jtsGeom.getGeometryN(i).asInstanceOf[JTS]

  def componentGeometries: Seq[COMP] = jtsComponents.map(makeComponent)

  protected def makeComponent(jtsG: JTS): COMP
