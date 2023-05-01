package com.escalatesoft.geo

import com.escalatesoft.geo.crs.{CRSTransform, CRST}
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.operation.valid.{IsValidOp, TopologyValidationError}
import org.locationtech.jts.{geom => jts}

/**
  * Representation of a group of related but spatially separated polygons
  * @param jtsMP the (hidden) underlying JTS geometry upon which this is based
  * @tparam CRS the CRS for the polygons in this MultiPolygon grouping
  */
class MultiPolygon2D[CRS: CRST] private (jtsMP: jts.MultiPolygon)
    extends Polygonal[CRS]
    with Validator[MultiPolygon2D[CRS]]
    with GeometryCollection[CRS, Polygon2D[CRS], jts.Polygon]:
  override protected val jtsGeom: Either[jts.Polygon, jts.MultiPolygon] = Right(jtsMP)

  override protected[geo] def asJTS: jts.MultiPolygon = jtsMP
  def assignId(id: String): MultiPolygon2D[CRS] with GeometryWithID[CRS] =
    new MultiPolygon2D[CRS](jtsMP) with GeometryWithID[CRS]:
      override val geometryId: String = id

  private def getValidationError: Option[TopologyValidationError] =
    Option(new IsValidOp(jtsMP).getValidationError)

  /**
    * Validate this geometry. Either return this item (right) or
    * the validity violation (left)
    *
    * @return an Either with this on the right projection or the invalidity
    *         reason on the left projection.
    */
  override def validated: Either[TopologyValidationError, MultiPolygon2D[CRS]] =
    getValidationError.map(Left.apply).getOrElse(Right(this))

  override def transformCRS[NEW_CRS: CRST]: MultiPolygon2D[NEW_CRS] =
    if CRSTransform.sameCRS[CRS, NEW_CRS] then this.asInstanceOf[MultiPolygon2D[NEW_CRS]]
    else
      val transformed = Geometry.transformCRS[CRS, NEW_CRS](jtsMP).asInstanceOf[jts.MultiPolygon]
      new MultiPolygon2D[NEW_CRS](transformed)

  def toWKT: String = jtsMP.toText

  protected def makeComponent(jtsG: jts.Polygon): Polygon2D[CRS] = Polygon2D[CRS](jtsG)

object MultiPolygon2D:
  private[geo] def fromJTS[CRS: CRST](jtsMP: jts.MultiPolygon): MultiPolygon2D[CRS] =
    new MultiPolygon2D[CRS](jtsMP)

  /**
    * Create a multipolygon out of a sequence of polygons by creating an Array of jts Polygons
    * from the innards and inflating a new JTS multipolygon
    * @param polys one or more polygons (which must be Polygon2D in the same CRS) to turn into a MultiPolygon
    * @tparam CRS the CRS of the Polygon provided, and hence the resulting Multipolygon also
    * @return A multipolygon created from the provided Polygon2Ds
    */
  def fromPolygons[CRS: CRST](polys: Seq[Polygon2D[CRS]]): MultiPolygon2D[CRS] =
    val polyArray: Array[Polygon] = polys.map(_.asJTS).collect { case p: jts.Polygon => p }.toArray
    val jtsMp = (new jts.GeometryFactory).createMultiPolygon(polyArray)
    new MultiPolygon2D[CRS](jtsMp)

  /**
    * Create a multipolygon out of a sequence of polygons by creating an Array of jts Polygons
    * from the innards and inflating a new JTS multipolygon
    * @param polys one or more polygons (which must be Polygon2D in the same CRS) to turn into a MultiPolygon
    * @param withId unique, stable String ID
    * @tparam CRS the CRS of the Polygon provided, and hence the resulting Multipolygon also
    * @return A multipolygon created from the provided Polygon2Ds
    */
  def fromPolygons[CRS: CRST](
      polys: Seq[Polygon2D[CRS]],
      withId: String): MultiPolygon2D[CRS] with GeometryWithID[CRS] =

    fromPolygons[CRS](polys).assignId(withId)
