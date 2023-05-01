package com.escalatesoft.geo

import com.escalatesoft.geo.crs.CRST
import org.locationtech.jts.geom.Point
import org.locationtech.jts.{geom => jts}

/**
  * A collection of Point2Ds in the same CRS
  * @param points the sequence of Point2Ds in this MultiPoint2D
  * @tparam CRS CRS for the component points and for this MultiPoint2D
  */
class MultiPoint2D[CRS: CRST] private (val points: Seq[Point2D[CRS]])
    extends Puntal[CRS]
    with GeometryCollection[CRS, Point2D[CRS], jts.Point]:

  val crs: CRST[CRS] = summon[CRST[CRS]]

  override def transformCRS[NEW_CRS: CRST]: MultiPoint2D[NEW_CRS] =
    MultiPoint2D.fromPoints[NEW_CRS](points.map(p => p.transformCRS[NEW_CRS]))

  def assignId(id: String): MultiPoint2D[CRS] with GeometryWithID[CRS] =
    new MultiPoint2D[CRS](points) with GeometryWithID[CRS]:
      override val geometryId: String = id

  private def coordsStr: String = points.map(p => s"${p.x} ${p.y}").mkString(", ")
  def toWKT: String = s"MULTIPOINT($coordsStr)"

  import org.locationtech.jts.{geom => jts}
  override protected[geo] lazy val asJTS: jts.MultiPoint =
    val geometryFactory = new jts.GeometryFactory()
    val pointArray: Array[jts.Point] = points
      .map(p => p.asJTS)
      .collect { case p: jts.Point => p }
      .toArray
    new jts.MultiPoint(pointArray, geometryFactory)

  protected def makeComponent(jtsG: Point): Point2D[CRS] = Point2D[CRS](jtsG.getX, jtsG.getY)

  // for efficiency, don't go and create a whole bunch of new ones
  override def componentGeometries: Seq[Point2D[CRS]] = points

object MultiPoint2D:
  import org.locationtech.jts.{geom => jts}

  private[geo] def fromJTSCoords[CRS: CRST](raw: Seq[jts.Coordinate]): MultiPoint2D[CRS] =
    new MultiPoint2D[CRS](raw.map(r => Point2D[CRS](r.x, r.y)))

  /**
    * Factory method to construct a MultiPoint2D from a sequence of raw coordinates (no CRS)
    * and a given CRS type.
    * @param raw the sequence of RawCoordinate2Ds
    * @tparam CRS the CRS for the resulting MultiPoint2D
    * @return a MultiPoint2D with the given coordinates and CRS
    */
  def apply[CRS: CRST](raw: Seq[RawCoordinate2D]): MultiPoint2D[CRS] =
    fromJTSCoords(raw)

  /**
    * Factory method to construct a MultiPoint2D from a sequence of raw coordinates (no CRS)
    * and a given CRS type.
    * @param raw the sequence of RawCoordinate2Ds
    * @param withId unique, stable String ID
    * @tparam CRS the CRS for the resulting MultiPoint2D
    * @return a MultiPoint2D with the given coordinates and CRS
    */
  def apply[CRS: CRST](
    raw: Seq[RawCoordinate2D],
    withId: String
  ): MultiPoint2D[CRS] with GeometryWithID[CRS] =
    fromJTSCoords(raw).assignId(withId)

  /**
    * Construct a MultiPoint2D from a sequence of Point2Ds in the same CRS
    * @param points sequence of Point2Ds
    * @tparam CRS the CRS type of the Point2Ds, and the resulting MultiPoint2D
    * @return new MultiPoint2D with the points and CRS given
    */
  def fromPoints[CRS: CRST](points: Seq[Point2D[CRS]]): MultiPoint2D[CRS] =
    new MultiPoint2D[CRS](points)

  /**
    * Construct a MultiPoint2D from a sequence of Point2Ds in the same CRS
    * @param points sequence of Point2Ds
    * @param withId optional Id for the linestrings, None by default
    * @tparam CRS the CRS type of the Point2Ds, and the resulting MultiPoint2D
    * @return new MultiPoint2D with the points and CRS given
    */
  def fromPoints[CRS: CRST](
    points: Seq[Point2D[CRS]],
    withId: String
  ): MultiPoint2D[CRS] with GeometryWithID[CRS] =
    fromPoints(points).assignId(withId)
