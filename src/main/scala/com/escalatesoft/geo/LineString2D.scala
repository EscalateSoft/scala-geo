package com.escalatesoft.geo

import com.escalatesoft.geo.crs.CRST
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.geom.{Coordinate, GeometryFactory, LineString => JTSLineString}
import org.locationtech.jts.{geom => jts}

/**
  * A single connected string of points in a given CRS.
  * @param jtsLineString the JTS geometry upon which this is based (hidden)
  * @tparam CRS the CRS of the 1D geometry
  */
class LineString2D[CRS: CRST] private (jtsLineString: JTSLineString) extends Lineal[CRS]:
  @inline override protected[geo] def asJTS: jts.LineString = jtsLineString

  override def transformCRS[NEW_CRS: CRST]: LineString2D[NEW_CRS] =
    val transformed = Geometry.transformCRS[CRS, NEW_CRS](jtsLineString).asInstanceOf[JTSLineString]
    LineString2D[NEW_CRS](transformed)

  def assignId(id: String): LineString2D[CRS] with GeometryWithID[CRS] =
    new LineString2D[CRS](jtsLineString) with GeometryWithID[CRS]:
      override val geometryId: String = id

  /**
    * Return a vector of the points making up this LineString, in the order
    * they are defined for the line
    * @return Vector of Point2Ds in the same CRS
    */
  def getPoints: Vector[Point2D[CRS]] =
    jtsLineString.getCoordinates.map(c => Point2D[CRS](c.x, c.y)).toVector

  override def toWKT: String = jtsLineString.toText

object LineString2D:
  private[geo] def apply[CRS: CRST](jtsLineString: JTSLineString): LineString2D[CRS] =
    new LineString2D[CRS](jtsLineString)

  /**
    * Construct a new LineString2D from a sequence of RawCoordinate2Ds and a given CRS
    * @param coords sequence of RawCoordinate2D (with no attached CRS)
    * @tparam CRS the CRS type to construct the LineString2D in
    * @return a new LineString2D from the CRS and coords provided
    */
  def apply[CRS: CRST](coords: Seq[RawCoordinate2D]): LineString2D[CRS] = //
    val geometryFactory = new GeometryFactory()
    val coordArray: Array[Coordinate] = coords.toArray
    val coordinateSequence = new CoordinateArraySequence(coordArray)
    val jtsLineString = new JTSLineString(coordinateSequence, geometryFactory)
    apply[CRS](jtsLineString)

  /**
    * Create a geometry with an assigned, stable, unique ID
    * @param coords sequence of RawCoordinate2D (with no attached CRS)
    * @param withId String containing stable, unique ID for geometry
    * @tparam CRS the CRS type to construct the LineString2D in
    * @return a new LineString2D from the CRS and coords provided
    */
  def apply[CRS: CRST](
    coords: Seq[RawCoordinate2D],
    withId: String
  ): LineString2D[CRS] with GeometryWithID[CRS] =
    apply(coords).assignId(withId)

  /**
    * Construct a new LineString2D from a sequence of Point2Ds in the same CRS
    * @param points the sequence of Point2Ds to include in the LineString
    * @tparam CRS The CRS of the points, and the resulting LineString
    * @return LineString2D of CRS with given points
    */
  def fromPoints[CRS: CRST](points: Seq[Point2D[CRS]]): LineString2D[CRS] =
    val coords = points.map(p => RawCoordinate2D(p.x, p.y))
    apply[CRS](coords)

  /**
    * Construct a new LineString2D from a sequence of Point2Ds in the same CRS
    * @param points the sequence of Point2Ds to include in the LineString
    * @param withId String containing a stable, unique ID
    * @tparam CRS The CRS of the points, and the resulting LineString
    * @return LineString2D of CRS with given points
    */
  def fromPoints[CRS: CRST](points: Seq[Point2D[CRS]],
                                    withId: String): LineString2D[CRS] with GeometryWithID[CRS] =
    val coords = points.map(p => RawCoordinate2D(p.x, p.y))
    apply[CRS](coords, withId)
