package com.escalatesoft.geo

import com.escalatesoft.geo.crs.CRST
import com.escalatesoft.geo.crs.CRSTransform
import com.typesafe.scalalogging.LazyLogging
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.geom.{Coordinate, GeometryFactory, LinearRing, Polygon => JTSPolygon}
import org.locationtech.jts.operation.valid.{IsValidOp, TopologyValidationError}
import org.locationtech.jts.{geom => jts}
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.algorithm.CGAlgorithms
import org.locationtech.jts.algorithm.Area

/**
  * A Single 2D polygon (with possible holes). Should be constructed by one
  * of the factory methods in the Polygon2D companion object. This constructor
  * is private to prevent accidental usage.
  *
  * @param jtsPolygon JTS polygon to use as the basis
  * @tparam CRS CRST definition for the polygon
  */
class Polygon2D[CRS: CRST] private (jtsPolygon: JTSPolygon)
    extends Polygonal[CRS]
    with Validator[Polygon2D[CRS]]
    with LazyLogging:

  override protected val jtsGeom: jts.Polygon | jts.MultiPolygon = jtsPolygon

  def assignId(id: String): Polygon2D[CRS] with GeometryWithID[CRS] =
    new Polygon2D[CRS](jtsPolygon) with GeometryWithID[CRS]:
      override val geometryId: String = id

  override protected[geo] def asJTS: jts.Polygon = jtsPolygon

  override def transformCRS[NEW_CRS: CRST]: Polygon2D[NEW_CRS] =
    if CRSTransform.sameCRS[CRS, NEW_CRS] then this.asInstanceOf[Polygon2D[NEW_CRS]]
    else
      val transformed = Geometry.transformCRS[CRS, NEW_CRS](jtsPolygon).asInstanceOf[JTSPolygon]
      Polygon2D[NEW_CRS](transformed)

  /**
    * Return just the single exterior ring of the polygon as a closed sequence of
    * Point2Ds in the same CRS
    *
    * @return Vector of 2D points in the same CRS
    */
  def getExteriorRing: Vector[Point2D[CRS]] =
    jtsPolygon.getExteriorRing.getCoordinates.map(c => Point2D[CRS](c.x, c.y)).toVector

  private def getInteriorLinearRings: Vector[LineString] =
    val numRings = jtsPolygon.getNumInteriorRing
    val rings = for i <- 0 until numRings yield
      jtsPolygon.getInteriorRingN(i)
    rings.toVector

  /**
    * Return all of the interior rings (holes) in the current polygon as a sequence of closed sequences
    * of Point2D in the same CRS
    *
    * @return Vector of Vector of 2D points in the same CRS
    */
  def getInteriorRings: Vector[Vector[Point2D[CRS]]] =
    getInteriorLinearRings.map(_.getCoordinates.map(c => Point2D[CRS](c.x, c.y)).toVector)

  private def lineStringToLinearRing(ls: LineString): LinearRing =
    ls match
      case lr: LinearRing => lr
      case other =>
        val coords = other.getCoordinates()
        val closed = if coords(coords.length - 1) == coords(0) then coords else coords :+ coords(0)
        val gf = new GeometryFactory()
        gf.createLinearRing(closed)

  /**
    * filter all interior rings to remove any smaller than the threshold provided, used to clean up
    * polygons with tiny holes
    */
  def filterInteriorRingsByArea(areaThreshold: Double): Polygon2D[CRS] =
    val geometryFactory = new GeometryFactory()

    val interiorRings = getInteriorLinearRings.map(lineStringToLinearRing)
    val filtered =
      interiorRings
        .filter(lr => Area.ofRingSigned(lr.getCoordinateSequence).abs >= areaThreshold)
        .toArray

    val newJtsPoly =
      new JTSPolygon(lineStringToLinearRing(jtsPolygon.getExteriorRing), filtered, geometryFactory)
    Polygon2D[CRS](newJtsPoly)

  /**
    * The well known text for this polygon geometry
    *
    * @return String containing the WKT
    */
  override def toWKT: String = jtsPolygon.toText

  private def getValidationError: Option[TopologyValidationError] =
    Option(new IsValidOp(jtsPolygon).getValidationError)

  /**
    * Validate this geometry. Either return this item the validity violation in a union type
    *
    * @return a union type with this polygon or the invalidity reason
    */
  override def validated: TopologyValidationError | Polygon2D[CRS] =
    getValidationError.getOrElse(this)


object Polygon2D:

  /**
    * Create a Polygon2D based on a donor JTSPolygon and a given CRS definition
    * @param jtsPolygon The underlying JTS polygon to use
    * @tparam CRS CRST definition for the polygon
    * @return Polygon2D in the requested CRS
    */
  private[geo] def apply[CRS: CRST](jtsPolygon: JTSPolygon): Polygon2D[CRS] =
    new Polygon2D[CRS](jtsPolygon)

  /**
    * Create a Polygon2D using a sequence of raw (no-CRS) coordinate pairs and a given CRS definition.
    * This method only takes the outer polygon and cannot create a polygon with holes. There is
    * another factory taking holes below.
    *
    * This factory method will close a polygon automatically if it is not closed in the supplied sequence
    *
    * @param coords Sequence of RawCoordinate2D (with no attached CRS)
    * @param withId stable, unique String ID
    * @tparam CRS The CRST definition to use
    * @return Polygon2D in the requested CRS with no holes.
    */
  def apply[CRS: CRST](
      coords: Seq[RawCoordinate2D],
      withId: String
  ): Polygon2D[CRS] with GeometryWithID[CRS] =
    apply(coords, Nil, withId)

  /**
    * Create a Polygon2D using a sequence of raw (no-CRS) coordinate pairs and a given CRS definition.
    * This method only takes the outer polygon and cannot create a polygon with holes. There is
    * another factory taking holes below.
    *
    * This factory method will close a polygon automatically if it is not closed in the supplied sequence
    *
    * @param coords Sequence of RawCoordinate2D (with no attached CRS)
    * @tparam CRS The CRST definition to use
    * @return Polygon2D in the requested CRS with no holes.
    */
  def apply[CRS: CRST](coords: Seq[RawCoordinate2D]): Polygon2D[CRS] =
    apply(coords, Nil)

  private def makeLinearRing(
      coords: Seq[RawCoordinate2D],
      geometryFactory: GeometryFactory
  ): LinearRing =
    val closedPoly =
      if coords.isEmpty then coords
      else if coords.head == coords.last then coords
      else coords :+ coords.head
    val coordArray: Array[Coordinate] = closedPoly.toArray
    val coordinateSequence = new CoordinateArraySequence(coordArray)
    new LinearRing(coordinateSequence, geometryFactory)

  /**
    * Create a Polygon2D with holes using a sequence of raw (no-CRS) coordinate pairs for the outer ring and
    * another sequence of rings (also raw coords) for each of the holes in the polygon. All rings (outer or holes)
    * will be automatically closed if the sequence does not close.
    * @param outer The outer ring of the polygon as sequence of RawCoordinate2D (with no attached CRS)
    * @param holes The inner rings (holes) of the polygon as sequence of sequence of RawCoordinate2D
    * @param withId unique, stable String ID
    * @tparam CRS The CRST definition to use
    * @return Polygon2D with holes in the requested CRS
    */
  def apply[CRS: CRST](
      outer: Seq[RawCoordinate2D],
      holes: Seq[Seq[RawCoordinate2D]],
      withId: String
  ): Polygon2D[CRS] with GeometryWithID[CRS] = apply(outer, holes).assignId(withId)

  /**
    * Create a Polygon2D with holes using a sequence of raw (no-CRS) coordinate pairs for the outer ring and
    * another sequence of rings (also raw coords) for each of the holes in the polygon. All rings (outer or holes)
    * will be automatically closed if the sequence does not close.
    * @param outer The outer ring of the polygon as sequence of RawCoordinate2D (with no attached CRS)
    * @param holes The inner rings (holes) of the polygon as sequence of sequence of RawCoordinate2D
    * @tparam CRS The CRST definition to use
    * @return Polygon2D with holes in the requested CRS
    */
  def apply[CRS: CRST](
      outer: Seq[RawCoordinate2D],
      holes: Seq[Seq[RawCoordinate2D]]
  ): Polygon2D[CRS] =
    val geometryFactory = new GeometryFactory()
    val outerRing = makeLinearRing(outer, geometryFactory)
    val innerRings = holes.map(coords => makeLinearRing(coords, geometryFactory)).toArray
    val jtsPolygon = new JTSPolygon(outerRing, innerRings, geometryFactory)
    apply[CRS](jtsPolygon)

  private def pointsToRaw(points: Seq[Point2D[_]]): Seq[RawCoordinate2D] =
    points.map(p => RawCoordinate2D(p.x, p.y))

  /**
    * Create a Polygon2D without holes from a sequence of Point2Ds in the same CRS
    * @param outer Seq of Point2D in the same CRS as that requested for the polygon
    * @param withId unique, stable String ID
    * @tparam CRS CRST definition to use for the polygon
    * @return Polygon2D in the requested CRS (with no holes)
    */
  def fromPoints[CRS: CRST](
      outer: Seq[Point2D[CRS]],
      withId: String
  ): Polygon2D[CRS] with GeometryWithID[CRS] =
    apply(pointsToRaw(outer), withId)

  /**
    * Create a Polygon2D without holes from a sequence of Point2Ds in the same CRS
    * @param outer Seq of Point2D in the same CRS as that requested for the polygon
    * @tparam CRS CRST definition to use for the polygon
    * @return Polygon2D in the requested CRS (with no holes)
    */
  def fromPoints[CRS: CRST](
      outer: Seq[Point2D[CRS]]
  ): Polygon2D[CRS] = apply(pointsToRaw(outer))

  /**
    * Create a Polygon2D with holes from a sequence of Point2Ds in the same CRS for the outer
    * and a sequence of rings (also sequence of Point2Ds in the same CRS) for the holes.
    * @param outer Seq of Point2D in the same CRS as that requested for the polygon
    * @param holes Seq of Seq of Point2Ds in the same CRS
    * @param withId unique, stable String ID
    * @tparam CRS CRST definition to use for the polygon
    * @return Polygon2D in the requested CRS (with holes)
    */
  def fromPoints[CRS: CRST](
      outer: Seq[Point2D[CRS]],
      holes: Seq[Seq[Point2D[CRS]]],
      withId: String
  ): Polygon2D[CRS] with GeometryWithID[CRS] =
    apply(pointsToRaw(outer), holes.map(pointsToRaw)).assignId(withId)

  /**
    * Create a Polygon2D with holes from a sequence of Point2Ds in the same CRS for the outer
    * and a sequence of rings (also sequence of Point2Ds in the same CRS) for the holes.
    * @param outer Seq of Point2D in the same CRS as that requested for the polygon
    * @param holes Seq of Seq of Point2Ds in the same CRS
    * @tparam CRS CRST definition to use for the polygon
    * @return Polygon2D in the requested CRS (with holes)
    */
  def fromPoints[CRS: CRST](
      outer: Seq[Point2D[CRS]],
      holes: Seq[Seq[Point2D[CRS]]]
  ): Polygon2D[CRS] =
    apply(pointsToRaw(outer), holes.map(pointsToRaw))
