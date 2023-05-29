package com.escalatesoft.geo

import com.escalatesoft.geo.crs.CRST.CRSDefinitions.EPSG_4326
import com.escalatesoft.geo.crs.CRST
import org.geotools.geometry.DirectPosition2D
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.GeodeticCalculator
import org.opengis.geometry.DirectPosition
import com.escalatesoft.geo.crs.CRSTransform
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.{geom => jts}
import Units._  

/**
  * A 2D Point.  Should be constructed by one
  * of the factory methods in the Polygon2D companion object. This constructor
  * is private to prevent accidental usage.
  * @param x the X coordinate
  * @param y the Y coordinate
  * @tparam CRS CRST definition for the polygon
  */
case class Point2D[CRS: CRST] private (x: Double, y: Double)
    extends Puntal[CRS]
    with Ordered[Point2D[CRS]]:

  val crs: CRST[CRS] = summon[CRST[CRS]]

  override def assignId(id: String): Point2D[CRS] & GeometryWithID[CRS] =
    new Point2D[CRS](x, y) with GeometryWithID[CRS]:
      override val geometryId: String = id

  /**
    * Transform the CRS of Point2D object.
    * @tparam NEW_CRS target CRS
    * @return Point2D object in a new CRS
    */
  override def transformCRS[NEW_CRS: CRST]: Point2D[NEW_CRS] =
    val transform = CRSTransform[CRS, NEW_CRS]()
    val newCoord = JTS.transform(new jts.Coordinate(x, y), new jts.Coordinate(), transform)
    Point2D[NEW_CRS](newCoord.x, newCoord.y)

  override protected[geo] def asJTS: jts.Point =
    Point2D.jtsFactory.createPoint(RawCoordinate2D(x, y))

  /**
    * Covert Point2D to a DirectPosition2D object
    */
  lazy val toDirectPosition: DirectPosition2D =
    new DirectPosition2D(crs.crs, x, y)

  /**
    * Compare with another Point2D
    * @param that Point2D object to compare with
    */
  def compare(that: Point2D[CRS]): Int =
    summon[Ordering[(Double, Double)]].compare((x, y), (that.x, that.y))

  /**
    * Returns Point2D in a different CRS.
    * @tparam NEW_CRS target CRS
    * @return Point 2D in the target CRS
    */
  def toCRS[NEW_CRS: CRST]: Point2D[NEW_CRS] = this.transformCRS[NEW_CRS]

  /**
    * Returns the well-known string for Point2D object
    * @return String containing the WKT
    */
  def toWKT: String = s"""POINT ($x $y)"""

  /**
    * Returns a Point2D with a function applied on its coordiantes
    * @param f  a function from a double to another double
    * @return  Point2D object with the function applied
    */
  def map(f: Double => Double): Point2D[CRS] =
    Point2D[CRS](f(x), f(y))

  /**
    * Returns an updated copy of Point2D with assigned x and y coordinates.
    * Like copy, but doesn't affect the CRS type or implicit.
    * @param x A double representing the x-coordinate  (default value is current Point2D's x-coordinate)
    * @param y A double representing the y-coordinate  (default value is current Point2D's y-coordinate)
    * @return Point2D with given x and y coordinate
    */
  def updated(x: Double = this.x, y: Double = this.y): Point2D[CRS] =
    Point2D(x, y)

  private[geo] def isWGS84UtmZoneCRS: Boolean =
    crs.crsId match
      case s 
        if s.startsWith("EPSG:3260") |
           s.startsWith("EPSG:3261") |
           s.startsWith("EPSG:3262") |
           s.startsWith("EPSG:3263") |
           s.startsWith("EPSG:3264") |
           s.startsWith("EPSG:3265") |
           s.startsWith("EPSG:32660") |
           s.startsWith("EPSG:3270") |
           s.startsWith("EPSG:3271") |
           s.startsWith("EPSG:3272") |
           s.startsWith("EPSG:3273") |
           s.startsWith("EPSG:3274") |
           s.startsWith("EPSG:3275") |
           s.startsWith("EPSG:32760") => true
      case _ => false

  /**
    * Produce the "best" UTM CRS at this point in space. If the point is already in UTM coordinates this will
    * return the UTM CRS it's already in (e.g. [[CRS]]). Otherwise, it will convert to EPSG_4326 and look up the
    * best UTM zone for this point.
    */
  def bestUtmCRS: CRST.SomeCRS =
    if isWGS84UtmZoneCRS then
      new CRST.SomeCRS {
        type SOME_CRS = CRS
        given SOME_CRS: CRST[SOME_CRS] = crs
      }
    else CRST.crsFromId(CRST.epsgUtmCodeForZoneNumber(bestUtmCRSCode))

  /**
   * Find the EPSG code for the best UTM CRS at this point in space, e.g. 15N
   * Take your longitude coordinate in decimal degrees and add 180.
   * Then divide by 6.
   * Finally round-up to the next highest whole number.
   * Append N if north of the equator, S if south.
   */
  def bestUtmCRSCode: String =
    val transformed = toCRS[EPSG_4326]
    val zoneNumber = (((transformed.x + 180) / 6).toInt + 1).min(60)
    val hemisphere = if transformed.y >= 0 then "N" else "S"
    // pad the zone number with a leading zero if it's less than 10
    f"$zoneNumber%02d$hemisphere"

  /**
    * Converts a point to a rectangle Polygon2D centered around the point
    * @param width Length object that represents width of the rectangle
    * @param height Length object that represents height of the rectangle
    * @return Polygon2D object with a given width and height centered at the point
    */
  def toRectangle(width: Meters, height: Meters): Polygon2D[CRS] =
    val bestUTM: CRST.SomeCRS = bestUtmCRS
    import bestUTM.{SOME_CRS => BestUTM}

    val utm = toCRS[BestUTM]

    Polygon2D.fromPoints[CRS](
      List(
        Point2D[BestUTM](utm.x + width.toMeters / 2, utm.y + height.toMeters / 2).toCRS[CRS],
        Point2D[BestUTM](utm.x - width.toMeters / 2, utm.y + height.toMeters / 2).toCRS[CRS],
        Point2D[BestUTM](utm.x - width.toMeters / 2, utm.y - height.toMeters / 2).toCRS[CRS],
        Point2D[BestUTM](utm.x + width.toMeters / 2, utm.y - height.toMeters / 2).toCRS[CRS]
      ))

  /**
    * Converts a point to a square Polygon2D centered around the point
    * @param sideLength Length object that represents width of the square
    * @return Polygon2D object with a given width and height centered at the point
    */
  def toSquare(sideLength: Meters): Polygon2D[CRS] =
    toRectangle(sideLength, sideLength)

  /**
    * Simple pythagorean square of distance between two points in the same CRS
    * Result will be in whatever units the CRS uses for axes and this is not orthographically
    * correct. For orthographic distance, use distanceToOrtho instead.
    * Note since this is used for fast sorting, the result is kept squared, take a square
    * root yourself if you need approx distance (or use distanceToOrtho)
    * @param p1 Coordinate to calculate distance to
    * @return Pythagorean distance in CRS axis units
    */
  def distanceSquaredTo(p1: Point2D[CRS]): Double =
    val dx = p1.x - x
    val dy = p1.y - y

    (dx * dx) + (dy * dy)

  /**
    * Distance between two points in the same CRS, in the same units as the CRS axes.
    * The result is calculated using the orthodromic distance in the geodetic calculator
    * so will be more accurate over large areas, although slower than the pythagorean
    * distance from distanceSquaredTo.
    * @param p1 Coordinate 1
    * @return Ortho distance in CRS axis units
    */
  def distanceOrthoTo(p1: Point2D[CRS]): Double =
    val crs = summon[CRST[CRS]].crs
    val gc = new GeodeticCalculator(crs)
    gc.setStartingPosition(new DirectPosition2D(crs, p1.x, p1.y))
    gc.setDestinationPosition(new DirectPosition2D(crs, x, y))
    gc.getOrthodromicDistance


object Point2D:
  def apply[CRS: CRST](x: Double, y: Double): Point2D[CRS] = new Point2D(x, y)

  private lazy val jtsFactory: GeometryFactory = new jts.GeometryFactory()

  /**
    * Factory method to construct a Point2D
    * @param x Double representing the x-coordiante
    * @param y Double representing the y-coordinate
    * @tparam CRS CRS of the point
    * @return Point2D with given coordinates in some CRS
    */
  def apply[CRS: CRST](x: Int, y: Int): Point2D[CRS] = new Point2D[CRS](x, y)

  /**
    * Returns a point with new coordinates transformed from some CRS to a different CRS.
    * @param x Double representing the x-coordinate
    * @param y Double representing the y-coordinate
    * @tparam OLD_CRS the original CRS to convert from
    * @tparam NEW_CRS target CRS
    * @return Point2D object with x and y coordiantes in the new CRS
    */
  private[this] def innerTransformTo[OLD_CRS: CRST, NEW_CRS: CRST](
      x: Double,
      y: Double): Point2D[NEW_CRS] =
    import org.locationtech.jts.geom.Coordinate
    import org.geotools.geometry.jts.JTS

    val lenient: Boolean = true

    val transform = CRSTransform[OLD_CRS, NEW_CRS](lenient)

    val newCoord = JTS.transform(new Coordinate(x, y), new Coordinate(), transform)
    Point2D[NEW_CRS](newCoord.x, newCoord.y)

  /**
    * Returns a Point2D using a DirectPosition object in some target CRS
    * @param from  DirectPosition object representing a point. Should have dimension 2.
    * @tparam TARGET_CRS Desired CRS for Point2D
    * @return Point2D in the target CRS.
    */
  def fromDirectPosition[TARGET_CRS: CRST](from: DirectPosition): Point2D[TARGET_CRS] =
    require(from.getDimension == 2,
            s"Provided is ${from.getDimension} dimensional, position must be two dimensional")
    import org.geotools.referencing.{CRS => GCRS}

    val crs = summon[CRST[TARGET_CRS]].crs

    require(
      GCRS.equalsIgnoreMetadata(crs, from.getCoordinateReferenceSystem),
      s"Incompatible CRS, requested: ${crs.getName}, supplied ${from.getCoordinateReferenceSystem.getName}"
    )

    if GCRS.getAxisOrder(from.getCoordinateReferenceSystem) == GCRS.AxisOrder.EAST_NORTH then // x,y ordering
      Point2D[TARGET_CRS](from.getCoordinate.apply(0), from.getCoordinate.apply(1))
    else
      Point2D[TARGET_CRS](from.getCoordinate.apply(1), from.getCoordinate.apply(0))

  /**
    * Returns a Point2D from a DirectPosition2D object in some target CRS
    * @param from  DirectPosition2D object representing a point.
    * @tparam TARGET_CRS Desired CRS for Point2D
    * @return Point2D in the target CRS.
    */
  def fromDirectPositionAnyCRS[TARGET_CRS: CRST](
      from: DirectPosition2D): Point2D[TARGET_CRS] =
    val crs = from.getCoordinateReferenceSystem

    val fromCRS = CRST.crsFromGeotools(crs)
    import fromCRS.SOME_CRS

    innerTransformTo[SOME_CRS, TARGET_CRS](from.x, from.y)

