package com.escalatesoft.geo

import com.escalatesoft.geo.crs.CRST
import com.escalatesoft.geo.crs.CRST.CRSDefinitions.EPSG_4326
import com.typesafe.scalalogging.LazyLogging
import org.locationtech.jts.operation.union.CascadedPolygonUnion
import org.locationtech.jts.simplify.TopologyPreservingSimplifier
import org.locationtech.jts.{geom => jts}
import org.scalactic.Requirements._

import scala.util.Try
import scala.util.control.NonFatal

/**
  * A super class category for anything Polygonal in nature (i.e. covering an area
  * or 2D in space). Known subclasses are Polygon2D and MultiPolygon2D.
  *
  * This geometry type allows treatment of any of its subtypes with a number of
  * useful, area based abstractions, and it is quite common to use this superclass
  * instead of either Polygon or MultiPolygon specific classes since mostly you
  * don't care which one you have at runtime.
  *
  * @tparam CRS CRS type of the Polygonal geometry
  */
abstract class Polygonal[CRS: CRST] extends Geometry[CRS] with LazyLogging:
  import Polygonal.makePolygon

  protected val jtsGeom: jts.Polygon | jts.MultiPolygon
  def assignId(id: String): Polygonal[CRS] with GeometryWithID[CRS]

  /**
    * The default tolerance for polygonals is based on the CRS and an area allowance
    * rather than an allowance for individual points.
    */
  @inline override def defaultTolerance: Double = ccrs.areaTolerance

  @inline override protected[geo] def asJTS: jts.Geometry & jts.Polygonal = genericGeom
  private[this] lazy val genericGeom: jts.Geometry & jts.Polygonal =
    jtsGeom match
      case p: jts.Polygonal => p

  /**
    * Return the centroid of the current polygon, as specified by JTS.
    *
    * See [[http://bjornharrtell.github.io/jsts/1.1.2/apidocs/org/locationtech/jts/algorithm/Centroid.html JTS Centroid]]
    * @return Centroid of polygon as Point2D in the same CRS
    */
  def centroid: Point2D[CRS] =
    val jtsCentroid = genericGeom.getCentroid
    Point2D[CRS](jtsCentroid.getX, jtsCentroid.getY)

  /**
    * For polygons the convex hull should always be a polygon 2d result, if it's not
    * that is an exceptional outcome.
    * @return the convex hull as a Polygon2D in the same CRS
    */
  override def convexHull: Polygon2D[CRS] =
    requireState(isValid, "Attempt to obtain convex hull over invalid polygon is unsupported")
    super.convexHull match
      case p: Polygon2D[CRS] => p
      case _ =>
        throw new IllegalStateException("Convex hull operation did not produce a valid polygon")

  /**
    * Return the area of the polygon using CRS units.
    *
    * Note that this will not provide useful values for some CRSs like WGS84 since
    * the resulting value will be in square degrees of latitude/longitude (which can
    * also vary by latitude). This is useful as the basis for more controlled area
    * methods, and for comparisons and sorting by size, hence it is private to the
    * package.
    * @return Double area in square CRS units
    */
  private[geo] lazy val area: Double = genericGeom.getArea

  /**
    * Expose the area in the units for this CRS which may be, e.g
    * square degrees instead of meters or something more useful
    * however the metric is still handy for sorting by area or
    * for checking if the area is actually 0.0 (empty)
    * This is exposed with an alias where the name makes it clear what you are getting
    *
    * return Double area in square CRS units
    */
  def areaInCRSUnits: Double = area

  /**
    * Return a simplified geometry based on the geotools simplification algorithm
    * and threshold supplied. This removes points from polygons and lines if they
    * will not alter the overall polygon beyond the arbitrary threshold value.
    *
    * see [[https://locationtech.github.io/jts/javadoc/org/locationtech/jts/simplify/TopologyPreservingSimplifier.html]]
    *
    * @param threshold the distanceThreshold for the TopologyPreservingSimplifier
    * @return A simplified Geometry
    */
  def simplifyTopology(threshold: Double): Polygonal[CRS] =
    try
      val simplified = TopologyPreservingSimplifier.simplify(genericGeom, threshold)
      Geometry.fromJTS[CRS](simplified) match
        case p: Polygon2D[CRS]       => p
        case mp: MultiPolygon2D[CRS] => mp
    catch
      case NonFatal(ex) =>
        logger.warn(s"Unable to simplify Polygon, returning original", ex)
        this

  /**
    * Return the geometry that results from intersecting this geometry with
    * the given one.
    *
    * If the intersection is empty, or otherwise cannot be represented by
    * polygonal, the result will be a Failed Try.
    *
    * @param thatGeometry the other geometry to intersect
    * @return A Try of the new polygonal in the same CRS representing the intersection
    */
  def intersection(thatGeometry: Polygonal[CRS]): Try[Polygonal[CRS]] = Try {
    val intersection = genericGeom.intersection(thatGeometry.asJTS)
    makePolygon(Geometry.fromJTS[CRS](intersection))
  }

  /**
    * Check if the Geometry sub-type provided is within the geometry, return true
    * if so, false otherwise
    * @param geom Geometry in the same CRS as this geometry
    * @return true if the Geometry is contained, false otherwise
    */
  def contains(geom: Geometry[CRS]): Boolean =
    genericGeom.contains(geom.asJTS)

  /**
    * Check if the given geometry is covered by this polygonal. This differs
    * from contains in that the geometries can touch at the edges, but the
    * given geometry must not go outside of this geometry at all or the result
    * will be false.
    * @param thatGeometry the geometry to check is contained within this one
    * @return true if the given geometry is covered by this one, false otherwise
    */
  def covers(thatGeometry: Geometry[CRS]): Boolean =
    genericGeom.covers(thatGeometry.asJTS)

  /**
    * Return the difference between two polygonals, that is effectively whatever
    * is in this geometry but is not in the other one (subtraction)
    *
    * @param polygonal the other polygonal to difference
    * @return A Try of a new polygonal in the same CRS with the result of the difference
    */
  def difference(polygonal: Polygonal[CRS]): Try[Polygonal[CRS]] = Try {
    val difference = genericGeom.difference(polygonal.asJTS)
    makePolygon(Geometry.fromJTS[CRS](difference))
  }

  /**
    * also @see [[Polygonal.unionAll]]
    *
    * Combine this polygonal with another by unioning the areas together, resulting in
    * another polygonal (which could be either Polygon or MultiPolygon depending on the
    * operation).
    *
    * @param other the other polygonal geometry to union with this one
    * @return Try of Polygonal in the same CRS with the union of the two polygonals
    */
  def union(other: Polygonal[CRS]): Try[Polygonal[CRS]] = Try {
    val union = genericGeom.union(other.asJTS)
    makePolygon(Geometry.fromJTS[CRS](union))
  }

  /**
    * Convert any polygonal to a multi polygon using the following heuristic:
    * If it is already a multipolygon just return that
    * If it is an ordinary polygon, inflate it to a multipolygon with 1 polygon in it
    * @return A Multipolygon in the same CRS as the Polygonal
    */
  def asMultiPolygon: MultiPolygon2D[CRS] = this match
    case p: Polygon2D[CRS]       => MultiPolygon2D.fromPolygons[CRS](Seq(p))
    case mp: MultiPolygon2D[CRS] => mp

  /**
    * For any polygonal, return a sequence of Polygon2Ds, using the following heuristic:
    * If it is a single polygon, give back that single item in a 1 length seq
    * If it is a multi polygon, return each of the component polygons as a Polygon2D in a sequence
    * @return A Seq[Polygon2D] of the component polygons in the multipolygon
    */
  def asPolygonSeq: Seq[Polygon2D[CRS]] = this match
    case p: Polygon2D[CRS]       => Seq(p)
    case mp: MultiPolygon2D[CRS] => mp.componentGeometries

  /**
    * Return true if the given geometry intersects with this geometry anywhere
    * or false otherwise
    * @param geom the other geometry to test for intersection
    * @return true if there is an intersection, false otherwise
    */
  def intersects(geom: Geometry[CRS]): Boolean =
    asJTS.intersects(geom.asJTS)


object Polygonal:

  /**
    * Given a sequence of polygonals, union them all together and produce a new polygonal
    * which may end up being either a Polygon or a Multipolygon, in the same CRS.
    * This operation may fail for various reasons, and hence a Try of the Polygonal is returned
    * @param polys Sequence of Polygonals all in the same CRS to union together
    * @tparam CRS The CRST that all the polygonals must be in, and the CRS of the result
    * @return A Try of a single polygonal (maybe Polygon or MultiPolygon) with the result of
    *         unioning all of the polygonals
    */
  def unionAll[CRS: CRST](polys: Seq[Polygonal[CRS]]): Try[Polygonal[CRS]] = Try {
    import scala.jdk.CollectionConverters._
    require(!polys.isEmpty, "at least one polygonal needed for unionAll, got zero")
    val jts = polys.map(_.asJTS)
    val unioned = new CascadedPolygonUnion(jts.asJava).union()
    makePolygon(Geometry.fromJTS[CRS](unioned))
  }

  private def makePolygon[CRS: CRST](g: Geometry[CRS]): Polygonal[CRS] =
    g match
      case p: Polygonal[CRS] => p
      case _                 => throw new IllegalStateException("Difference resulted in non-polygonal geometry")

  /**
    * Create a polygonal from a set of geohashes. This may return a single or multi polygon that will
    * contain the union of all of the geohash areas in the set.
    * @param geohashes Set of geohashes to make the polygonal from
    * @return A polygonal in EPSG:4326 containing the areas described by the geohashes
    */
  def fromGeohashSet(geohashes: Set[String]): Try[Polygonal[EPSG_4326]] =
    Try {
      val polygons = geohashes.toSeq.map(h => Envelope2D.fromGeohash(h).get.boundsPolygon)
      unionAll(polygons)
    }.flatten

