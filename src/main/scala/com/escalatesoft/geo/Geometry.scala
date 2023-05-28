package com.escalatesoft.geo

import java.util.UUID

import com.escalatesoft.geo.crs.CRST.CRSDefinitions.EPSG_4326
import com.escalatesoft.geo.crs.{CRSTransform, CRST}
import com.github.davidmoten.geo.GeoHash
import org.locationtech.jts.geom.{Coordinate, Geometry => JTSGeometry}
import org.locationtech.jts.io.{WKBReader, WKBWriter, WKTReader}
import org.geotools.geometry.jts.JTS
import org.scalactic.Tolerance._
import org.scalactic.TypeCheckedTripleEquals._

abstract class Geometry[CRS](using protected val ccrs: CRST[CRS]):

  @inline def defaultTolerance: Double

  /**
    * Assign a new ID to this Geometry, likely obtained from the stable UUID lookup
    * but can be anything (as long as it is unique for this geometry)
    */
  def assignId(id: String): Geometry[CRS] with GeometryWithID[CRS]

  /**
    * Return true if the geometry is considered valid (as defined by JTS) or false otherwise
    * @return true if the geometry reports back as valid, false otherwise
    */
  def isValid: Boolean = asJTS.isValid

  /**
    * Transform this geometry to a new CRS, calculating the mathematical transform
    * and updating the coordinates in addition to changing the CRS type parameter
    * in the returned geometry.
    * @tparam NEW_CRS The new CRS type to transform this geometry to
    * @return The same type of geometry in the requested CRS
    */
  def transformCRS[NEW_CRS: CRST]: Geometry[NEW_CRS]

  /**
    * Provide the standard WKT string for this geometry
    * @return String containing the WKT
    */
  def toWKT: String

  /** Extended Well-Known Text for this geometry */
  def toEWKT: String =
    val crs = ccrs.crs
    import org.geotools.referencing.{CRS => GCRS}
    s"SRID=${GCRS.lookupEpsgCode(crs, true)};$toWKT"

  /** Well-Known Binary */
  def toWKB: Array[Byte] =
    val writer = new WKBWriter()
    writer.write(asJTS)

  protected[geo] def asJTS: JTSGeometry

  /**
    * toString representation defaults to the WKT for a geometry
    * @return the WKT for this geometry
    */
  override def toString: String = toWKT

  def canEqual(other: Any): Boolean = other.isInstanceOf[Geometry[_]]

  override def equals(other: Any): Boolean = other match
    case that: Geometry[_] if this eq that => true
    case that: Geometry[_] =>
      (that canEqual this) &&
        ccrs.crs.equals(ccrs.crs) &&
        asJTS == that.asJTS
    case _ => false

  private[this] lazy val _hashCode: Int =
    31 * {
      31 + asJTS.##
    } + ccrs.##

  override def hashCode(): Int = _hashCode

  /**
    * Get the convex hull of this geometry (the minimum area convex polygon that contains all points
    * in this geometry)
    * @return the convex hull as a Polygon2D in the same CRS
    */
  def convexHull: Geometry[CRS] =
    val jch = asJTS.convexHull
    Geometry.fromJTS(jch)

  /**
    * The minimum envelope, in the same CRS as this geometry, that completely covers the geometry.
    * The envelope will be rectangular and aligned to the CRS. For single points, the min and max
    * coords will correspond to the original point values.
    * @return Envelope2D in the same CRS as the geometry
    */
  def boundingEnvelope: Envelope2D[CRS] =
    val jtsEnvelope = asJTS.getEnvelope
    val allCoords: Array[Coordinate] = jtsEnvelope.getCoordinates
    val first = allCoords.head
    var minX, maxX = first.x
    var minY, maxY = first.y

    var idx = 1
    val limit = allCoords.length

    while idx < limit do
      val next = allCoords(idx)
      minX = minX min next.x
      maxX = maxX max next.x
      minY = minY min next.y
      maxY = maxY max next.y

      idx += 1

    require(minX <= maxX && minY <= maxY, s"Invalid envelope detection: ($minX, $minY, $maxX, $maxY)")
    Envelope2D[CRS](Point2D[CRS](minX, minY), Point2D[CRS](maxX, maxY))

  /**
    * For another geometry and a given tolerance (to allow for rounding errors), check if
    * this geometry and the other geometry are equal within that given numerical tolerance
    * @param otherGeom the other geometry to compare with this, Geometries of different types are
    *                  automatically considered nonequal
    * @param tolerance The numeric tolerance to allow between values, for points/lines this is the maximum
    *                  distance allowed, for polygons this is the area difference
    * @return true if the geometries are equal within specified tolerance, false otherwise
    */
  def equalsWithinTolerance(otherGeom: Geometry[CRS], tolerance: Double = defaultTolerance): Boolean =
    Geometry.equalsWithinTolerance(this, otherGeom, tolerance)

object Geometry:

  private[geo] def transformCRS[OLD_CRS: CRST, NEW_CRS: CRST](jtsGeometry: JTSGeometry): JTSGeometry =
    val transform = CRSTransform[OLD_CRS, NEW_CRS]()
    JTS.transform(jtsGeometry, transform)

  private[geo] def fromJTS[CRS: CRST](jtsg: JTSGeometry): Geometry[CRS] =
    import org.locationtech.jts.{geom => jts}
    jtsg match
      case p: jts.Point             => Point2D[CRS](p.getX, p.getY)
      case mp: jts.MultiPoint       => MultiPoint2D.fromJTSCoords[CRS](mp.getCoordinates.toIndexedSeq)
      case ls: jts.LineString       => LineString2D[CRS](ls)
      case mls: jts.MultiLineString => MultiLineString2D.fromJTS[CRS](mls)
      case p: jts.Polygon           => Polygon2D[CRS](p)
      case mp: jts.MultiPolygon     => MultiPolygon2D.fromJTS[CRS](mp)
      case _ =>
        throw new IllegalStateException(s"Unhandled JTS Geometry: ${jtsg.getClass.getSimpleName}")

  /**
    * Create a geometry with the given CRS from the provided WKT string. This is limited to constructing
    * only one of the following geometries:
    *
    * * Point
    * * MultiPoint
    * * LineString
    * * MultiLineString
    * * Polygon
    * * MultiPolygon
    *
    * and it will return 2D versions of the above only (which can be disambiguated by pattern matching).
    *
    * @param wkt The WKT string to build the geometry from
    * @tparam CRS The CRS to attach to the resulting geometry
    * @return Geometry sub-type corresponding to the parsed WKT
    */
  def fromWKT[CRS: CRST](wkt: String): Geometry[CRS] =
    val reader = new WKTReader()
    val jtsGeom = reader.read(wkt)
    fromJTS[CRS](jtsGeom)

  /**
    * Create a geometry with the given CRS from the provided well known bytes. See fromWKT.
    */
  def fromWKB[CRS: CRST](wkb: Array[Byte]): Geometry[CRS] =
    val reader = new WKBReader()
    val jtsGeom = reader.read(wkb)
    fromJTS[CRS](jtsGeom)

  private[geo] def idFromWKT(wkt: String): String =
    uuidFromWKT(wkt).toString

  private[geo] def uuidFromWKT(wkt: String): UUID =
    UUID.nameUUIDFromBytes(wkt.getBytes())

  /**
    * Generate a unique, and hopefully somewhat stable, ID from the WKT of a geometry
    * @param geometry the geometry to return a UUID for
    * @return UUID string based on geometry WKT
    */
  def idFromGeometryWKT(geometry: Geometry[_]): String =
    idFromWKT(geometry.toWKT)

  /**
    * Generate a unique, and hopefully somewhat stable, ID from the WKT of a geometry
    * @param geometry the geometry to return a UUID for
    * @return UUID based on geometry WKT
    */
  def uuidFromGeometryWKT(geometry: Geometry[_]): UUID =
    uuidFromWKT(geometry.toWKT)

  /**
    * Return a single geohash to cover the given geometry up to the maximum precision length specified)
    * (note that large features will yield lower precision
    * due to the nature of a single geohash needed to cover the full area)
    * @param geometry the geometry to geohash over
    * @param precision maximum string length in resulting geohash (default 12)
    * @return a single geohash string up to the specified length
    */
  def singleGeohash(geometry: Geometry[EPSG_4326], precision: Int = 12): String =
    val bb = geometry.boundingEnvelope
    val bbhash = GeoHash.coverBoundingBoxMaxHashes(
      bb.maxDiag.y,
      bb.minDiag.x,
      bb.minDiag.y,
      bb.maxDiag.x,
      1
    )
    require(bbhash.getHashes.size == 1, s"Unable to get single geohash for $this")
    bbhash.getHashes.iterator().next().take(precision)

  /**
    * Return a set of geohashes of the given string length (e.g. 4,5,6 characters, etc). The
    * set will completely cover the bounding box. See https://en.wikipedia.org/wiki/Geohash for
    * information on the relationship between string length and geohash resolution.
    * @param length the string length to use for the generated geohash(es). There is little point
    *               in providing a number higher than 12 here as that is about 5cm precision on
    *               the surface of the earth.
    * @return
    */
  def geohashSet(geometry: Geometry[EPSG_4326], length: Int, refine: Boolean = true): Set[String] =
    import scala.jdk.CollectionConverters._
    require(length <= 12, "Geohashes longer than 12 are not supported lest it generate too many results")
    val bb = geometry.boundingEnvelope
    val bbhash = GeoHash.coverBoundingBox(
      bb.maxDiag.y,
      bb.minDiag.x,
      bb.minDiag.y,
      bb.maxDiag.x,
      length
    )
    val bbSet = Set.empty ++ bbhash.getHashes.asScala

    refineGeohashSet(geometry, refine, bbSet)

  private def refineGeohashSet(geometry: Geometry[EPSG_4326], refine: Boolean, bbSet: Set[String]) =
    if !refine then bbSet
    else
      bbSet.filter { h =>
        val geohashTry = Envelope2D.fromGeohash(h)
        geohashTry.isSuccess && geohashTry.get.boundsPolygon.intersects(geometry)
      }

  /**
    * Return a default set of up to 25 geohashes (usually much lower) that covers the
    * geometry given in its entirety and optionally refines so that only geohash cells
    * that actually intersect are returned. The length of the geohash strings will vary
    * based on what length is needed to represent the feature in 25 geohashes or fewer
    *
    * @param geometry the geometry to geohash
    * @param refine true if you want to clear out geohash cells that do not intersect the feature
    * @param maxHashes the maximum number of hashes to include in the set, defaults to 25
    * @return Set of strings containing the geohashes covering the feature given
    */
  def geohashDefaultSet(geometry: Geometry[EPSG_4326], refine: Boolean = true, maxHashes: Int = 25): Set[String] =
    import scala.jdk.CollectionConverters._

    val bb = geometry.boundingEnvelope
    val bbhash = GeoHash.coverBoundingBoxMaxHashes(
      bb.maxDiag.y,
      bb.minDiag.x,
      bb.minDiag.y,
      bb.maxDiag.x,
      maxHashes
    )
    val bbSet = Set.empty ++ bbhash.getHashes.asScala

    refineGeohashSet(geometry, refine, bbSet)

  /**
    * Compare two geometries for equality allowing tolerance to cope with
    * rounding differences but still consider the geometries equal.
    *
    * This form takes an explicit tolerance. To use a default tolerance,
    * call equalsWithinTolerance as a method directly on a geometry
    * and it will use the default tolerance for that geometry/CRS
    *
    * @param geom1 geometry 1 to compare
    * @param geom2 geometry 2 to compare
    * @param tolerance numeric tolerance to allow for small differences. This
    *                  value will depend on the geometry and CRS types involved.
    *                  To use defaults, call the equalsWithinTolerance method on
    *                  a geometry directly and use the default tolerance.
    * @tparam CRS The CRS of both features, they must be in the same CRS or
    *             this method cannot be called
    * @return true if the features are equal within the specified tolerance,
    *         false otherwise
    */
  def equalsWithinTolerance[CRS: CRST](
      geom1: Geometry[CRS],
      geom2: Geometry[CRS],
      tolerance: Double
  ): Boolean =
    (geom1, geom2) match
      case (g1: Polygonal[CRS], g2: Polygonal[CRS]) =>
        comparePolygonsWithinTolerance(g1, g2, tolerance)

      case (p1: Point2D[CRS], p2: Point2D[CRS]) =>
        p1.distanceSquaredTo(p2) <= tolerance * tolerance

      case (ls1: LineString2D[CRS], ls2: LineString2D[CRS]) if ls1.getPoints.size == ls2.getPoints.size =>
        compareLinePointsWithinTolerance(ls1, ls2, tolerance)

      case (cg1: GeometryCollection[CRS, _, _], cg2: GeometryCollection[CRS, _, _])
          if cg1.componentGeometries.size == cg2.componentGeometries.size =>
        (cg1.componentGeometries zip cg2.componentGeometries).forall {
          case (g1, g2) =>
            equalsWithinTolerance[CRS](g1.asInstanceOf[Geometry[CRS]], g2.asInstanceOf[Geometry[CRS]], tolerance)
        }

      case _ => false


  private def compareLinePointsWithinTolerance[CRS: CRST](
      ls1: LineString2D[CRS],
      ls2: LineString2D[CRS],
      tolerance: Double
  ) =
    (ls1.getPoints zip ls2.getPoints).forall {
      case (p1, p2) => equalsWithinTolerance(p1, p2, tolerance)
    }

  private def comparePolygonsWithinTolerance[CRS: CRST](
      g1: Polygonal[CRS],
      g2: Polygonal[CRS],
      tolerance: Double
  ) =
    (g1.area === (g2.area +- tolerance)) &&
    (g1.intersection(g2).map(_.area).getOrElse(0.0) === g1.area +- tolerance)
