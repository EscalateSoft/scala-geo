package com.escalatesoft.geo
import com.escalatesoft.geo.crs.CRST
import scala.util.Try
//import com.cibo.continuum.spatial.domain.ContinuumFeatureCollection

/**
  * A set of utility routines to provide sensible defaults and limits
  * for geometries, in particular minimum polygon sizes for processing,
  * and cleanup to those sizes, point equality tolerance, etc.
  */
object PracticalLimits:

  private val X_AXIS = 0

  /**
    * Provide a default minimum processing area threshold for polygons, at
    * about 1/2 a square meter minimum polygon size for anything we care about.
    * This is based on the CRS and is a very different tolerance for meter based
    * CRSs to, say, degree based.
    */
  def defaultAreaThreshold[CRS](using ccrs: CRST[CRS]): Double =
    Try {
      ccrs.crs.getCoordinateSystem.getAxis(X_AXIS).getUnit.toString match
        case "m" => 5e-1
        case _   => 1e-12
    }.getOrElse(1e-12)

  /**
    * Determine if a polygon is considered "processable" in a sensible way by checking
    * if it has an area greater than the default threshold of about half a meter square.
    * If smaller than this area, the function will return false and the polygon can
    * usually be skipped from being processed
    */
  def processablePolygon[CRS: CRST](p: Polygonal[CRS]): Boolean =
    p.areaInCRSUnits >= defaultAreaThreshold[CRS]

  /**
    * Clean up a Polygon or MultiPolygon by filtering out any sub-polygons
    * or holes smaller than the default area threshold size.
    */
  def cleanPolygon[CRS: CRST](p: Polygonal[CRS]): Polygonal[CRS] =
    val areaThreshold = defaultAreaThreshold[CRS]
    p match
      case poly: Polygon2D[CRS] => poly.filterInteriorRingsByArea(areaThreshold)
      case multiPoly: MultiPolygon2D[CRS] =>
        val separatePolys = multiPoly.asPolygonSeq.filter(_.area >= areaThreshold)
        val cleanedPolys = separatePolys.map(_.filterInteriorRingsByArea(areaThreshold))
        MultiPolygon2D.fromPolygons[CRS](cleanedPolys)

  /**
    * Clean up a polygonal feature by filtering out any sub-polygons or holes
    * smaller than the default area threshold size. Retain the same CRS and Attributes
    */
  def cleanPolygonalFeature[CRS: CRST, ATTR](
      feature: FeatureInCRS[CRS, Polygonal, ATTR]
  ): FeatureInCRS[CRS, Polygonal, ATTR] =
    feature.mapGeometry(poly => cleanPolygon(poly), retainId = true)

  /**
    * Given a feature collection, remove any polygons under the minimum default threshold
    * in area, and clean up any remaining polygons to remove any sub-polygons or holes below
    * the minimum area size as well
    */
  def cleanFeatureCollection[CRS: CRST, ATTR](
      fc: FeatureCollectionInCRS[CRS, Polygonal, ATTR]
  ): FeatureCollectionInCRS[CRS, Polygonal, ATTR] =
    val areaThreshold = defaultAreaThreshold[CRS]
    fc.filter(_.geometry.area >= areaThreshold)
      .map(polyFeature => cleanPolygonalFeature(polyFeature))

  /**
    * Clean a continuum feature collection by converting it to
    * geo 2 features, running the cleanFeatureCollection and converting
    * back again. This is a stop gap until we can roll geo 2 out more
    * widely.
    *
    * Since ContinuumFeatureCollections have an unmanaged Map[String,Any]
    * to represent attributes, this can cause problems with conversion to/from
    * geo 2 (which is typesafe) so we allow failure by returning a Try
    * instead of just the new collection.
    */
  // def cleanContinuumFeatureCollection[CRS: CRST](
  //     cfc: ContinuumFeatureCollection[CRS]
  // ): Try[ContinuumFeatureCollection[CRS]] = {
  //   import com.escalatesoft.geo.continuum.adapters.ContinuumAdapters._
  //   for {
  //     fc <- continuumFeatureCollectionToFeatureCollection(cfc)
  //     cleaned = cleanFeatureCollection(fc.collectGeometries[Polygonal])
  //     newCfc <- featureCollectionToContinuumFeatureCollection(cleaned)
  //   } yield newCfc
  // }
