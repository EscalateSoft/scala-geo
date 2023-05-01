package com.escalatesoft.geo

import com.escalatesoft.geo.crs.CRST
import com.escalatesoft.geo.crs.CRST.CRSDefinitions.EPSG_4326
import language.higherKinds

type Feature[+GEOM[_] <: Geometry[_], +ATTR] =
  FeatureInCRS[EPSG_4326, GEOM, ATTR]
type FeatureCollection[+GEOM[_] <: Geometry[_], +ATTR] =
  FeatureCollectionInCRS[EPSG_4326, GEOM, ATTR]

type AreaFeature[ATTR] = Feature[Polygonal, ATTR]
type PolygonFeature[ATTR] = Feature[Polygon2D, ATTR]
type MultiPolygonFeature[ATTR] = Feature[MultiPolygon2D, ATTR]
type LineFeature[ATTR] = Feature[LineString2D, ATTR]
type MultiLineFeature[ATTR] = Feature[MultiLineString2D, ATTR]
type PointFeature[ATTR] = Feature[Point2D, ATTR]
type MultiPointFeature[ATTR] = Feature[MultiPoint2D, ATTR]

type AreaFeatureCRS[CRS, ATTR] = FeatureInCRS[CRS, Polygonal, ATTR]
type PolygonFeatureCRS[CRS, ATTR] = FeatureInCRS[CRS, Polygon2D, ATTR]
type MultiPolygonFeatureCRS[CRS, ATTR] = FeatureInCRS[CRS, MultiPolygon2D, ATTR]
type LineFeatureCRS[CRS, ATTR] = FeatureInCRS[CRS, LineString2D, ATTR]
type MultiLineFeatureCRS[CRS, ATTR] = FeatureInCRS[CRS, MultiLineString2D, ATTR]
type PointFeatureCRS[CRS, ATTR] = FeatureInCRS[CRS, Point2D, ATTR]
type MultiPointFeatureCRS[CRS, ATTR] = FeatureInCRS[CRS, MultiPoint2D, ATTR]

/**
  * Factory methods for constructing a Normalized feature simply.
  * A normalized feature is just one that has the CRS set to EPSG_4326
  * via typedef, since that is by far the most common interchange
  * CRS for vector data.
  */
object Feature:

  /**
    * Construct a Normalized Feature (a FeatureInCRS with CRS of EPSG_4326).
    * The GEOM and ATTR type parameters are drawn from the geometry and
    * attribute fields provided. Note that if the geometry is in a
    * different CRS than EPSG_4326, it will be transformed to EPSG_4326
    * before being put into the feature.
    * @param geom a Geometry for the feature
    * @param attr an attribute for the feature (may be rich, compound, etc)
    * @param setId an optional string ID to attach to the feature
    * @tparam CRS the CRS of the incoming geometry, will be transformed to
    *             EPSG_4326 as necessary
    * @tparam GEOM the Geometry subtype of the provided geometry and
    *              hence the resulting geometry type of the feature
    * @tparam ATTR the attribute field type for the feature
    * @return a Normalized Feature[GEOM, ATTR]
    */
  def apply[CRS: CRST, GEOM[_] <: Geometry[_], ATTR](
      geom: GEOM[CRS],
      attr: ATTR,
      setId: Option[String] = None
  ): Feature[GEOM, ATTR] =
    new FeatureInCRS[EPSG_4326, GEOM, ATTR](geom.transformCRS[EPSG_4326].asInstanceOf[GEOM[EPSG_4326]], attr, setId)
