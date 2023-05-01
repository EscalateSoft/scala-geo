package com.escalatesoft.geo

import com.escalatesoft.geo.crs.CRST.CRSDefinitions.EPSG_4326

import language.higherKinds

/**
  * Factory methods for constructing a Normalized FeatureCollection from a sequence of
  * Normalized Features.
  */
object FeatureCollection:

  /**
    * Create a new Normalized FeatureCollection from a sequence of Normalized Features.
    * The GEOM and ATTR types must be consistent across all features provided though
    * due to the covariant nature of each, they can be a wider type than the individual
    * features. E.g. you could have a Normalized FeatureCollection[Geometry, Fruit] with features
    * Feature[Polygon2D, Banana] and Feature[Point2D, Apple] in it.
    *
    * @param features sequence of normalized features to include in the
    *                 normalized feature collection
    * @tparam GEOM the Geometry supertype of all of the features in this collection
    * @tparam ATTR the attribute supertype of all of the features in this collection
    * @return a Normalized FeatureCollection with the Normalized Features provided in it
    */
  def apply[GEOM[_] <: Geometry[_], ATTR](
      features: Seq[Feature[GEOM, ATTR]],
      setId: Option[String] = None): FeatureCollection[GEOM, ATTR] =
    new FeatureCollectionInCRS[EPSG_4326, GEOM, ATTR](features, setId)
