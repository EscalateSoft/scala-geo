package com.escalatesoft.geo

import com.escalatesoft.geo.crs.CRST
import com.escalatesoft.geo.crs.CRST.CRSDefinitions.EPSG_4326

import scala.language.higherKinds
import scala.reflect.ClassTag

/**
  * A collection of type-safe features, in a consistent CRS, of a consistent
  * Geometry sub-type and with consistent Attributes. Since GEOM and ATTR
  * are covariant, mixing of geometries and/or attributes is possible with use
  * of a more general type for that parameter.
  *
  * @param features The sequence of features, in the same CRS, all with a subtype
  *                 of the given geometry and attribute types
  * @param featureTypeId An optional ID for the whole collection - used particularly
  *                      for shape-file compliance (where the ID is used to
  *                      signify a homogenous schema - correct use of this field
  *                      is left to the user, so please understand it before using it)
  * @tparam CRS The CRS type of the collection
  * @tparam GEOM The geometry type of the collection, must be a Geometry or subtype thereof
  * @tparam ATTR The type of the attributes in the features, all features must have
  *              a subtype (including the same type) as this type parameter
  */
class FeatureCollectionInCRS[CRS: CRST, +GEOM[_] <: Geometry[_], +ATTR] protected[geo] (
    val features: Seq[FeatureInCRS[CRS, GEOM, ATTR]],
    val featureTypeId: Option[String] = None
):

  /**
    * Return a new feature collection identical to this except for having a different
    * optional ID parameter set
    * @param newId the new ID for the resulting collection
    * @return copy of this collection with a new ID (note, contents will be identical)
    */
  def withFeatureTypeId(newId: Option[String]): FeatureCollectionInCRS[CRS, GEOM, ATTR] =
    FeatureCollectionInCRS(features, newId)

  /**
    * Create a new feature collection in the given CRS by transforming all features
    * in this collection to the given CRS. Attributes will remain identical.
    * @tparam NEW_CRS The new CRS for the resulting feature collection
    * @return A new feature collection with all features transformed to the given CRS
    */
  def transformCRS[NEW_CRS: CRST]: FeatureCollectionInCRS[NEW_CRS, GEOM, ATTR] =
    FeatureCollectionInCRS(features.map(_.transformCRS[NEW_CRS]), featureTypeId)

  /**
    * Provide a new feature collection with only those features from this one
    * that match the given predicate
    * @param predicate A predicate over the Feature to select features by
    * @return New feature collection in same CRS, with same GEOM and ATTR types
    *         but with only the features that match the predicate.
    */
  def filter(
      predicate: FeatureInCRS[CRS, GEOM, ATTR] => Boolean
  ): FeatureCollectionInCRS[CRS, GEOM, ATTR] =
    FeatureCollectionInCRS(features.filter(predicate), featureTypeId)

  /**
    * Normalize the feature collection to EPSG_4326 (WGS84). If the
    * CRS is already EPSG_4326, this is a simple cast, otherwise
    * the features will be transformed to EPSG_4326
    * @return A Normalized FeatureCollection[GEOM, ATTR] where all
    *         features are in EPSG_4326 CRS
    */
  def normalize: FeatureCollection[GEOM, ATTR] =
    if summon[CRST[CRS]] == EPSG_4326 then
      this.asInstanceOf[FeatureCollection[GEOM, ATTR]]
    else
      val normalizedFeatures = mapOut(_.normalize)
      FeatureCollection(normalizedFeatures)

  /**
    * Map the feature contents of this collection out to a regular Seq
    * by applying the function from feature => T
    * @param f function from Feature[CRS, GEOM, ATTR] => T
    * @tparam T The resulting sequence type
    * @return A Seq[T] resulting from applying f to each feature in the collection
    */
  def mapOut[T](f: FeatureInCRS[CRS, GEOM, ATTR] => T): Seq[T] = features.map(f)

  /**
    * Map the contents of these features one by one to new features that can alter
    * the CRS, Geometry and/or Attribute contents and types, but must retain the
    * overall FeatureInCRS type. If you want to convert them generally to some
    * other type, use mapOut instead
    */
  def map[NEW_CRS: CRST, NEW_GEOM[_] <: Geometry[_], NEW_ATTR](
      f: FeatureInCRS[CRS, GEOM, ATTR] => FeatureInCRS[NEW_CRS, NEW_GEOM, NEW_ATTR]
  ): FeatureCollectionInCRS[NEW_CRS, NEW_GEOM, NEW_ATTR] =

    val newFeatures = mapOut(f)
    FeatureCollectionInCRS[NEW_CRS, NEW_GEOM, NEW_ATTR](newFeatures)

  /**
    * @return the number of features in this collection
    */
  def length: Int = features.length

  /**
    * Create a new collection with just the first n features from this one
    * @param n number of features to take from this collection
    * @return new feature collection, same CRS, GEOM and ATTR types but with
    *         only the first n features
    */
  def take(n: Int): FeatureCollectionInCRS[CRS, GEOM, ATTR] =
    FeatureCollectionInCRS(features.take(n), featureTypeId)

  /**
    * For each feature in this collection, map the function to alter the attributes
    * to a new value/type over it. The geometry will be identical but this can be
    * used to parse or adapt attributes en-masse in a collection.
    * @param fn from ATTR to NEW_ATTR - a function to adapt the attributes for each feature
    * @tparam NEW_ATTR The resulting attribute type for each feature
    * @return A new feature collection with the NEW_ATTR attribute type
    */
  def mapAttributes[NEW_ATTR](fn: ATTR => NEW_ATTR): FeatureCollectionInCRS[CRS, GEOM, NEW_ATTR] =
    FeatureCollectionInCRS(features.map(_.mapAttribute(fn)))

  /**
    * Filter out just the geometry types matching that requested, and narrow
    * the resulting feature collection down to that potentially more-specific
    * geometry type.
    *
    * If there are no geometries of the specified type in the feature collection,
    * the result will be an empty feature collection.
    *
    * @param ct the classtag of the collection type to collect (summon
    *           provided by Scala)
    * @tparam CGEOM The desired geometry type to narrow this collection to
    * @return a new feature collection with geometry type narrowed to the geometry
    *         requested, and only geometries of that type in the collection
    */
  def collectGeometries[CGEOM[_] <: Geometry[_]](
      using ct: ClassTag[CGEOM[CRS]]
  ): FeatureCollectionInCRS[CRS, CGEOM, ATTR] =
    val geomClass = ct.runtimeClass
    val collected: Seq[FeatureInCRS[CRS, CGEOM, ATTR]] = features.collect {
      case f if geomClass.isAssignableFrom(f.geometry.getClass) =>
        f.asInstanceOf[FeatureInCRS[CRS, CGEOM, ATTR]]
    }
    FeatureCollectionInCRS[CRS, CGEOM, ATTR](collected, featureTypeId)

object FeatureCollectionInCRS:

  /**
    * Factory method to create a new feature collection from a sequence of features
    * and an optional ID
    * @param features Seq of features, in the same CRS and with consistent geometry
    *                 and attribute types (though these can be supertypes to the
    *                 individual feature geometry and attribute types in each feature)
    * @param featureTypeId Optional feature ID, used by shapefiles for schema grouping
    * @tparam CRS The CRS type of this feature collection
    * @tparam GEOM The geometry type of this feature collection
    * @tparam ATTR The attribute type of this feature collection
    * @return a new feature collection
    */
  def apply[CRS: CRST, GEOM[_] <: Geometry[_], ATTR](
      features: Seq[FeatureInCRS[CRS, GEOM, ATTR]],
      featureTypeId: Option[String] = None
  ): FeatureCollectionInCRS[CRS, GEOM, ATTR] =
    new FeatureCollectionInCRS[CRS, GEOM, ATTR](features, featureTypeId)
