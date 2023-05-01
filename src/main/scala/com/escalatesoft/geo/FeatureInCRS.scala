package com.escalatesoft.geo

import com.escalatesoft.geo.crs.CRST
import com.escalatesoft.geo.crs.CRST.CRSDefinitions.EPSG_4326

import language.higherKinds
import scala.reflect._
import scala.util.{Failure, Success, Try}

/**
  * A type-safe feature representation for a feature with a CRS, geometry type and
  * attribute type. GEOM and ATTR are co-variant.
  *
  * @param geometry The geometry in the given CRS for the feature
  * @param attr The attribute for the feature, there is only one attribute per
  *             feature which can be any type, e.g. compound types, Lists, Maps,
  *             case classes, AttributeMap and much more.
  * @param setId an optional ID for the feature. Will be generated from a geometry
  *              hash if not specified
  * @tparam CRS the CRS for this feature
  * @tparam GEOM The geometry type, must be Geometry or a subtype thereof, covariant
  * @tparam ATTR The attribute type, covariant
  */
class FeatureInCRS[CRS: CRST, +GEOM[_] <: Geometry[_], +ATTR] protected[geo] (
    val geometry: GEOM[CRS],
    val attr: ATTR,
    val setId: Option[String] = None
):

  /**
    * The ID of this feature, either from the one provided or computed from
    * the geometry hash
    */
  lazy val id: String = setId.getOrElse(Geometry.idFromWKT(geometry.toWKT))

  /**
    * Copy the feature but provide a new ID. The geometry and attribute of the
    * new feature will be identical to this one.
    * @param newId The optional new ID to use
    * @return new feature with given ID
    */
  def withId(newId: Option[String]): FeatureInCRS[CRS, GEOM, ATTR] =
    new FeatureInCRS[CRS, GEOM, ATTR](geometry, attr, setId = newId)

  /**
    * Obtain the geometry for this feature in the CRS specified, it can
    * be the same as the feature CRS or a different CRS (and will be transformed
    * as needed)
    * @tparam NEW_CRS the CRS to get the geometry in
    * @return GEOM type in the CRS requested
    */
  def geometryInCRS[NEW_CRS: CRST]: GEOM[NEW_CRS] =
    geometry.transformCRS[NEW_CRS].asInstanceOf[GEOM[NEW_CRS]]

  /**
    * Transform this whole feature to a different CRS. Since this
    * will alter the geometry, repeated back-and-forth transforms will
    * introduce losses and rounding errors, consider keeping the same
    * CRS for the feature and using geometryInCRS to access the geometry
    * in a different CRS instead, since that will produce more stable results.
    * @tparam NEW_CRS The CRS for the new feature
    * @return new feature, transformed into the new CRS, attributes identical
    */
  def transformCRS[NEW_CRS: CRST]: FeatureInCRS[NEW_CRS, GEOM, ATTR] =
    FeatureInCRS[NEW_CRS, GEOM, ATTR](geometryInCRS[NEW_CRS], attr)

  def canEqual(other: Any): Boolean = other match
    case _: FeatureInCRS[_, _, _] => true
    case _                        => false

  /**
    * Map a function over the attribute of this feature to produce a new feature
    * with modified attribute (and potentially different Attribute type)
    * @param fn The function to map over the feature attribute
    * @tparam NEW_ATTR The resulting type of the function
    * @return a new feature with modified attribute but identical geometry
    */
  def mapAttribute[NEW_ATTR](fn: ATTR => NEW_ATTR): FeatureInCRS[CRS, GEOM, NEW_ATTR] =
    new FeatureInCRS(geometry, fn(attr), setId)

  /**
    * Produce a new feature resulting from mapping the given function over the
    * geometry. Note that this does not allow alteration of the CRS of the geometry,
    * if you want to transform the CRS, use transformCRS instead. This method allows
    * the geometry to be manipulated to another geometry in the same CRS as this one,
    * but possibly into a different geometry type instead.
    *
    * For example, you could use mapGeometry to get the centroid of a polygon, resulting
    * in a new feature with a centroid point instead of the polygon. You could also erode
    * or buffer a polygon with this function, resulting in a new feature but still
    * polygonal in nature.
    *
    * @param fn function to map over the geometry of this feature to produce a new
    *           geometry and feature
    * @param retainId whether to retain the current feature ID. False by default since
    *                 altering the geometry probably heralds treatment of this feature
    *                 differently than before. Be very careful you know what you are
    *                 doing if you retain the feature ID.
    * @tparam NEW_GEOM The new geometry type resulting from the function provided
    * @return a new feature with the altered geometry and geometry type, CRS and ATTR
    *         are unchanged and attr is identical
    */
  def mapGeometry[NEW_GEOM[_] <: Geometry[_]](
      fn: GEOM[CRS] => NEW_GEOM[CRS],
      retainId: Boolean = false
  ): FeatureInCRS[CRS, NEW_GEOM, ATTR] =
    val newId = if retainId then setId else None
    new FeatureInCRS[CRS, NEW_GEOM, ATTR](fn(geometry), attr, newId)

  /**
    * Attempt to narrow the geometry of a feature to that requested and return
    * a Try with success and the new geometry type if it can be, or a Try failure
    * with an illegal state exception if not.
    * @tparam NEW_GEOM the geometry type to narrow to
    * @return a feature with the new geometry type in a successful Try if successful,
    *         or an illegal state exception in a failed Try if not
    */
  def narrowGeometry[NEW_GEOM[_] <: Geometry[_]](
      using ct: ClassTag[NEW_GEOM[CRS]]
  ): Try[FeatureInCRS[CRS, NEW_GEOM, ATTR]] = {
    val geomClass = ct.runtimeClass

    geometry match {
      case g if geomClass.isAssignableFrom(g.getClass) =>
        Success(this.asInstanceOf[FeatureInCRS[CRS, NEW_GEOM, ATTR]])
      case g =>
        Failure(
          new IllegalStateException(
            s"Feature ${g.getClass.getSimpleName} could not be narrowed to ${classTag[NEW_GEOM[CRS]].runtimeClass.getSimpleName}"
          )
        )
    }
  }

  /**
    * Normalize this feature to EPSG_4326 (WGS84) if it is not already
    * @return Normalized Feature with same Geometry and Attribute types as this
    *         one but with EPSG_4326 transformed geometry (if it wasn't already)
    */
  def normalize: Feature[GEOM, ATTR] =
    Feature(geometryInCRS[EPSG_4326], attr)

  /**
    * Compare this feature for precise equality of geometry and attributes
    * to another object.
    * @param other The other object to test for equality
    * @return true if geometry and attributes are exactly equal, false otherwise
    */
  override def equals(other: Any): Boolean = other match
    case that: FeatureInCRS[_, _, _] =>
      (that canEqual this) &&
        geometry == that.geometry &&
        attr == that.attr
    case _ => false

  /**
    * Test equality of this feature with another allowing for some default
    * tolerance differences in the geometry (usually the result of rounding
    * errors in transformations). The attributes still must equal precisely, but a
    * small amount of variance in the geometries are permissable.
    * @param other the other feature to compare to this one
    * @tparam ATTR2 the attribute type of the other feature (type can vary, but the
    *               attributes must still report back that they are equal)
    * @return true if the attributes are equal and the geometries are within
    *         tolerance, false otherwise
    */
  def equalsWithinTolerance[ATTR2](other: FeatureInCRS[CRS, Geometry, ATTR2]): Boolean =
    attr == other.attr && geometry
      .asInstanceOf[Geometry[CRS]]
      .equalsWithinTolerance(other.geometry)

  private[this] lazy val innerHashcode: Int =
    41 * (
      41 + geometry.toWKT.##
    ) + attr.##

  override def hashCode(): Int = innerHashcode

object FeatureInCRS:

  /**
    * Create a feature in the given CRS, from the geometry and attributes
    * passed in
    * @param geom geometry in the correct CRS
    * @param attr attribute value for the feature
    * @param setId optional ID to use for the feature
    * @tparam CRS type of the CRS for this feature
    * @tparam GEOM Geometry type of the feature, must be Geometry or a subtype thereof
    * @tparam ATTR type of the attribute field for this feature
    * @return a feature in the requested CRS, with the geometry and attribute values
    *         and types provided.
    */
  def apply[CRS: CRST, GEOM[_] <: Geometry[_], ATTR](
      geom: GEOM[CRS],
      attr: ATTR,
      setId: Option[String] = None
  ): FeatureInCRS[CRS, GEOM, ATTR] =
    new FeatureInCRS[CRS, GEOM, ATTR](geom, attr, setId)

