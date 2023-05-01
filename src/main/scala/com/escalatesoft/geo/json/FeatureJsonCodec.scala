package com.escalatesoft.geo.json

import play.api.libs.json.Format
import play.api.libs.json.Json
import play.api.libs.json.JsValue
import scala.util.Try
import com.escalatesoft.geo.FeatureCollectionInCRS
import com.escalatesoft.geo.crs.CRST.CRSDefinitions.EPSG_4326
import com.escalatesoft.geo.Geometry
import com.escalatesoft.geo.FeatureInCRS
import play.api.libs.json.JsResult
import com.escalatesoft.geo.json.GeometryJson
import play.api.libs.json.JsObject
import play.api.libs.json.Reads
import play.api.libs.json.Writes

object FeatureJsonCodec:
  def featureFromGeoRaw[ATTR: Reads](featureRaw: FeatureJson): Try[FeatureInCRS[EPSG_4326, Geometry, ATTR]] =
    Try {
      import GeoJsonRawFormats.given
      val attr = featureRaw.properties.get.as[ATTR]
      val geom = featureRaw.geometry.map(_.toGeometry).getOrElse(throw new IllegalStateException(s"No geometry specified in feature $featureRaw"))
      FeatureInCRS[EPSG_4326, Geometry, ATTR](geom, attr)
    }

  def featureToGeoRaw[ATTR: Writes](feature: FeatureInCRS[EPSG_4326, Geometry, ATTR]): FeatureJson =
    import GeoJsonRawFormats.given
    val attrJson = Json.toJson(feature.attr).as[JsObject]
    val geomJson = GeometryJson.fromGeometry(feature.geometry)
    FeatureJson(if feature.id.isEmpty then None else Some(feature.id), Some(geomJson), Some(attrJson))

  given featureFromJSON[ATTR: Reads]: Reads[FeatureInCRS[EPSG_4326, Geometry, ATTR]] = { js =>
    import GeoJsonRawFormats.given
    for
      featureJson <- js.validate[FeatureJson]
      feature <- JsResult.fromTry(featureFromGeoRaw[ATTR](featureJson))
    yield feature
  }

  given featureToJSON[ATTR: Writes]: Writes[FeatureInCRS[EPSG_4326, Geometry, ATTR]] = { ft =>
    import GeoJsonRawFormats.given
    val featureJson = featureToGeoRaw[ATTR](ft)
    Json.toJson(featureJson)
  }

  def featureCollectionFromGeoRaw[ATTR: Reads](
    featureCollection: FeatureCollectionJson
  ): Try[FeatureCollectionInCRS[EPSG_4326, Geometry, ATTR]] =
    Try {
      import GeoJsonRawFormats.given
      val features = featureCollection.features.map(f => featureFromGeoRaw[ATTR](f).get)
      FeatureCollectionInCRS[EPSG_4326, Geometry, ATTR](features)
    }

  def featureCollectionToGeoRaw[ATTR: Writes](
    featureCollection: FeatureCollectionInCRS[EPSG_4326, Geometry, ATTR]
  ): FeatureCollectionJson =
    import GeoJsonRawFormats.given
    val features = featureCollection.features.map(f => featureToGeoRaw[ATTR](f))
    FeatureCollectionJson(features)

  given featureCollectionFromGeoJSON[ATTR: Reads]: Reads[FeatureCollectionInCRS[EPSG_4326, Geometry, ATTR]] = { js =>
    import GeoJsonRawFormats.given
    for
      fcJson <- js.validate[FeatureCollectionJson]
      fc <- JsResult.fromTry(featureCollectionFromGeoRaw[ATTR](fcJson))
    yield fc
  }

  given featureCollectionToGeoJSON[ATTR: Writes]: Writes[FeatureCollectionInCRS[EPSG_4326, Geometry, ATTR]] = { fc =>
    import GeoJsonRawFormats.given
    val features = featureCollectionToGeoRaw(fc)
    Json.toJson(features)
  }