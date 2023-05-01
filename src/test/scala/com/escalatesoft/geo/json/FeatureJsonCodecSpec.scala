package com.escalatesoft.geo.json

import com.escalatesoft.geo._
import com.escalatesoft.geo.crs.CRST.CRSDefinitions.EPSG_4326
import org.apache.commons.io.FileUtils
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.JsError
import play.api.libs.json.JsSuccess
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.json.Reads
import play.api.libs.json.Writes

import java.io.File
import java.nio.charset.StandardCharsets
import scala.util.Try

import language.higherKinds
import GeoJsonRawFormats.given

class FeatureJsonCodecSpec extends AnyFunSpec with Matchers with TypeCheckedTripleEquals:

  def readFileToString(filePath: String): String =
    val file = new File(getClass.getClassLoader.getResource(filePath).toURI)
    FileUtils.readFileToString(file, StandardCharsets.UTF_8)

  describe("Feature GeoJSON encoding and decoding") {

    it("should read a feature collection from geojson to a raw representation") {
      val featureCollectionJsonStr = readFileToString("demodata/geometry/justpolys.geojson")
      val featureCollectionJson = Json.parse(featureCollectionJsonStr)

      val fc = featureCollectionJson.as[FeatureCollectionJson]

      Json.toJson(fc) should ===(featureCollectionJson)
    }

    it("should read all geom types from geojson to a raw representation") {
      val justPolys = new File(getClass.getClassLoader.getResource("demodata/geometry/allgeoms.geojson").toURI)
      val featureCollectionJsonStr = FileUtils.readFileToString(justPolys, StandardCharsets.UTF_8)
      val featureCollectionJson = Json.parse(featureCollectionJsonStr)

      val fc = featureCollectionJson.as[FeatureCollectionJson]

      Json.toJson(fc) should ===(featureCollectionJson)
    }

    it("should read ugly attributes from geojson to a raw representation") {
      val justPolys = new File(getClass.getClassLoader.getResource("demodata/geometry/uglyattributes.geojson").toURI)
      val featureStr = FileUtils.readFileToString(justPolys, StandardCharsets.UTF_8)
      val feature = Json.parse(featureStr)

      val fc = feature.as[FeatureJson]

      Json.toJson(fc) should ===(feature)
    }

    it("should read real-world data from geojson to a raw representation") {
      val justPolys = new File(getClass.getClassLoader.getResource("demodata/geometry/reiher_stabilitymap.geojson").toURI)
      val featureCollectionStr = FileUtils.readFileToString(justPolys, StandardCharsets.UTF_8)
      val featureCollection = Json.parse(featureCollectionStr)

      val fc = featureCollection.as[FeatureCollectionJson]

      Json.toJson(fc) should ===(featureCollection)
    }


    it("should narrow deserialized feature collections to requested geometry types") {
      val featureCollectionJsonStr = readFileToString("demodata/geometry/allgeoms.geojson")
      val featureCollectionJson = Json.parse(featureCollectionJsonStr)
      val collection: Option[FeatureCollectionInCRS[EPSG_4326, Geometry, JsValue]] =
        FeatureJsonCodec.featureCollectionFromGeoJSON[JsValue].reads(featureCollectionJson).asOpt

      val justPolys: FeatureCollectionInCRS[EPSG_4326, Polygonal, JsValue] =
        collection.get.collectGeometries[Polygonal]
      justPolys.features should have size (3)
      collection.get.features.head.setId should be(defined)

      val justSinglePolys: FeatureCollectionInCRS[EPSG_4326, Polygon2D, JsValue] =
        collection.get.collectGeometries[Polygon2D]
      justSinglePolys.features should have size (2)

      val justMultiPolys: FeatureCollectionInCRS[EPSG_4326, MultiPolygon2D, JsValue] =
        collection.get.collectGeometries[MultiPolygon2D]
      justMultiPolys.features should have size (1)

      val justPoints: FeatureCollectionInCRS[EPSG_4326, Point2D, JsValue] =
        collection.get.collectGeometries[Point2D]
      justPoints.features should have size (4)
    }

    it("should encode and decode collections with mixed geometry types") {
      val point1 = Point2D[EPSG_4326](10.0, 20.0)
      val point2 = Point2D[EPSG_4326](20.0, 30.0)
      val point3 = Point2D[EPSG_4326](30.0, 40.0)
      val point4 = Point2D[EPSG_4326](20.0, 50.0)
      val mp = MultiPoint2D.fromPoints[EPSG_4326](Seq(point1, point2, point3, point4))
      val ls1 = LineString2D.fromPoints[EPSG_4326](Seq(point1, point2))
      val ls2 = LineString2D.fromPoints[EPSG_4326](Seq(point3, point4))
      val mls = MultiLineString2D[EPSG_4326](Seq(ls1, ls2))
      val poly1 = Polygon2D.fromPoints[EPSG_4326](Seq(point1, point2, point3, point4))
      val poly2 = Polygon2D[EPSG_4326](
        Seq(
          RawCoordinate2D(100.0, 100.0),
          RawCoordinate2D(110.0, 100.0),
          RawCoordinate2D(110.0, 110.0),
          RawCoordinate2D(100.0, 110.0)
        ))
      val mpoly = MultiPolygon2D.fromPolygons(Seq(poly1, poly2))

      val justPolygonal: FeatureCollection[Polygonal, TestAttributes] =
        FeatureCollection(attributedGeoms(Seq(poly1, poly2, mpoly)))

      val allGeoms: FeatureCollection[Geometry, TestAttributes] =
        FeatureCollection(
          attributedGeoms(
            Seq(
              point1,
              mp,
              point2,
              ls1,
              point3,
              poly1,
              point4,
              mls,
              ls2,
              mpoly,
              poly2
            )))

      import FeatureJsonCodec.given
      val justPolysJson = Json.toJson(justPolygonal: FeatureCollection[Geometry, TestAttributes])
      val allGeomsJson = Json.toJson(allGeoms)

      val comparePolysStr = readFileToString("demodata/geometry/justpolys.geojson")
      val compareAllStr = readFileToString("demodata/geometry/allgeoms.geojson")

      justPolysJson should be(Json.parse(comparePolysStr))
      allGeomsJson should be(Json.parse(compareAllStr))

      val decodePolys = FeatureJsonCodec.featureCollectionFromGeoJSON[JsValue].reads(justPolysJson).asEither
      val decodeAll = FeatureJsonCodec.featureCollectionFromGeoJSON[TestAttributes].reads(allGeomsJson).asEither

      val polysWithAttrs: FeatureCollection[Geometry, TestAttributes] =
        decodePolys.toOption.get.mapAttributes(_.as[TestAttributes])

      val polygonals = polysWithAttrs.collectGeometries[Polygonal]

      polygonals.features should have size (3)

      polygonals.collectGeometries[Polygon2D].features should have size (2)
      polygonals.collectGeometries[MultiPolygon2D].features should have size (1)

      val allWithAttrs = decodeAll.toOption.get

      allWithAttrs.features should have size (11)
      allWithAttrs.collectGeometries[Puntal].features should have size (5)
    }
  }

  def attributeGeom[GEOM[_] <: Geometry[_], ATTR](
    geom: GEOM[EPSG_4326],
    attr: ATTR,
    idx: Int,
  ): Feature[GEOM, ATTR] =
    Feature(geom, attr, Some(s"test_id$idx"))

  case class TestAttributes(i: Int, square: Double, stars: String)
  given Reads[TestAttributes] = Json.reads[TestAttributes]
  given Writes[TestAttributes] = Json.writes[TestAttributes]

  def attributedGeoms[GEOM[_] <: Geometry[_], ATTR](
    geoms: Seq[GEOM[EPSG_4326]]
  ): Seq[Feature[GEOM, TestAttributes]] =
    for (g, i) <- geoms.zipWithIndex yield {
      attributeGeom(g, TestAttributes(i, i * i, "*" * i), i)
    }
    
end FeatureJsonCodecSpec
