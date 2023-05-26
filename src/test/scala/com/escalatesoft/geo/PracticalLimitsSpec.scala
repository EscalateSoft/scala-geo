package com.escalatesoft.geo

import com.escalatesoft.geo.crs.CRST.CRSDefinitions.EPSG_4326
import com.escalatesoft.geo.json.FeatureCollectionJson
import com.escalatesoft.geo.json.FeatureJsonCodec
import com.escalatesoft.geo.json.GeoJsonRawFormats.given
import org.apache.commons.io.FileUtils
import org.scalatest.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.JsValue
import play.api.libs.json.Json

import java.io.File
import java.nio.charset.StandardCharsets

class PracticalLimitsSpec extends AnyFunSpec with Matchers:
  describe("Filtering for small polygons") {
    it("should clean up anything too small from either polygons or holes") {
      val testFile = new File(
        getClass.getClassLoader.getResource("demodata/geometry/reiher_stabilitymap.geojson").toURI
      )

      val featureCollectionJsonStr = FileUtils.readFileToString(testFile, StandardCharsets.UTF_8)
      val featureCollectionJson = Json.parse(featureCollectionJsonStr)

      val testFeatures: FeatureCollectionInCRS[EPSG_4326, Geometry, JsValue] =
        FeatureJsonCodec.featureCollectionFromGeoJSON[JsValue].reads(featureCollectionJson).get

      val geo2PolyFeatures = testFeatures.collectGeometries[Polygonal]

      val beforePolys = flattenAllPolys(geo2PolyFeatures)

      val cleaned = PracticalLimits.cleanFeatureCollection(geo2PolyFeatures)

      val beforeArea = geo2PolyFeatures.mapOut(_.geometry.areaInCRSUnits).sum

      val afterArea = cleaned.mapOut(_.geometry.areaInCRSUnits).sum

      val afterPolys = flattenAllPolys(cleaned)

      beforePolys.size should be(110)
      afterPolys.size should be(80)

      beforePolys.count(_.areaInCRSUnits < 1e-12) should be(30)
      afterPolys.count(_.areaInCRSUnits < 1e-12) should be(0)

      beforeArea should be(afterArea +- 2e-6)

      cleaned.features.zip(testFeatures.features) foreach {
        case (cleaned, original) =>
          original.attr should be(cleaned.attr)
          original.id should be(cleaned.id)
      }
    }

    it("should not filter out holes that are larger than the threshold") {
      val testFile = new File(
        getClass.getClassLoader.getResource("demodata/geometry/HoledPoly.wkt").toURI
      )
      val testFeatureWKT = FileUtils.readFileToString(testFile, "UTF8")
      val testPoly = Geometry.fromWKT[EPSG_4326](testFeatureWKT) match {
        case p: Polygon2D[EPSG_4326] => p
      }

      val cleaned = PracticalLimits.cleanPolygon(testPoly)

      cleaned.equalsWithinTolerance(testPoly) should be(true)
    }
  }

  def flattenAllPolys(fc: FeatureCollection[Polygonal, JsValue]): Seq[Polygon2D[EPSG_4326]] = {
    fc.features.flatMap { feature =>
      feature.geometry match
        case p: Polygon2D[EPSG_4326]       => Seq(p)
        case mp: MultiPolygon2D[EPSG_4326] => mp.asPolygonSeq
    }
  }