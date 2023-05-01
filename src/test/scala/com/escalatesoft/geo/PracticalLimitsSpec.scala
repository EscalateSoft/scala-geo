/*package com.escalatesoft.geo

import org.scalatest._
import com.cibo.continuum.storage.json.ContinuumFeatureJsonCodec
import io.circe.parser._
import org.apache.commons.io.FileUtils
import java.io.File
import io.circe.Json
import com.escalatesoft.geo.crs.CRST.CRSDefinitions.EPSG_4326

class PracticalLimitsSpec extends FunSpec with Matchers {
  describe("Filtering for small polygons") {
    it("should clean up anything too small from either polygons or holes") {
      val testFile = new File(
        getClass.getClassLoader.getResource("demodata/geometry/reiher_stabilitymap.geojson").toURI
      )
      val testFeatureJson = parse(FileUtils.readFileToString(testFile, "UTF8"))
      val testFeatures = ContinuumFeatureJsonCodec.featureCollectionFromGeoJSON(testFeatureJson).get

      import com.escalatesoft.geo.continuum.adapters.ContinuumAdapters._
      import com.cibo.continuum.spatial.domain.DefaultAttributes._

      val geo2PolyFeatures =
        continuumFeatureCollectionToFeatureCollection(testFeatures).get.collectGeometries[Polygonal]

      val beforePolys = flattenAllPolys(geo2PolyFeatures)

      val cleaned = PracticalLimits.cleanContinuumFeatureCollection(testFeatures).get

      val beforeArea = geo2PolyFeatures.mapOut(_.geometry.areaInCRSUnits).sum

      val geo2cleaned =
        continuumFeatureCollectionToFeatureCollection(cleaned).get.collectGeometries[Polygonal]

      val afterArea = geo2cleaned.mapOut(_.geometry.areaInCRSUnits).sum

      val afterPolys = flattenAllPolys(geo2cleaned)

      beforePolys.size should be(110)
      afterPolys.size should be(80)

      beforePolys.count(_.areaInCRSUnits < 1e-12) should be(30)
      afterPolys.count(_.areaInCRSUnits < 1e-12) should be(0)

      beforeArea should be(afterArea +- 2e-6)

      cleaned.features.zip(testFeatures.features) foreach {
        case (cleaned, original) =>
          original.attributes.toMapAny() should be(cleaned.attributes.toMapAny())
          original.id should be(cleaned.id)
          original.userData should be(cleaned.userData)
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

  def flattenAllPolys(fc: FeatureCollection[Polygonal, Json]): Seq[Polygon2D[EPSG_4326]] = {
    fc.features.flatMap { feature =>
      feature.geometry match {
        case p: Polygon2D[EPSG_4326]       => Seq(p)
        case mp: MultiPolygon2D[EPSG_4326] => mp.asPolygonSeq
      }
    }
  }
}
*/