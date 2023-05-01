package com.escalatesoft.geo

import com.escalatesoft.geo.crs.CRST.CRSDefinitions.{EPSG_32615, EPSG_4326}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class LineString2DSpec extends AnyFunSpec with Matchers:
  describe("LineStringSpec") {

    it("should transformCRS") {
      val rawPoints = Seq(
        RawCoordinate2D(-92.2493031319753, 42.672304270958215),
        RawCoordinate2D(-92.24929402927881, 42.67243292753743),
        RawCoordinate2D(-92.24714959503662, 42.67242300503766)
      )

      val lineString2d = LineString2D[EPSG_4326](rawPoints)

      val transformedLineString2d = lineString2d.transformCRS[EPSG_32615]

      val wkt = transformedLineString2d.toWKT
      val actualWKT =
        "LINESTRING (561512.3702547641 4724698.932194901, 561512.9892547643 4724713.2251949, 561688.7152547662 4724713.6861948995)"
      wkt should be(actualWKT)

      val pointsBack = transformedLineString2d.getPoints

      pointsBack should have size 3

      pointsBack(0).x should be(561512.3702547651 +- 1e-9)
      pointsBack(0).y should be(4724698.932194901 +- 1e-9)
      pointsBack(1).x should be(561512.9892547653 +- 1e-9)
      pointsBack(1).y should be(4724713.2251949 +- 1e-9)
      pointsBack(2).x should be(561688.7152547662 +- 1e-9)
      pointsBack(2).y should be(4724713.6861948995 +- 1e-9)
    }

  }
