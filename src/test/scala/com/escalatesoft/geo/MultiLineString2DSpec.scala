package com.escalatesoft.geo

import com.escalatesoft.geo.crs.CRST.CRSDefinitions.{EPSG_32615, EPSG_4326}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class MultiLineString2DSpec extends AnyFunSpec with Matchers:
  describe("MultiLineString2DTest") {

    it("should return component lines when requested") {
      val point1 = Point2D[EPSG_32615](10, 10)
      val point2 = Point2D[EPSG_32615](10, 20)
      val point3 = Point2D[EPSG_32615](20, 10)
      val point4 = Point2D[EPSG_32615](20, 20)

      val ls1 = LineString2D.fromPoints(Seq(point1, point2))
      val ls2 = LineString2D.fromPoints(Seq(point3, point4))

      val multiLS = MultiLineString2D(Seq(ls1, ls2))

      val components: Seq[LineString2D[EPSG_32615]] = multiLS.componentGeometries

      components should be(Seq(ls1, ls2))
    }

    it("should transformCRS for all points") {
      val point1 = Point2D[EPSG_32615](10, 10)
      val point2 = Point2D[EPSG_32615](10, 20)
      val point3 = Point2D[EPSG_32615](20, 10)
      val point4 = Point2D[EPSG_32615](20, 20)

      val ls1 = LineString2D.fromPoints(Seq(point1, point2))
      val ls2 = LineString2D.fromPoints(Seq(point3, point4))

      val multiLS = MultiLineString2D(Seq(ls1, ls2))
      val transformed = multiLS.transformCRS[EPSG_4326]

      val components: Seq[LineString2D[EPSG_4326]] = transformed.componentGeometries

      components should be(Seq(ls1.transformCRS[EPSG_4326], ls2.transformCRS[EPSG_4326]))

    }

    it("should produce correct WKT and eWKT") {
      val point1 = Point2D[EPSG_32615](10, 10)
      val point2 = Point2D[EPSG_32615](10, 20)
      val point3 = Point2D[EPSG_32615](20, 10)
      val point4 = Point2D[EPSG_32615](20, 20)

      val ls1 = LineString2D.fromPoints(Seq(point1, point2))
      val ls2 = LineString2D.fromPoints(Seq(point3, point4))

      val multiLS = MultiLineString2D(Seq(ls1, ls2))
      multiLS.toWKT should be("MULTILINESTRING ((10 10, 10 20), (20 10, 20 20))")
      multiLS.toEWKT should be("SRID=32615;MULTILINESTRING ((10 10, 10 20), (20 10, 20 20))")

      val transformed = multiLS.transformCRS[EPSG_4326]
      transformed.toWKT should be(
        "MULTILINESTRING ((-97.48865429474021 0.0000901937692401, -97.48865429475683 0.0001803875384803), " +
          "(-97.48856470475677 0.0000901937803864, -97.48856470477338 0.0001803875607727))")
      transformed.toEWKT should be(
        "SRID=4326;MULTILINESTRING ((-97.48865429474021 0.0000901937692401, -97.48865429475683 0.0001803875384803), " +
          "(-97.48856470475677 0.0000901937803864, -97.48856470477338 0.0001803875607727))")
    }

  }
