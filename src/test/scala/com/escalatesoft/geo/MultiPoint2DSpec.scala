package com.escalatesoft.geo

import com.escalatesoft.geo.crs.CRST.CRSDefinitions.{EPSG_32615, EPSG_4326}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class MultiPoint2DSpec extends AnyFunSpec with Matchers:
  describe("MultiPoint2DTest") {

    it("should return component points when requested") {
      val point1 = Point2D[EPSG_32615](10, 10)
      val point2 = Point2D[EPSG_32615](10, 20)
      val point3 = Point2D[EPSG_32615](20, 10)

      val multipoint = MultiPoint2D.fromPoints(Seq(point1, point2, point3))

      val components: Seq[Point2D[EPSG_32615]] = multipoint.points
      val components2: Seq[Point2D[EPSG_32615]] = multipoint.componentGeometries

      components should be(Seq(point1, point2, point3))
      components2 should be(Seq(point1, point2, point3))
    }

    it("should transformCRS for all points") {
      val point1 = Point2D[EPSG_32615](10, 10)
      val point2 = Point2D[EPSG_32615](10, 20)
      val point3 = Point2D[EPSG_32615](20, 10)

      val multipoint = MultiPoint2D.fromPoints(Seq(point1, point2, point3))
      val transformed = multipoint.transformCRS[EPSG_4326]

      val components: Seq[Point2D[EPSG_4326]] = transformed.points
      val components2: Seq[Point2D[EPSG_4326]] = transformed.componentGeometries

      components should be(
        Seq(point1.transformCRS[EPSG_4326],
            point2.transformCRS[EPSG_4326],
            point3.transformCRS[EPSG_4326]))
      components2 should be(
        Seq(point1.transformCRS[EPSG_4326],
            point2.transformCRS[EPSG_4326],
            point3.transformCRS[EPSG_4326]))

    }

    it("should produce correct WKT and eWKT") {
      val point1 = Point2D[EPSG_32615](10, 10)
      val point2 = Point2D[EPSG_32615](10, 20)
      val point3 = Point2D[EPSG_32615](20, 10)

      val multipoint = MultiPoint2D.fromPoints(Seq(point1, point2, point3))
      multipoint.toWKT should be("MULTIPOINT(10.0 10.0, 10.0 20.0, 20.0 10.0)")
      multipoint.toEWKT should be("SRID=32615;MULTIPOINT(10.0 10.0, 10.0 20.0, 20.0 10.0)")

      val transformed = multipoint.transformCRS[EPSG_4326]
      transformed.toWKT should be(
        "MULTIPOINT(-97.48865429474021 9.019376924014616E-5, -97.48865429475683 1.8038753848028644E-4, -97.48856470475677 9.019378038636453E-5)")
      transformed.toEWKT should be(
        "SRID=4326;MULTIPOINT(-97.48865429474021 9.019376924014616E-5, -97.48865429475683 1.8038753848028644E-4, -97.48856470475677 9.019378038636453E-5)")
    }

  }
