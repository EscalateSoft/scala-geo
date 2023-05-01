package com.escalatesoft.geo

import com.escalatesoft.geo.crs.CRST.CRSDefinitions.{EPSG_32615, EPSG_4326}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ConvexHullSpec extends AnyFunSpec with Matchers:
  describe("Creating convex hulls") {
    it("should work for a point") {
      val p1 = Point2D[EPSG_32615](8000.0, 20000.0)

      val ch = p1.convexHull
      ch shouldBe a[Point2D[_]]

      ch.toEWKT should be("SRID=32615;POINT (8000.0 20000.0)")
    }

    it("should work for a multi-point") {
      val mp1 = MultiPoint2D[EPSG_32615](
        Seq(
          RawCoordinate2D(10, 10),
          RawCoordinate2D(10, 20)
        ))

      val mp2 = MultiPoint2D[EPSG_32615](
        Seq(
          RawCoordinate2D(10, 10),
          RawCoordinate2D(10, 20),
          RawCoordinate2D(20, 10)
        ))

      val ch1 = mp1.convexHull
      val ch2 = mp2.convexHull

      ch1 shouldBe a[LineString2D[_]]
      ch2 shouldBe a[Polygon2D[_]]

      ch1.toEWKT should be("SRID=32615;LINESTRING (10 10, 10 20)")
      ch2.toEWKT should be("SRID=32615;POLYGON ((10 10, 10 20, 20 10, 10 10))")
    }

    it("should work for LineStrings") {
      val ls1 = LineString2D[EPSG_32615](
        Seq(
          RawCoordinate2D(10, 10),
          RawCoordinate2D(10, 20)
        ))

      val ls2 = LineString2D[EPSG_32615](
        Seq(
          RawCoordinate2D(10, 10),
          RawCoordinate2D(10, 20),
          RawCoordinate2D(20, 10)
        ))

      val ch1 = ls1.convexHull
      val ch2 = ls2.convexHull

      ch1 shouldBe a[LineString2D[_]]
      ch2 shouldBe a[Polygon2D[_]]

      ch1.toEWKT should be("SRID=32615;LINESTRING (10 10, 10 20)")
      ch2.toEWKT should be("SRID=32615;POLYGON ((10 10, 10 20, 20 10, 10 10))")
    }

    it("should work for MultiLineStrings") {
      val ls1 = LineString2D[EPSG_32615](
        Seq(
          RawCoordinate2D(10, 10),
          RawCoordinate2D(10, 20)
        ))

      val ls2 = LineString2D[EPSG_32615](
        Seq(
          RawCoordinate2D(10, 40),
          RawCoordinate2D(10, 50)
        ))

      val ls3 = LineString2D[EPSG_32615](
        Seq(
          RawCoordinate2D(20, 40),
          RawCoordinate2D(20, 50)
        ))

      val mls1 = MultiLineString2D[EPSG_32615](Seq(ls1, ls2))
      val mls2 = MultiLineString2D[EPSG_32615](Seq(ls1, ls2, ls3))

      val ch1 = mls1.convexHull
      val ch2 = mls2.convexHull

      ch1 shouldBe a[LineString2D[_]]
      ch2 shouldBe a[Polygon2D[_]]

      ch1.toEWKT should be("SRID=32615;LINESTRING (10 10, 10 50)")
      ch2.toEWKT should be("SRID=32615;POLYGON ((10 10, 10 50, 20 50, 20 40, 10 10))")
    }

    it("should work on polygons") {
      val rawPoints = Seq(
        RawCoordinate2D(-92.2493031319753, 42.672304270958215),
        RawCoordinate2D(-92.24929402927881, 42.67243292753743),
        RawCoordinate2D(-92.24714959503662, 42.67242300503766)
      )

      val poly = Polygon2D[EPSG_4326](rawPoints)

      val ch1: Polygon2D[EPSG_4326] = poly.convexHull

      ch1.toEWKT should be(
        "SRID=4326;POLYGON ((-92.2493031319753 42.672304270958215, -92.24929402927881 42.67243292753743, " +
          "-92.24714959503662 42.67242300503766, -92.2493031319753 42.672304270958215))")
    }

    it("should never be called to work on an invalid polygon") {
      val rawPoints = Seq(
        RawCoordinate2D(-92, 42),
        RawCoordinate2D(-91.5, 42),
        RawCoordinate2D(-91.0, 42)
      )

      val poly = Polygon2D[EPSG_4326](rawPoints)
      poly.isValid should be(false)

      val failure = intercept[IllegalStateException] {
        poly.convexHull
      }

      failure.getMessage should be(
        "Polygonal.this.isValid was false Attempt to obtain convex hull over invalid polygon is unsupported")
    }

    it("should work on multi polygons") {
      val poly1 = Polygon2D[EPSG_32615](
        Seq(
          RawCoordinate2D(1, 1),
          RawCoordinate2D(1, 9),
          RawCoordinate2D(9, 9)
        ))

      val poly2 = Polygon2D[EPSG_32615](
        Seq(
          RawCoordinate2D(0, 3),
          RawCoordinate2D(10, 3),
          RawCoordinate2D(10, 5),
          RawCoordinate2D(0, 5)
        ))

      val diffPoly = poly1.difference(poly2).get

      val ch: Polygon2D[EPSG_32615] = diffPoly.convexHull

      ch.toEWKT should be("SRID=32615;POLYGON ((1 1, 1 9, 9 9, 1 1))")
    }

  }
