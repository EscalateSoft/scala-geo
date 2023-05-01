package com.escalatesoft.geo

import java.io.File
import java.nio.charset.Charset

import com.escalatesoft.geo.crs.CRST.CRSDefinitions.{EPSG_32615, EPSG_4326}
import org.apache.commons.io.FileUtils
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class EqualsWithinToleranceSpec extends AnyFunSpec with Matchers:
  describe("Geometry equality within tolerance") {
    it("should work for points") {
      val p1 = Point2D[EPSG_4326](-121.00001, 36.99999)
      val p2 = Point2D[EPSG_4326](-120.99999, 37.00001)

      p1.equalsWithinTolerance(p2, 0.0001) should be(true)
      p1.equalsWithinTolerance(p2, 0.00003) should be(true)
      p1.equalsWithinTolerance(p2, 0.00002) should be(false)

      val someCRS = p1.bestUtmCRS

      import someCRS.SOME_CRS

      val p3 = p1.transformCRS[SOME_CRS]
      val p4 = p2.transformCRS[SOME_CRS]

      p3.distanceSquaredTo(p4) should be(8.095486663778642 +- 1e-9)

      p3.equalsWithinTolerance(p4, 3.0) should be(true)
      p3.equalsWithinTolerance(p4, 2.0) should be(false)

      p4.equalsWithinTolerance(p3, 3.0) should be(true)
      p4.equalsWithinTolerance(p3, 2.0) should be(false)

      "p1.equalsWithinTolerance(p3, 3.0)" shouldNot typeCheck
    }

    it("should work for multipoints") {
      val p1 = Point2D[EPSG_32615](100000, 200000)
      val p2 = Point2D[EPSG_32615](100300, 200500)
      val p3 = Point2D[EPSG_32615](100300, 200000)

      val mp = MultiPoint2D.fromPoints(Seq(p1, p2, p3))

      val mpnorm = mp.transformCRS[EPSG_4326]
      val mpback = mpnorm.transformCRS[EPSG_32615]
      val mpnormAgain = mpback.transformCRS[EPSG_4326]

      mp.toWKT should not be mpback.toWKT
      mp should not be mpback
      mp.equalsWithinTolerance(mpback, 1e-4) should be(true)
      mp.equalsWithinTolerance(mpback, 1e-6) should be(false)

      mpnorm.toWKT should not be mpnormAgain.toWKT
      mpnorm should not be mpnormAgain
      mpnorm.equalsWithinTolerance(mpnormAgain, 1e-9) should be(true)
      mpnorm.equalsWithinTolerance(mpnormAgain, 1e-12) should be(false)
    }

    it("should work for linestrings") {
      val p1 = Point2D[EPSG_32615](100000, 200000)
      val p2 = Point2D[EPSG_32615](100300, 200500)
      val p3 = Point2D[EPSG_32615](100300, 200000)

      val ls = LineString2D.fromPoints(Seq(p1, p2, p3))

      val lsnorm = ls.transformCRS[EPSG_4326]
      val lsback = lsnorm.transformCRS[EPSG_32615]
      val lsnormAgain = lsback.transformCRS[EPSG_4326]

      ls.toWKT should not be lsback.toWKT
      ls should not be lsback
      ls.equalsWithinTolerance(lsback) should be(true)
      ls.equalsWithinTolerance(lsback, 1e-4) should be(true)
      ls.equalsWithinTolerance(lsback, 1e-6) should be(false)

      lsnorm.toWKT should not be lsnormAgain.toWKT
      lsnorm should not be lsnormAgain
      lsnorm.equalsWithinTolerance(lsnormAgain) should be(true)
      lsnorm.equalsWithinTolerance(lsnormAgain, 1e-9) should be(true)
      lsnorm.equalsWithinTolerance(lsnormAgain, 1e-12) should be(false)
    }

    it("should work for polygons") {
      val p1 = Point2D[EPSG_32615](100000, 200000)
      val p2 = Point2D[EPSG_32615](100300, 200500)
      val p3 = Point2D[EPSG_32615](100300, 200000)

      val poly = Polygon2D.fromPoints(Seq(p1, p2, p3))

      val polynorm = poly.transformCRS[EPSG_4326]
      val polyback = polynorm.transformCRS[EPSG_32615]
      val polynormAgain = polyback.transformCRS[EPSG_4326]

      poly.toWKT should not be polyback.toWKT
      poly should not be polyback
      poly.equalsWithinTolerance(polyback) should be(true)
      poly.equalsWithinTolerance(polyback, 0.02) should be(true)
      poly.equalsWithinTolerance(polyback, 0.01) should be(false)

      polynorm.toWKT should not be polynormAgain.toWKT
      polynorm should not be polynormAgain
      polynorm.equalsWithinTolerance(polynormAgain) should be(true)
      polynorm.equalsWithinTolerance(polynormAgain, 1e-9) should be(true)
      polynorm.equalsWithinTolerance(polynormAgain, 1e-12) should be(false)

    }

    it("should work for complex, multipolygons") {
      val wkt = FileUtils.readFileToString(
        new File(getClass.getClassLoader.getResource("demodata/geometry/simplification.wkt").toURI),
        Charset.defaultCharset())

      val poly = Geometry.fromWKT[EPSG_4326](wkt) match
        case p: Polygon2D[EPSG_4326] => p

      val holePoly = Geometry.fromWKT[EPSG_4326](
        """Polygon ((-92.89107214796356971 39.12633506482126933, -92.89106954398060623 39.1279625541738767, -92.88899937752410096 39.12792089044644683,
          |-92.88911134879155895 39.12632725287237889, -92.89107214796356971 39.12633506482126933))""".stripMargin)

      val multiPoly = poly.difference(holePoly.asInstanceOf[Polygonal[EPSG_4326]]).get

      multiPoly shouldBe a[MultiPolygon2D[_]]

      val someCRS = multiPoly.centroid.bestUtmCRS
      import someCRS.SOME_CRS

      val transformed = multiPoly.transformCRS[SOME_CRS]

      val back1 = transformed.transformCRS[EPSG_4326]

      val back2 = back1.transformCRS[SOME_CRS]

      multiPoly.equalsWithinTolerance(back1) should be(true)
      multiPoly.equalsWithinTolerance(back1, 1e-18) should be(false)
      back1.equalsWithinTolerance(multiPoly) should be(true)

      val simplified = multiPoly.simplifyTopology(0.0001)
      multiPoly.equalsWithinTolerance(simplified) should be(false)
      multiPoly.equalsWithinTolerance(simplified, 1e-4) should be(true)

      transformed.equalsWithinTolerance(back2) should be(true)
      transformed.equalsWithinTolerance(back2, tolerance = 1e-8) should be(false)
      back2.equalsWithinTolerance(transformed) should be(true)

      "multiPoly.equalsWithinTolerance(back2)" shouldNot typeCheck
    }

  }
