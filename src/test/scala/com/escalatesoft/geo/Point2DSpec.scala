package com.escalatesoft.geo

import com.escalatesoft.geo.crs.CRST
import com.escalatesoft.geo.crs.CRST.CRSDefinitions.{EPSG_32615, EPSG_32616, EPSG_4326}
import org.geotools.geometry.{DirectPosition1D, DirectPosition2D}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import com.escalatesoft.geo.Units.Meters

class Point2DSpec extends AnyFunSpec with Matchers:
  describe("Point2DSpec") {

    it("should transformCRS") {
      val p = Point2D[EPSG_4326](-92.63868149, 43.16017236)
      val p2: Point2D[EPSG_32615] = p.transformCRS[EPSG_32615]
      p2.x should be(529373.876 +- 0.01)
      p2.y should be(4778665.271 +- 0.01)

      val p3 = p2.toCRS[EPSG_4326]
      p3.x should be(-92.638681 +- 0.01)
      p3.y should be(43.160172 +- 0.01)
    }

    it("should construct with the correct values") {
      val p2c = Point2D[EPSG_32615](500010.0, 8500020.0)

      p2c.x should be(500010.0 +- 0.000000001)
      p2c.y should be(8500020.0 +- 0.000000001)
    }

    it("should convert to DirectPosition2D") {
      val p1 = Point2D[EPSG_32615](200.45, 300.60)
      val directposition = p1.toDirectPosition

      directposition.getCoordinate()(0) should be(200.45 +- 0.000001)
      directposition.getCoordinate()(1) should be(300.60 +- 0.000001)
      directposition.getDimension should be(2)
    }

    it("should be able to tell a clearly larger or smaller coordinate") {
      val p2p1 = Point2D[EPSG_32615](500010.0, 8500020.0)
      val p2p2 = Point2D[EPSG_32615](500010.1, 8500020.1)
      val p2p3 = Point2D[EPSG_32615](500009.9, 8500019.9)
      val p2p6 = Point2D[EPSG_32615](500010.0, 8500020.0)

      p2p1 > p2p2 should be(false)
      p2p1 >= p2p2 should be(false)
      p2p1 < p2p2 should be(true)
      p2p1 <= p2p2 should be(true)

      p2p1 < p2p3 should be(false)
      p2p1 <= p2p3 should be(false)
      p2p1 > p2p3 should be(true)
      p2p1 >= p2p3 should be(true)

      p2p1 > p2p6 should be(false)
      p2p1 < p2p6 should be(false)
      p2p1 <= p2p6 should be(true)
      p2p1 >= p2p6 should be(true)

      p2p1 == p2p6 should be(true)
    }

    it("should handle ordering properly even for coordinates 'on the same line'") {
      val p1 = Point2D[EPSG_4326](40, 80)
      val p2 = Point2D[EPSG_4326](41, 80)
      val p3 = Point2D[EPSG_4326](40, 81)

      p1 < p2 should be(true)
      p1 < p3 should be(true)
      p1 == p2 should be(false)
      p1 == p3 should be(false)
    }

    it("should totally order coordinates lexicographically") {
      val p1 = Point2D[EPSG_4326](41, 80)
      val p2 = Point2D[EPSG_4326](40, 81)

      p1 > p2 should be(true)
    }

    it("should map a function to the coordiantes") {
      val p1 = Point2D[EPSG_4326](41, 80)

      val pmap1 = p1.map(2 * _)
      val pmap2 = p1.map(_ => 5)

      pmap1.x should be(82.0 +- 0.01)
      pmap1.y should be(160.0 +- 0.01)

      pmap2.x should be(5.0 +- 0.01)
      pmap2.y should be(5.0 +- 0.01)
    }

    it("should produce WKT") {
      val p1 = Point2D[EPSG_4326](41.2, 80.234)
      val actualWKT = "POINT (41.2 80.234)"
      p1.toWKT should be(actualWKT)
    }

    it("should produce EWKT") {
      val p1 = Point2D[EPSG_4326](41.2, 80.234)
      p1.toEWKT should be("""SRID=4326;POINT (41.2 80.234)""")
    }

    it("should copy Point2D") {
      val p1 = Point2D[EPSG_4326](41, 80)
      val copiedPoint = p1.updated(20, 30)
      copiedPoint.x should be(20.0 +- 0.00000001)
      copiedPoint.y should be(30.0 +- 0.00000001)
    }

    it("should create a rectangle of the appropriate size") {
      val c = Point2D[EPSG_32615](500010.0, 8500020.0)
      val bestUTM: CRST.SomeCRS = c.bestUtmCRS
      import bestUTM.{SOME_CRS => UTM}

      val square = c.toSquare(Meters(2.0))
      val utmFeat = square.transformCRS[UTM]
      val areaInSqMeters = utmFeat.area
      val dist = areaInSqMeters - 4.0

      assert(dist <= 1e-4, (areaInSqMeters, c, utmFeat))
    }

    it("should create a rectangle with a centroid equivalent to the initial point") {
      val c = Point2D[EPSG_32615](500010.0, 8500020.0)

      val rect = c.toSquare(Meters(2.0))

      val cent = rect.centroid
      val dist = Math.sqrt(Math.pow(c.x - cent.x, 2) + Math.pow(c.y - cent.y, 2))
      assert(dist <= 1e-3, (c, cent))
    }

    it("should not be constructable from a DirectPosition1D") {
      val dp = new DirectPosition1D(5000)
      val reason = intercept[IllegalArgumentException] {
        Point2D.fromDirectPosition[EPSG_32615](dp)
      }
      reason.getMessage should include("")
    }

    it("should be constructable from a DirectPosition2D") {
      val dp = new DirectPosition2D(EPSG_32615.crs, 500010.0, 8500020.0)
      val c2c = Point2D.fromDirectPosition[EPSG_32615](dp)
      c2c should be(Point2D[EPSG_32615](500010.0, 8500020.0))
    }

    it("should not be constructable from a DirectPosition2D with a different CRS") {
      val dp = new DirectPosition2D(EPSG_32616.crs, 500010.0, 8500020.0)
      val reason = intercept[IllegalArgumentException] {
        Point2D.fromDirectPosition[EPSG_32615](dp)
      }
      reason.getMessage should include(
        "Incompatible CRS, requested: EPSG:WGS 84 / UTM zone 15N, supplied EPSG:WGS 84 / UTM zone 16N"
      )
    }

    it("should allow conversion between coordinates") {
      val c2c = Point2D[EPSG_32615](500010.0, 8500020.0)
      val transformed: Point2D[EPSG_32616] = c2c.toCRS[EPSG_32616]

      transformed.crs should be(EPSG_32616)
      c2c.crs should be(EPSG_32615)

      transformed.x should be(344828.9284714414 +- 0.000000001)
      transformed.y should be(8507930.42468832 +- 0.000000001)
    }

    it("should allow construction to from any DirectPosition CRS if desired") {
      val dp = new DirectPosition2D(EPSG_32615.crs, 500010.0, 8500020.0)
      val c2c = Point2D.fromDirectPositionAnyCRS[EPSG_32616](dp)

      c2c.crs should be(EPSG_32616)

      c2c.x should be(344828.9284714414 +- 0.000000001)
      c2c.y should be(8507930.42468832 +- 0.000000001)
    }

    it("should calculate pythag distance") {
      val c1 = Point2D[EPSG_32615](500010.0, 8500020.0)
      val c2 = Point2D[EPSG_32615](500040.0, 8500060.0)

      math.sqrt(c1.distanceSquaredTo(c2)) should be(50.0 +- 1e-12)
      math.sqrt(c2.distanceSquaredTo(c1)) should be(50.0 +- 1e-12)

      "c1.distanceSquaredTo(c2.toCRS[EPSG_4326])" shouldNot typeCheck
    }

    it("should have decent correlation between pythag and ortho dist for small distances") {
      import com.escalatesoft.geo.crs.CRST.CRSDefinitions.EPSG_32610
      val rand = new scala.util.Random()

      for _ <- 1 to 1000 do
        val c1 =
          Point2D[EPSG_32610](400000.0 + rand.nextInt(20000), 8000000.0 + rand.nextInt(20000))
        val c2 =
          Point2D[EPSG_32610](400000.0 + rand.nextInt(20000), 8000000.0 + rand.nextInt(20000))

        if c1 != c2 then
          val d1 = math.sqrt(c1.distanceSquaredTo(c2))
          math.sqrt(c2.distanceSquaredTo(c1)) should be(d1 +- 1e-12)

          // Ortho distance should always be at least as large and usually larger than pythag
          c1.distanceOrthoTo(c2) should be >= d1
          c2.distanceOrthoTo(c1) should be >= d1

          // should be correct to within 0.1% at these distances
          (c1.distanceOrthoTo(c2) - d1) / d1 should be < 0.001
          (c2.distanceOrthoTo(c1) - d1) / d1 should be < 0.001

    }

    it("should produce geohash") {
      val p = Point2D[EPSG_4326](41, 80)
      val actualHash = "uyjphgx3c0zu"
      Geometry.singleGeohash(p, 12) should be(actualHash)
    }

    it("should give the correct CRS for coords in Northern Hemisphere") {
      val coords: Point2D[EPSG_4326] = Point2D[EPSG_4326](-93, 39)
      val crs: CRST.SomeCRS = coords.bestUtmCRS
      crs shouldBe CRST.crsFromId("EPSG:32615")
    }
    it("should give the correct CRS for coords in Southern Hemisphere") {
      val coords: Point2D[EPSG_4326] = Point2D[EPSG_4326](-63, -10)
      val crs: CRST.SomeCRS = coords.bestUtmCRS
      crs shouldBe CRST.crsFromId("EPSG:32720")
    }
    it("should update the type when you include an id") {
      val coords: Point2D[EPSG_4326] = Point2D[EPSG_4326](-63, -10)
      val withId: GeometryWithID[EPSG_4326] & Point2D[EPSG_4326] = coords.assignId("test_id")
      withId.geometryId shouldBe "test_id"
    }
  }
