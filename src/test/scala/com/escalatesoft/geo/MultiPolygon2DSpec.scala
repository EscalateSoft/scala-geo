package com.escalatesoft.geo

import com.escalatesoft.geo.crs.CRST.CRSDefinitions.{EPSG_32615, EPSG_4326}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.locationtech.jts.operation.valid.TopologyValidationError

class MultiPolygon2DSpec extends AnyFunSpec with Matchers:
  describe("MultiPolygon2DTest") {

    it("should transformCRS correctly") {
      val rawPoints = Seq(
        RawCoordinate2D(-92.2493031319753, 42.672304270958215),
        RawCoordinate2D(-92.24929402927881, 42.67243292753743),
        RawCoordinate2D(-92.24714959503662, 42.67242300503766),
        RawCoordinate2D(-92.2493031319753, 42.672304270958215)
      )

      val polygon2d1 = Polygon2D[EPSG_4326](rawPoints)
      val polygon2d2 = Polygon2D[EPSG_4326](rawPoints.map(c => RawCoordinate2D(c.x + 0.01, c.y)))

      val mp = MultiPolygon2D.fromPolygons(Seq(polygon2d1, polygon2d2))

      val transformed2d = mp.transformCRS[EPSG_32615]

      val wkt = transformed2d.toWKT
      println(wkt)
      val actualWKT =
        "MULTIPOLYGON (((561512.3702547641 4724698.932194901, 561512.9892547643 4724713.2251949, " +
          "561688.7152547662 4724713.6861948995, 561512.3702547641 4724698.932194901)), " +
          "((562331.7778103662 4724706.257987821, 562332.3951201201 4724720.551078805, " +
          "562508.1212841299 4724721.032869675, 562331.7778103662 4724706.257987821)))"
      wkt should be(actualWKT)

      val polys = transformed2d.asPolygonSeq

      polys.size should be(2)

      val pointsBack = polys(0).getExteriorRing

      pointsBack should have size 4

      pointsBack(0).x should be(561512.3702547651 +- 1e-9)
      pointsBack(0).y should be(4724698.932194901 +- 1e-9)
      pointsBack(1).x should be(561512.9892547653 +- 1e-9)
      pointsBack(1).y should be(4724713.2251949 +- 1e-9)
      pointsBack(2).x should be(561688.7152547662 +- 1e-9)
      pointsBack(2).y should be(4724713.6861948995 +- 1e-9)
      pointsBack(3).x should be(561512.3702547651 +- 1e-9)
      pointsBack(3).y should be(4724698.932194901 +- 1e-9)
    }

    val inner1 = Vector(
      RawCoordinate2D(1, 1),
      RawCoordinate2D(1, 2),
      RawCoordinate2D(2, 2),
      RawCoordinate2D(2, 1)
    )

    val inner2 = Vector(
      RawCoordinate2D(5, 5),
      RawCoordinate2D(5, 6),
      RawCoordinate2D(6, 6),
      RawCoordinate2D(6, 5)
    )

    val inner3 = Vector(
      RawCoordinate2D(1, 5),
      RawCoordinate2D(1, 6),
      RawCoordinate2D(2, 6),
      RawCoordinate2D(2, 5)
    )

    it("should validate a valid polygon") {
      val validMP = MultiPolygon2D.fromPolygons(
        Seq(
          Polygon2D[EPSG_32615](inner1),
          Polygon2D[EPSG_32615](inner2),
          Polygon2D[EPSG_32615](inner3)
        ))

      validMP.isValid should be(true)

      val validated = validMP.validated

      validated match
        case mp: MultiPolygon2D[_] => succeed
        case err: TopologyValidationError => fail(err.toString)      
    }

    it("should not validate an invalid polygon") {
      val inner4 = Vector(
        RawCoordinate2D(1, 1),
        RawCoordinate2D(1, 6),
        RawCoordinate2D(5, 6),
        RawCoordinate2D(5, 1)
      )

      val invalidMP = MultiPolygon2D.fromPolygons(
        Seq(
          Polygon2D[EPSG_32615](inner1),
          Polygon2D[EPSG_32615](inner2),
          Polygon2D[EPSG_32615](inner4)
        ))

      invalidMP.isValid should be(false)

      val validated = invalidMP.validated

      validated match
        case mp: MultiPolygon2D[_] => fail("Should not have validated an invalid multipolygon")
        case err: TopologyValidationError =>
          err.toString should be ("Self-intersection at or near point (1.0, 1.0, NaN)")
    }

  }
