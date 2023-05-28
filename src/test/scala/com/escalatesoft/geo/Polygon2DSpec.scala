package com.escalatesoft.geo

import com.escalatesoft.geo.crs.CRST.CRSDefinitions.EPSG_32615
import com.escalatesoft.geo.crs.CRST.CRSDefinitions.EPSG_32616
import com.escalatesoft.geo.crs.CRST.CRSDefinitions.EPSG_4326
import org.apache.commons.io.FileUtils
import org.locationtech.jts.operation.valid.TopologyValidationError
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.nio.charset.Charset

class Polygon2DSpec extends AnyFunSpec with Matchers:
  describe("PolygonSpec") {

    it("should transformCRS") {
      val rawPoints = Seq(
        RawCoordinate2D(-92.2493031319753, 42.672304270958215),
        RawCoordinate2D(-92.24929402927881, 42.67243292753743),
        RawCoordinate2D(-92.24714959503662, 42.67242300503766),
        RawCoordinate2D(-92.2493031319753, 42.672304270958215)
      )

      val polygon2d = Polygon2D[EPSG_4326](rawPoints)

      val transformed2d = polygon2d.transformCRS[EPSG_32615]

      val wkt = transformed2d.toWKT
      val actualWKT =
        "POLYGON ((561512.3702547641 4724698.932194901, 561512.9892547643 4724713.2251949, " +
          "561688.7152547662 4724713.6861948995, 561512.3702547641 4724698.932194901))"
      wkt should be(actualWKT)

      val pointsBack = transformed2d.getExteriorRing

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

    it("should close the polygon if not already closed") {
      val rawPoints = Seq(
        RawCoordinate2D(-92.2493031319753, 42.672304270958215),
        RawCoordinate2D(-92.24929402927881, 42.67243292753743),
        RawCoordinate2D(-92.24714959503662, 42.67242300503766)
      )

      val polygon2d = Polygon2D[EPSG_4326](rawPoints)

      val transformed2d = polygon2d.transformCRS[EPSG_32615]

      val wkt = transformed2d.toWKT
      val actualWKT =
        "POLYGON ((561512.3702547641 4724698.932194901, 561512.9892547643 4724713.2251949, " +
          "561688.7152547662 4724713.6861948995, 561512.3702547641 4724698.932194901))"
      wkt should be(actualWKT)

      val pointsBack = transformed2d.getExteriorRing

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

    val outerRing = Vector(
      RawCoordinate2D(0, 0),
      RawCoordinate2D(10, 0),
      RawCoordinate2D(10, 10),
      RawCoordinate2D(0, 10)
    )

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
      val validPolygonWithHoles = Polygon2D[EPSG_32615](outerRing, Seq(inner1, inner2, inner3))
      validPolygonWithHoles.isValid should be(true)

      validPolygonWithHoles.validated match
        case poly: Polygon2D[_] => poly should be(validPolygonWithHoles)
        case _ => fail("should have been an identical polygon")
    }

    it("should not validate an invalid polygon") {
      val inner4 = Vector(
        RawCoordinate2D(1, 1),
        RawCoordinate2D(1, 6),
        RawCoordinate2D(5, 6),
        RawCoordinate2D(5, 1)
      )

      val invalidPoly = Polygon2D[EPSG_32615](outerRing, Seq(inner1, inner2, inner3, inner4))
      invalidPoly.isValid should be(false)

      invalidPoly.validated match
        case poly: Polygon2D[_] => fail("should not have been a valid polygon")
        case err: TopologyValidationError =>
          err.toString should be ("Self-intersection at or near point (1.0, 1.0, NaN)")
    }

    it("should produce correct WKT and eWKT when requested") {
      val validPolygonWithHoles = Polygon2D[EPSG_32615](outerRing, Seq(inner1, inner2, inner3))

      validPolygonWithHoles.toWKT should be(
        "POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0), (1 1, 1 2, 2 2, 2 1, 1 1), (5 5, 5 6, 6 6, 6 5, 5 5), (1 5, 1 6, 2 6, 2 5, 1 5))"
      )

      validPolygonWithHoles.toEWKT should be(
        "SRID=32615;POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0), (1 1, 1 2, 2 2, 2 1, 1 1), (5 5, 5 6, 6 6, 6 5, 5 5), (1 5, 1 6, 2 6, 2 5, 1 5))"
      )
    }

    it("should produce correct WKB (well known binary)") {
      val validPolygonWithHoles = Polygon2D[EPSG_32615](outerRing, Seq(inner1, inner2, inner3))

      val wkb = validPolygonWithHoles.toWKB

      wkb should be(
        Array[Byte](0, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 64, 36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64, 36, 0, 0, 0, 0, 0, 0, 64, 36,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64, 36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 63, -16, 0, 0, 0, 0, 0, 0, 63, -16, 0, 0, 0, 0, 0,
          0, 63, -16, 0, 0, 0, 0, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 64, 0, 0,
          0, 0, 0, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 63, -16, 0, 0, 0, 0, 0, 0, 63, -16, 0, 0, 0, 0, 0,
          0, 63, -16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 64, 20, 0, 0, 0, 0, 0, 0, 64, 20, 0, 0, 0, 0, 0,
          0, 64, 20, 0, 0, 0, 0, 0, 0, 64, 24, 0, 0, 0, 0, 0, 0, 64, 24, 0, 0, 0, 0, 0, 0, 64, 24,
          0, 0, 0, 0, 0, 0, 64, 24, 0, 0, 0, 0, 0, 0, 64, 20, 0, 0, 0, 0, 0, 0, 64, 20, 0, 0, 0, 0,
          0, 0, 64, 20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 63, -16, 0, 0, 0, 0, 0, 0, 64, 20, 0, 0, 0, 0,
          0, 0, 63, -16, 0, 0, 0, 0, 0, 0, 64, 24, 0, 0, 0, 0, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 64,
          24, 0, 0, 0, 0, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 64, 20, 0, 0, 0, 0, 0, 0, 63, -16, 0, 0, 0,
          0, 0, 0, 64, 20, 0, 0, 0, 0, 0, 0)
      )
    }

    it("should simplify a complex polygon correctly") {
      val wkt = FileUtils.readFileToString(
        new File(getClass.getClassLoader.getResource("demodata/geometry/simplification.wkt").toURI),
        Charset.defaultCharset())

      val poly = Geometry.fromWKT[EPSG_4326](wkt) match
        case p: Polygon2D[EPSG_4326] => p

      poly.getExteriorRing should have size (215)
      poly.getInteriorRings should have size (1)
      poly.getInteriorRings.head should have size (25)

      val simplified = poly.simplifyTopology(0.0001) match
        case p: Polygon2D[EPSG_4326] => p

      simplified.getExteriorRing should have size (66)
      simplified.getInteriorRings should have size (1)
      simplified.getInteriorRings.head should have size (9)

      val simplifiedCompare =
        """
          |POLYGON ((-92.88579683667012 39.127578049522455, -92.88583458617671 39.12734930225897, -92.88546060487447
          | 39.12699628378146, -92.8855906187656 39.12478661408038, -92.88587268825619 39.124428570792354, -92.88621724692139
          | 39.12449599570833, -92.88680765500008 39.124230137501776, -92.88697441782213 39.124316968657766, -92.8870637848933
          | 39.12410038190184, -92.88744206396697 39.12411808524348, -92.88911840552477 39.12299299014168, -92.89056478964439
          | 39.12304635134961, -92.89080899190763 39.123419264019496, -92.89088558597979 39.12426003209493, -92.89074482214183
          | 39.125343267019325, -92.89037646927586 39.12609694795564, -92.89046491725976 39.126461045844614, -92.89099014024505
          | 39.1250054880753, -92.89098164283146 39.12329809030402, -92.89114907550446 39.12295156929954, -92.89259067731456
          | 39.12294742770588, -92.89233902793943 39.123342686505765, -92.89262665132476 39.12444365621335, -92.8929264757348
          | 39.12484261278363, -92.89271608014042 39.1241750617642, -92.89296204213458 39.12339526364825, -92.89270679772949
          | 39.12295010282206, -92.89484174705855 39.12299913159939, -92.89455749620079 39.125948777582025, -92.89366143797368
          | 39.12582207772764, -92.89261282450319 39.12622036483711, -92.89249006546238 39.126471599919434, -92.89292357583167
          | 39.12675800406773, -92.89215472235027 39.12745931848969, -92.89249939160078 39.12765031292242, -92.89345649950164
          | 39.12753851614499, -92.89440766557574 39.12789415985479, -92.89420123310143 39.128374627713335, -92.89204153581177
          | 39.128258205871816, -92.89133720090435 39.12765100153657, -92.89085236939738 39.127627660243675, -92.89052901864676
          | 39.12769603105077, -92.89117298972961 39.129333149844754, -92.89062985153166 39.1290007607171, -92.88989498323794
          | 39.12912595472726, -92.88911864605844 39.12759169150095, -92.88919286378409 39.12711110699, -92.8893401027605
          | 39.12690525180922, -92.88966337331573 39.12689411407317, -92.88984018578303 39.12657383552813, -92.88935568443503
          | 39.126344502264736, -92.88900271927866 39.12656160594595, -92.88904629465556 39.12688209319814, -92.8873857015161
          | 39.12707504264493, -92.88744325238376 39.12784186651699, -92.88670862211936 39.127829710688985, -92.88720723392005
          | 39.12842529089811, -92.88798584098882 39.12850615085077, -92.88848463073913 39.12899872821799, -92.88755912210472
          | 39.12889484166193, -92.88768737449494 39.12903014162166, -92.88760255346556 39.12929542932612, -92.88698522804394
          | 39.129432162425076, -92.88623632991222 39.12914532983081, -92.8859130231912 39.129179346347435, -92.88579683667012 39.127578049522455),
          | (-92.8875973125643 39.12444758396932, -92.88734118016299 39.12458600045654, -92.88734089024526 39.12476800759522,
          | -92.88781869361395 39.125175817872766, -92.88797465583745 39.125071961051134, -92.88796422897848 39.124621275873665,
          | -92.88818754845151 39.12411880267449, -92.88778695390005 39.12414441494783, -92.8875973125643 39.12444758396932))
          |""".stripMargin.replace("\n", "")

      simplified.toWKT should be(simplifiedCompare)

      poly.area should be(simplified.area +- 0.01)
    }

    it("should simplify to a multipolygon when necessary") {
      val poly1 = Polygon2D[EPSG_32616](
        Seq(
          RawCoordinate2D(1, 1),
          RawCoordinate2D(1, 9),
          RawCoordinate2D(9, 9),
          RawCoordinate2D(9, 1)
        ))

      val poly2 = Polygon2D[EPSG_32616](
        Seq(
          RawCoordinate2D(0, 3),
          RawCoordinate2D(10, 3),
          RawCoordinate2D(10, 5),
          RawCoordinate2D(0, 5)
        ))

      val diffPoly = poly1.difference(poly2).get

      val mp = diffPoly.simplifyTopology(0.00001)

      mp shouldBe a[MultiPolygon2D[_]]
      mp.toWKT should be("MULTIPOLYGON (((1 1, 1 3, 9 3, 9 1, 1 1)), ((1 5, 1 9, 9 9, 9 5, 1 5)))")
    }

    it("should correctly determine if points are inside or outside the polygon") {
      val poly = Polygon2D[EPSG_32615](outerRing, Seq(inner1, inner2, inner3))

      // points on the edges are not contained
      val allPoints = (poly.getExteriorRing +: poly.getInteriorRings).flatten

      for point <- allPoints do
        poly.contains(point) should be(false)

      poly.contains(Point2D[EPSG_32615](0.5, 0.5)) should be(true)

      // points in holes should be outside
      poly.contains(Point2D[EPSG_32615](1.5, 1.5)) should be(false)

      // and something way out there should be outside too
      poly.contains(Point2D[EPSG_32615](1000, 1000)) should be(false)
      poly.contains(Point2D[EPSG_32615](-1000, -1000)) should be(false)

      // and the wrong CRS should not even compile
      "poly.contains(Point2D[EPSG_4326](0, 0))" shouldNot typeCheck
    }

    it("should correctly determine if points are covered by the polygon") {
      val poly = Polygon2D[EPSG_32615](outerRing, Seq(inner1, inner2, inner3))

      // points on the edges are covered
      val allPoints = (poly.getExteriorRing +: poly.getInteriorRings).flatten

      for point <- allPoints do
        poly.covers(point) should be(true)

      poly.covers(Point2D[EPSG_32615](0.5, 0.5)) should be(true)

      // points in holes should be outside
      poly.covers(Point2D[EPSG_32615](1.5, 1.5)) should be(false)

      // and something way out there should be outside too
      poly.covers(Point2D[EPSG_32615](1000, 1000)) should be(false)
      poly.covers(Point2D[EPSG_32615](-1000, -1000)) should be(false)

      // and the wrong CRS should not even compile
      "poly.covers(Point2D[EPSG_4326](0, 0))" shouldNot typeCheck
    }

    it("should correctly determine if other polygons are contained within a polygon") {
      val poly = Polygon2D[EPSG_32615](outerRing, Seq(inner1, inner2, inner3))

      // The polygon should contain itself
      poly.contains(poly) should be(true)

      // it should contain a small polygon inside this one
      val smallPoly = Polygon2D[EPSG_32615](
        Seq(
          RawCoordinate2D(0.25, 0.25),
          RawCoordinate2D(0.25, 0.75),
          RawCoordinate2D(0.75, 0.75),
          RawCoordinate2D(0.75, 0.25)
        ))

      poly.contains(smallPoly) should be(true)

      // it should not contain a small polygon outside of this one
      val smallPoly2 = Polygon2D[EPSG_32615](
        Seq(
          RawCoordinate2D(100.25, 100.25),
          RawCoordinate2D(100.25, 100.75),
          RawCoordinate2D(100.75, 100.75),
          RawCoordinate2D(100.75, 100.25)
        ))

      poly.contains(smallPoly2)

      // it should not contain a large poly that completely covers this one
      val largePoly = Polygon2D[EPSG_32615](
        Seq(
          RawCoordinate2D(-100.25, -100.25),
          RawCoordinate2D(-100.25, 100.75),
          RawCoordinate2D(100.75, 100.75),
          RawCoordinate2D(100.75, -100.25)
        ))

      poly.contains(largePoly) should be(false)
    }

    it("should correctly determine if other polygons are covered by a polygon") {
      val poly = Polygon2D[EPSG_32615](outerRing, Seq(inner1, inner2, inner3))

      // The polygon should cover itself
      poly.covers(poly) should be(true)

      // it should cover a small polygon inside this one
      val smallPoly = Polygon2D[EPSG_32615](
        Seq(
          RawCoordinate2D(0.25, 0.25),
          RawCoordinate2D(0.25, 0.75),
          RawCoordinate2D(0.75, 0.75),
          RawCoordinate2D(0.75, 0.25)
        ))

      poly.covers(smallPoly) should be(true)

      // it should not cover a small polygon outside of this one
      val smallPoly2 = Polygon2D[EPSG_32615](
        Seq(
          RawCoordinate2D(100.25, 100.25),
          RawCoordinate2D(100.25, 100.75),
          RawCoordinate2D(100.75, 100.75),
          RawCoordinate2D(100.75, 100.25)
        ))

      poly.covers(smallPoly2)

      // it should not cover a large poly that completely covers this one
      val largePoly = Polygon2D[EPSG_32615](
        Seq(
          RawCoordinate2D(-100.25, -100.25),
          RawCoordinate2D(-100.25, 100.75),
          RawCoordinate2D(100.75, 100.75),
          RawCoordinate2D(100.75, -100.25)
        ))

      poly.covers(largePoly) should be(false)
    }

    def checkPolygon2D(p: Polygonal[EPSG_32615]): Unit =
      p match
        case _: Polygon2D[EPSG_32615] =>
        case _                        => fail("wrong resulting type")

    it("should intersect with other polygons correctly") {
      val poly = Polygon2D[EPSG_32615](outerRing, Seq(inner1, inner2, inner3))

      // The polygon should produce itself when intersecting with itself
      val intersection1: Polygonal[EPSG_32615] = poly.intersection(poly).get
      intersection1.equalsWithinTolerance(poly, 1e-8) should be(true)

      // the result should also be of type Polygon2D
      checkPolygon2D(intersection1)

      // when intersected with a small fully contained poly, the result should be that small poly
      val smallPoly = Polygon2D[EPSG_32615](
        Seq(
          RawCoordinate2D(0.25, 0.25),
          RawCoordinate2D(0.25, 0.75),
          RawCoordinate2D(0.75, 0.75),
          RawCoordinate2D(0.75, 0.25)
        ))

      val intersection2 = poly.intersection(smallPoly).get
      intersection2.equalsWithinTolerance(smallPoly, 1e-9) should be(true)
      checkPolygon2D(intersection2)

      // it should not contain a small polygon outside of this one
      val smallPoly2 = Polygon2D[EPSG_32615](
        Seq(
          RawCoordinate2D(100.25, 100.25),
          RawCoordinate2D(100.25, 100.75),
          RawCoordinate2D(100.75, 100.75),
          RawCoordinate2D(100.75, 100.25)
        ))

      val emptyPoly = poly.intersection(smallPoly2).get
      emptyPoly.toWKT should be("POLYGON EMPTY")
      emptyPoly match
        case p2d: Polygon2D[EPSG_32615] =>
          p2d.getExteriorRing.size should be(0)
          p2d.getInteriorRings.size should be(0)
        case _ => fail("Did not get a polygon 2d back")

      val empty2 = Polygon2D[EPSG_32615](Nil)
      emptyPoly.equalsWithinTolerance(empty2, 1e-9) should be(true)

      // it should stay the same poly when intersected with one that completely covers it
      val largePoly = Polygon2D[EPSG_32615](
        Seq(
          RawCoordinate2D(-100.25, -100.25),
          RawCoordinate2D(-100.25, 100.75),
          RawCoordinate2D(100.75, 100.75),
          RawCoordinate2D(100.75, -100.25)
        ))

      val intersection3 = poly.intersection(largePoly).get

      intersection3.equalsWithinTolerance(poly, 1e-9) should be(true)
      checkPolygon2D(intersection3)
    }

    it("should correctly perform differences between polygons") {
      val polyWithHoles = Polygon2D[EPSG_32615](outerRing, Seq(inner1, inner2, inner3))
      val polyWithoutHoles = Polygon2D[EPSG_32615](outerRing)

      polyWithHoles.equalsWithinTolerance(polyWithoutHoles, 1e-9) should be(false)

      val poly1Hole = polyWithoutHoles.difference(Polygon2D[EPSG_32615](inner1)).get
      val poly2Holes = poly1Hole.difference(Polygon2D[EPSG_32615](inner2)).get
      val poly3Holes = poly2Holes.difference(Polygon2D[EPSG_32615](inner3)).get

      checkPolygon2D(poly1Hole)
      checkPolygon2D(poly2Holes)
      checkPolygon2D(poly3Holes)

      polyWithHoles.equalsWithinTolerance(poly1Hole, 1e-9) should be(false)
      polyWithHoles.equalsWithinTolerance(poly2Holes, 1e-9) should be(false)
      polyWithHoles.equalsWithinTolerance(poly3Holes, 1e-9) should be(true)

    }

    it("may produce a multi-polygon when differencing polygons") {
      val poly1 = Polygon2D[EPSG_32616](
        Seq(
          RawCoordinate2D(1, 1),
          RawCoordinate2D(1, 9),
          RawCoordinate2D(9, 9),
          RawCoordinate2D(9, 1)
        ))

      val poly2 = Polygon2D[EPSG_32616](
        Seq(
          RawCoordinate2D(0, 3),
          RawCoordinate2D(10, 3),
          RawCoordinate2D(10, 5),
          RawCoordinate2D(0, 5)
        ))

      val diffPoly = poly1.difference(poly2).get

      diffPoly.toWKT should be(
        "MULTIPOLYGON (((1 1, 1 3, 9 3, 9 1, 1 1)), ((1 5, 1 9, 9 9, 9 5, 1 5)))")

      diffPoly match
        case _: MultiPolygon2D[EPSG_32616] =>
        case _                             => fail("did not get expected multipolygon2d type")

      // we should be able to get 2 polygons out of the multipolygon
      val separatePolys = diffPoly.asPolygonSeq
      separatePolys should have size (2)
      separatePolys.head.toWKT should be("POLYGON ((1 1, 1 3, 9 3, 9 1, 1 1))")
      separatePolys(1).toWKT should be("POLYGON ((1 5, 1 9, 9 9, 9 5, 1 5))")

      // and the multipolygon when forced should be the same instance as the polygonal
      diffPoly.asMultiPolygon eq diffPoly should be(true)
    }

    it("should union multi-polygons back to a single polygon when the difference is filled in") {
      val poly1 = Polygon2D[EPSG_32616](
        Seq(
          RawCoordinate2D(1, 1),
          RawCoordinate2D(1, 9),
          RawCoordinate2D(9, 9),
          RawCoordinate2D(9, 1)
        ))

      val poly2 = Polygon2D[EPSG_32616](
        Seq(
          RawCoordinate2D(0, 3),
          RawCoordinate2D(10, 3),
          RawCoordinate2D(10, 5),
          RawCoordinate2D(0, 5)
        ))

      val diffPoly = poly1.difference(poly2).get

      diffPoly.toWKT should be(
        "MULTIPOLYGON (((1 1, 1 3, 9 3, 9 1, 1 1)), ((1 5, 1 9, 9 9, 9 5, 1 5)))")

      val holeFillPoly = Polygon2D[EPSG_32616](
        Seq(
          RawCoordinate2D(1, 3),
          RawCoordinate2D(9, 3),
          RawCoordinate2D(9, 5),
          RawCoordinate2D(1, 5)
        ))

      val union1 = diffPoly.union(holeFillPoly)

      union1.get.toWKT should be("POLYGON ((1 1, 1 3, 1 5, 1 9, 9 9, 9 5, 9 3, 9 1, 1 1))")
      union1.get shouldBe a[Polygon2D[_]]

      val union2 = Polygonal.unionAll(Seq(poly1, poly2, holeFillPoly))

      union2.get.toWKT should be(
        "POLYGON ((1 3, 0 3, 0 5, 1 5, 1 9, 9 9, 9 5, 10 5, 10 3, 9 3, 9 1, 1 1, 1 3))")
      union2.get shouldBe a[Polygon2D[_]]
    }

    it("should union to multipolygon when necessary") {
      val inner1P = Polygon2D[EPSG_32616](inner1)
      val inner2P = Polygon2D[EPSG_32616](inner2)
      val inner3P = Polygon2D[EPSG_32616](inner3)

      val union1 = inner1P.union(inner2P)

      union1.get.toWKT should be(
        "MULTIPOLYGON (((1 1, 1 2, 2 2, 2 1, 1 1)), ((5 5, 5 6, 6 6, 6 5, 5 5)))")

      union1.get shouldBe a[MultiPolygon2D[_]]

      val union2 = Polygonal.unionAll(Seq(inner1P, inner2P, inner3P))

      union2.get shouldBe a[MultiPolygon2D[_]]

      val mp = Polygon2D[EPSG_32616](outerRing, Seq(inner1, inner2, inner3))

      val union3 = Polygonal.unionAll(Seq(mp, inner1P, inner2P))

      union3.get.toWKT should be(
        "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0), (1 5, 2 5, 2 6, 1 6, 1 5))")
      union3.get shouldBe a[Polygon2D[_]]
    }

    it("should test intersection correctly for all known geometry types") {
      val poly1 = Polygon2D[EPSG_32616](
        Seq(
          RawCoordinate2D(1, 1),
          RawCoordinate2D(1, 9),
          RawCoordinate2D(9, 9),
          RawCoordinate2D(9, 1)
        ))

      val p1 = Point2D[EPSG_32616](0, 0)
      val p2 = Point2D[EPSG_32616](1, 1)
      val p3 = Point2D[EPSG_32616](2, 2)
      val p4 = Point2D[EPSG_32616](10.5, 10.5)
      val p5 = Point2D[EPSG_32616](11.5, 10.5)
      // points
      poly1.intersects(p1) should be(false)
      poly1.intersects(p2) should be(true)
      poly1.intersects(p3) should be(true)
      poly1.intersects(p4) should be(false)

      // multipoints
      poly1.intersects(MultiPoint2D.fromPoints(Seq(p1, p4))) should be(false)
      poly1.intersects(MultiPoint2D.fromPoints(Seq(p1, p2))) should be(true)
      poly1.intersects(MultiPoint2D.fromPoints(Seq(p1, p2, p3, p4))) should be(true)

      // lineStrings
      val ls1 = LineString2D.fromPoints(Seq(p4, p5))
      poly1.intersects(ls1) should be(false)
      val ls2 = LineString2D.fromPoints(Seq(p1, p4))
      poly1.intersects(ls2) should be(true)
      val ls3 = LineString2D.fromPoints(Seq(p1, p4, p5))
      poly1.intersects(ls3) should be(true)

      // multilinestrings
      poly1.intersects(MultiLineString2D(Seq(ls1, ls1))) should be(false)
      poly1.intersects(MultiLineString2D(Seq(ls1, ls2))) should be(true)
      poly1.intersects(MultiLineString2D(Seq(ls3, ls2))) should be(true)

      // polygons
      val poly2 = Envelope2D(Point2D[EPSG_32616](0, 0), Point2D[EPSG_32616](0.5, 0.5)).boundsPolygon
      val poly3 =
        Envelope2D(Point2D[EPSG_32616](0, 0), Point2D[EPSG_32616](10.5, 10.5)).boundsPolygon
      val poly4 = Envelope2D(p1, p2).boundsPolygon
      val poly5 = Envelope2D(p2, p3).boundsPolygon
      val poly6 =
        Envelope2D(Point2D[EPSG_32616](11.5, 11.5), Point2D[EPSG_32616](10.5, 10.5)).boundsPolygon

      poly1.intersects(poly2) should be(false)
      poly1.intersects(poly3) should be(true)
      poly1.intersects(poly4) should be(true)
      poly1.intersects(poly5) should be(true)
      poly1.intersects(poly6) should be(false)

      // multipolygons
      poly1.intersects(MultiPolygon2D.fromPolygons(Seq(poly2, poly6))) should be(false)
      poly1.intersects(MultiPolygon2D.fromPolygons(Seq(poly2, poly5))) should be(true)
    }
  }
