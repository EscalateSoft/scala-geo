package com.escalatesoft.geo

import java.io.File
import java.nio.charset.Charset

import com.escalatesoft.geo.crs.CRST.CRSDefinitions.EPSG_4326
import org.apache.commons.io.FileUtils
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class GeohashingSpec extends AnyFunSpec with Matchers:
  describe("Geohashing") {
    describe("for points") {
      it("should produce a very accurate geohash") {
        val point = Point2D[EPSG_4326](-121.6544, 37.1305)

        Geometry.singleGeohash(point) should be("9q97pxmuz4be")
      }

      it("should be the same when the point is very, very close") {
        val point = Point2D[EPSG_4326](-121.65440000001, 37.13050000001)

        Geometry.singleGeohash(point) should be("9q97pxmuz4be")
      }

      it("should give back only one geohash for a point, even if more are allowed") {
        val point = Point2D[EPSG_4326](-121.6544, 37.1305)

        val hashes = Geometry.geohashSet(point, 12)
        hashes.size should be(1)
        hashes should be(Set("9q97pxmuz4be"))

        val hashes2 = Geometry.geohashDefaultSet(point)
        hashes2.size should be(1)
        hashes2 should be(Set("9q97pxmuz4be"))
      }
    }

    describe("for linestrings") {
      it("should give back a suitable length geohash for a lineString, covering the whole thing") {
        val line = LineString2D.fromPoints(
          Seq(
            Point2D[EPSG_4326](-121.6544, 37.1305),
            Point2D[EPSG_4326](-121.7, 37.2)
          ))

        val linebb = line.boundingEnvelope

        val singleHash = Geometry.singleGeohash(line)
        singleHash should be("9q97")

        val geohashBB = Envelope2D.fromGeohash(singleHash).get

        // geohash BB should be strictly a superset of line BB
        linebb.minDiag.x should be >= geohashBB.minDiag.x
        linebb.minDiag.y should be >= geohashBB.minDiag.y
        linebb.maxDiag.x should be <= geohashBB.maxDiag.x
        linebb.maxDiag.y should be <= geohashBB.maxDiag.y
      }

      it("should give back refined geohashes for a multi-segment linestring") {
        val line = LineString2D.fromPoints(
          Seq(
            Point2D[EPSG_4326](-121.6544, 37.1305),
            Point2D[EPSG_4326](-121.7, 37.2),
            Point2D[EPSG_4326](-121.7, 37.1305)
          ))

        val geohashes = Geometry.geohashSet(line, 6)

        val geohashPoly = Polygonal.fromGeohashSet(geohashes)

        geohashes should be(
          Set(
            "9q97rk",
            "9q97r9",
            "9q97rn",
            "9q97wf",
            "9q97qw",
            "9q97wc",
            "9q97px",
            "9q97qt",
            "9q97w9",
            "9q97qx",
            "9q97q9",
            "9q97rh",
            "9q97q8",
            "9q97wd",
            "9q97r6",
            "9q97qs",
            "9q97rp",
            "9q97w8",
            "9q97qd",
            "9q97we",
            "9q97r7",
            "9q97nx",
            "9q97qe",
            "9q97r3",
            "9q97rj",
            "9q97qz",
            "9q97ws",
            "9q97wb",
            "9q97r8"
          ))

        geohashPoly.isSuccess should be(true)
      }

      it("should work for a default set of geohashes for a feature") {
        val line = LineString2D.fromPoints(
          Seq(
            Point2D[EPSG_4326](-121.6544, 37.1305),
            Point2D[EPSG_4326](-121.7, 37.2),
            Point2D[EPSG_4326](-121.7, 37.1305)
          ))

        val linebb = line.boundingEnvelope

        val geohashes = Geometry.geohashDefaultSet(line)

        geohashes should be(Set("9q97p", "9q97w", "9q97q", "9q97r", "9q97n"))

        val geohashPoly = Polygonal.fromGeohashSet(geohashes).get

        geohashPoly.contains(line) should be(true)

        val geohashBB = geohashPoly.boundingEnvelope

        // geohash BB should be strictly a superset of line BB
        linebb.minDiag.x should be >= geohashBB.minDiag.x
        linebb.minDiag.y should be >= geohashBB.minDiag.y
        linebb.maxDiag.x should be <= geohashBB.maxDiag.x
        linebb.maxDiag.y should be <= geohashBB.maxDiag.y

      }
    }

    describe("for polygons") {
      it("should give back geohashes covering a poly") {
        val wkt = FileUtils.readFileToString(
          new File(
            getClass.getClassLoader.getResource("demodata/geometry/simplification.wkt").toURI),
          Charset.defaultCharset())

        val poly = Geometry.fromWKT[EPSG_4326](wkt) match
          case p: Polygon2D[EPSG_4326] => p

        val geohashes = Geometry.geohashDefaultSet(poly)

        geohashes should be(Set("9yvyw4", "9yvyw5", "9yvyw6", "9yvyw7"))

        val geohashes2 = Geometry.geohashSet(poly, 7)

        geohashes2 should be(
          Set(
            "9yvyw6b",
            "9yvyw4t",
            "9yvyw66",
            "9yvyw4x",
            "9yvyw4k",
            "9yvyw6f",
            "9yvyw71",
            "9yvyw5j",
            "9yvyw4u",
            "9yvyw4q",
            "9yvyw63",
            "9yvyw4n",
            "9yvyw60",
            "9yvyw74",
            "9yvyw4j",
            "9yvyw4m",
            "9yvyw5n",
            "9yvyw70",
            "9yvyw5h",
            "9yvyw4r",
            "9yvyw6d",
            "9yvyw4z",
            "9yvyw5q",
            "9yvyw73",
            "9yvyw4v",
            "9yvyw68",
            "9yvyw4p",
            "9yvyw62",
            "9yvyw76",
            "9yvyw6c",
            "9yvyw4s",
            "9yvyw5p",
            "9yvyw4w",
            "9yvyw69",
            "9yvyw4h",
            "9yvyw4y"
          ))
      }
    }

    describe("for multipolygons") {
      it("should give back the geohashes covering a multipolygon") {
        val wkt = FileUtils.readFileToString(
          new File(
            getClass.getClassLoader.getResource("demodata/geometry/simplification.wkt").toURI),
          Charset.defaultCharset())

        val poly = Geometry.fromWKT[EPSG_4326](wkt) match
          case p: Polygon2D[EPSG_4326] => p

        val holePoly = Geometry.fromWKT[EPSG_4326](
          """Polygon ((-92.89107214796356971 39.12633506482126933, -92.89106954398060623 39.1279625541738767, -92.88899937752410096 39.12792089044644683,
            |-92.88911134879155895 39.12632725287237889, -92.89107214796356971 39.12633506482126933))""".stripMargin)

        val multiPoly = poly.difference(holePoly.asInstanceOf[Polygonal[EPSG_4326]])

        multiPoly.get shouldBe a[MultiPolygon2D[_]]

        val geohashSet1 = Geometry.geohashDefaultSet(multiPoly.get)

        geohashSet1 should be(Set("9yvyw4", "9yvyw5", "9yvyw6", "9yvyw7"))

        val geohashSet2 = Geometry.geohashSet(multiPoly.get, 7)

        geohashSet2 should be(
          Set(
            "9yvyw6b",
            "9yvyw4t",
            "9yvyw66",
            "9yvyw4x",
            "9yvyw4k",
            "9yvyw6f",
            "9yvyw71",
            "9yvyw5j",
            "9yvyw4u",
            "9yvyw4q",
            "9yvyw63",
            "9yvyw4n",
            "9yvyw60",
            "9yvyw74",
            "9yvyw4j",
            "9yvyw4m",
            "9yvyw5n",
            "9yvyw70",
            "9yvyw5h",
            "9yvyw4r",
            "9yvyw6d",
            "9yvyw5q",
            "9yvyw73",
            "9yvyw4v",
            "9yvyw68",
            "9yvyw4p",
            "9yvyw62",
            "9yvyw76",
            "9yvyw6c",
            "9yvyw4s",
            "9yvyw5p",
            "9yvyw4w",
            "9yvyw69",
            "9yvyw4h",
            "9yvyw4y"
          ))

      }
    }
  }
