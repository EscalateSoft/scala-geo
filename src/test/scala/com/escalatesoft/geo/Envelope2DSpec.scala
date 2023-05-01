package com.escalatesoft.geo

import com.escalatesoft.geo.crs.CRST.CRSDefinitions
import com.escalatesoft.geo.crs.CRST.CRSDefinitions.EPSG_4326
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

class Envelope2DSpec extends AnyFunSpec with Matchers:
  CRSDefinitions()

  describe("Envelope2DTest") {
    describe("for all of our geometries") {

      it("should create a bounds polygon") {
        val envelope = Envelope2D[EPSG_4326](Point2D(-121.0, 37.0), Point2D(-122.0, 38.0))
        envelope.minDiag should be(Point2D[EPSG_4326](-122.0, 37.0))
        envelope.maxDiag should be(Point2D[EPSG_4326](-121.0, 38.0))
        envelope.boundsPolygon shouldBe a[Polygon2D[_]]
        envelope.boundsPolygon.toWKT should be(
          "POLYGON ((-122 37, -122 38, -121 38, -121 37, -122 37))")

        val envelope2 = Envelope2D[EPSG_4326](Point2D(-122.0, 38.0), Point2D(-121.0, 37.0))
        envelope2.minDiag should be(Point2D[EPSG_4326](-122.0, 37.0))
        envelope2.maxDiag should be(Point2D[EPSG_4326](-121.0, 38.0))
        envelope2.boundsPolygon shouldBe a[Polygon2D[_]]
        envelope2.boundsPolygon.toWKT should be(
          "POLYGON ((-122 37, -122 38, -121 38, -121 37, -122 37))")
      }

      it("should combine two envelopes into a new one covering both entirely") {
        val envelope = Envelope2D[EPSG_4326](Point2D(-121.0, 37.0), Point2D(-122.0, 38.0))
        val envelope2 = Envelope2D[EPSG_4326](Point2D(-123.0, 38.0), Point2D(-122.0, 37.0))

        val combined = envelope.combineWith(envelope2)

        combined.minDiag should be(Point2D[EPSG_4326](-123.0, 37.0))
        combined.maxDiag should be(Point2D[EPSG_4326](-121.0, 38.0))

        combined.boundsPolygon.toWKT should be(
          "POLYGON ((-123 37, -123 38, -121 38, -121 37, -123 37))"
        )
      }

      it("should work for all geometry types") {
        // for a point
        val point = Point2D[EPSG_4326](-121.0, 37.0)
        point.boundingEnvelope should be(Envelope2D[EPSG_4326](point, point))
        point.boundingEnvelope.centroid should be(point)
        point.boundingEnvelope.width should be(0.0)
        point.boundingEnvelope.height should be(0.0)
        point.boundingEnvelope.area should be(0.0)

        // for a LineString
        val point2 = Point2D[EPSG_4326](-121.5, 37.0)
        val point3 = Point2D[EPSG_4326](-121.5, 37.5)

        val lineString2D = LineString2D.fromPoints[EPSG_4326](Seq(point3, point, point2))
        lineString2D.boundingEnvelope should be(
          Envelope2D[EPSG_4326](point2, Point2D[EPSG_4326](-121.0, 37.5)))
        lineString2D.boundingEnvelope.centroid should be(Point2D[EPSG_4326](-121.25, 37.25))
        lineString2D.boundingEnvelope.width should be(0.5)
        lineString2D.boundingEnvelope.height should be(0.5)
        lineString2D.boundingEnvelope.area should be(0.25)

        // for a polygon
        val poly = Polygon2D.fromPoints[EPSG_4326](Seq(point3, point, point2))
        poly.boundingEnvelope should be(
          Envelope2D[EPSG_4326](point2, Point2D[EPSG_4326](-121.0, 37.5)))
        poly.boundingEnvelope.centroid should be(Point2D[EPSG_4326](-121.25, 37.25))
        poly.boundingEnvelope.width should be(0.5)
        poly.boundingEnvelope.height should be(0.5)
        poly.boundingEnvelope.area should be(0.25)
      }

      it("should make column and row counts for resolution correctly") {
        val envelope = Envelope2D[EPSG_4326](Point2D(-121.0, 37.0), Point2D(-122.0, 38.0))
        envelope.makeColumnCount(0.1) should be(10)
        envelope.makeRowCount(0.05) should be(20)
      }
    }
  }
