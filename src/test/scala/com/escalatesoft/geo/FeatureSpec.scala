package com.escalatesoft.geo

import com.escalatesoft.geo.crs.CRST.CRSDefinitions.{EPSG_32615, EPSG_4326}

import scala.util.Try
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class FeatureSpec extends AnyFunSpec with Matchers:

  case class Flow(flow: Double)
  case class Duration(duration: Double)
  case class Yield(yld: Double)

  trait Fruit
  case class Banana(name: String) extends Fruit
  case class Apple(name: String) extends Fruit

  describe("Creating a typed feature") {
    describe("with 4326") {
      it("should have identical geometry") {
        val rawPoints = Seq(
          RawCoordinate2D(-92.2493031319753, 42.672304270958215),
          RawCoordinate2D(-92.24929402927881, 42.67243292753743),
          RawCoordinate2D(-92.24714959503662, 42.67242300503766)
        )

        val polygon2d = Polygon2D[EPSG_4326](rawPoints)

        val tf: Feature[Polygon2D, Apple] =
          Feature(polygon2d, Apple("Granny Smith"))

        tf.geometry.toWKT should be(polygon2d.toWKT)
      }

      it("should allow a feature to have the type narrowed") {
        val rawPoints = Seq(
          RawCoordinate2D(-92.2493031319753, 42.672304270958215),
          RawCoordinate2D(-92.24929402927881, 42.67243292753743),
          RawCoordinate2D(-92.24714959503662, 42.67242300503766)
        )

        val polygon2d = Polygon2D[EPSG_4326](rawPoints)

        val tf: Feature[Geometry, Apple] =
          Feature(polygon2d, Apple("Granny Smith"))

        // val nf1: Try[Feature[Polygonal, Apple]] = tf.narrowGeometry[Polygonal]
        // nf1.isSuccess should be(true)

        // val nf2: Try[Feature[Polygon2D, Apple]] = tf.narrowGeometry[Polygon2D]
        // nf2.isSuccess should be(true)

        // val nf3: Try[Feature[Point2D, Apple]] = tf.narrowGeometry[Point2D]
        // nf3.isFailure should be(true)
        // nf3.toEither.left.get.getMessage should be(
        //   "Feature Polygon2D could not be narrowed to Point2D"
        // )
      }

      it("should map over a geometry, changing the type as necessary") {
        val rawPoints = Seq(
          RawCoordinate2D(-92.2493031319753, 42.672304270958215),
          RawCoordinate2D(-92.24929402927881, 42.67243292753743),
          RawCoordinate2D(-92.24714959503662, 42.67242300503766)
        )

        val polygon2d = Polygon2D[EPSG_4326](rawPoints)

        val tf: Feature[Polygon2D, Apple] =
          Feature(polygon2d, Apple("Granny Smith"), Some("test id"))

        val pf = tf.mapGeometry(_.centroid, retainId = true)
        pf.attr should be(Apple("Granny Smith"))
        pf.id should be("test id")
        pf.geometry.x should be(-92.24858225209691)
        pf.geometry.y should be(42.6723867345111)

        val pf2 = tf.mapGeometry(_.centroid, retainId = false)
        pf2.attr should be(Apple("Granny Smith"))
        pf2.id should not be ("test id")
        pf2.geometry.x should be(-92.24858225209691)
        pf2.geometry.y should be(42.6723867345111)

      }
    }

    describe("with 32615") {
      it("should have geometry converted to 4326") {

        val rawPoints32615 = Seq(
          RawCoordinate2D(561512.3702547651, 4724698.932194901),
          RawCoordinate2D(561512.9892547653, 4724713.2251949),
          RawCoordinate2D(561688.7152547662, 4724713.686194)
        )

        val polygon2D = Polygon2D[EPSG_32615](rawPoints32615)

        val tf: Feature[Polygon2D, Banana] = Feature(polygon2D, Banana("Fife"))

        tf.geometry.toWKT should be(polygon2D.transformCRS[EPSG_4326].toWKT)
      }

      it("should allow us to get 32615 back out on request") {
        val rawPoints32615 = Seq(
          RawCoordinate2D(561512.3702547651, 4724698.932194901),
          RawCoordinate2D(561512.9892547653, 4724713.2251949),
          RawCoordinate2D(561688.7152547662, 4724713.686194)
        )

        val polygon2D = Polygon2D[EPSG_32615](rawPoints32615)

        val tf: Feature[Polygon2D, Banana] = Feature(polygon2D, Banana("Fife"))

        val pointsBack = tf.geometryInCRS[EPSG_32615].getExteriorRing

        pointsBack(0).x should be(561512.3702547651 +- 1e-6)
        pointsBack(0).y should be(4724698.932194901 +- 1e-6)
        pointsBack(1).x should be(561512.9892547653 +- 1e-6)
        pointsBack(1).y should be(4724713.2251949 +- 1e-6)
        pointsBack(2).x should be(561688.7152547662 +- 1e-6)
        pointsBack(2).y should be(4724713.6861948995 +- 1e-6)
        pointsBack(3).x should be(561512.3702547651 +- 1e-6)
        pointsBack(3).y should be(4724698.932194901 +- 1e-6)
      }

      it("should return stable 32615 results if called repeatedly") {
        val rawPoints32615 = Seq(
          RawCoordinate2D(561512.3702547651, 4724698.932194901),
          RawCoordinate2D(561512.9892547653, 4724713.2251949),
          RawCoordinate2D(561688.7152547662, 4724713.686194)
        )

        val polygon2D = Polygon2D[EPSG_32615](rawPoints32615)

        val tf: Feature[Polygon2D, Banana] = Feature(polygon2D, Banana("Fife"))

        val compare = tf.geometryInCRS[EPSG_32615].toWKT

        for _ <- 1 to 10 do
          tf.geometryInCRS[EPSG_32615].toWKT should be(compare)
      }

      it("should be covariant for geometries") {
        val rawPoints32615 = Seq(
          RawCoordinate2D(561512.3702547651, 4724698.932194901),
          RawCoordinate2D(561512.9892547653, 4724713.2251949),
          RawCoordinate2D(561688.7152547662, 4724713.686194)
        )

        val polygon2D = Polygon2D[EPSG_32615](rawPoints32615)

        """val tf2: TypedFeature[LineString2D, Banana] = TypedFeature(polygon2D, Banana("Fife"))""" shouldNot typeCheck

        val tf: Feature[Geometry, Banana] = Feature(polygon2D, Banana("Fife"))
        tf should be(Feature(polygon2D, Banana("Fife")))

      }

      it("should allow mapping over the attr without affecting geom") {
        val rawPoints32615 = Seq(
          RawCoordinate2D(561512.3702547651, 4724698.932194901),
          RawCoordinate2D(561512.9892547653, 4724713.2251949),
          RawCoordinate2D(561688.7152547662, 4724713.686194)
        )

        val polygon2D = Polygon2D[EPSG_32615](rawPoints32615)

        val tf: Feature[Polygon2D, Banana] =
          Feature(polygon2D, Banana("Fife"))

        val af: Feature[Polygon2D, Apple] =
          tf.mapAttribute(banana => Apple(banana.name.reverse))

        af.geometry should be theSameInstanceAs (tf.geometry)

        af.attr should be(Apple("efiF"))
        tf.attr should be(Banana("Fife"))
      }

      it("should work with Tuples as attributes") {
        val attrs = Flow(22.7) *: Duration(10.0) *: EmptyTuple

        val rawPoints32615 = Seq(
          RawCoordinate2D(561512.3702547651, 4724698.932194901),
          RawCoordinate2D(561512.9892547653, 4724713.2251949),
          RawCoordinate2D(561688.7152547662, 4724713.686194)
        )

        val polygon2D = Polygon2D[EPSG_32615](rawPoints32615)

        val tf: Feature[Polygon2D, (Flow, Duration)] =
          Feature(polygon2D, attrs)

        def calcYield(flow: Flow, duration: Duration): Yield =
          Yield(flow.flow * duration.duration)

        // val nf: Feature[Polygon2D, (Yield, Flow, Duration)] =
        //   tf.calcAttr2(calcYield)
      }
    }
  }
