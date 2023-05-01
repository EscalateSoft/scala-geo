package com.escalatesoft.geo.shapefile

import java.io.File
import java.time.{LocalDate, ZonedDateTime}
import java.util.UUID

import com.escalatesoft.geo.*
import org.apache.commons.io.FileUtils
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.util.Try
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.JsValue
import play.api.libs.json.Reads
import play.api.libs.json.Writes
import play.api.libs.json.Json

class ShapeSerializationSpec extends AnyFunSpec with Matchers with ScalaCheckPropertyChecks:

  case class AllTypesDTO(
    str: String,
    long: Long,
    double: Double,
    date: LocalDate,
    zdt: ZonedDateTime,
  )

  given Reads[AllTypesDTO] = Json.reads[AllTypesDTO]
  given Writes[AllTypesDTO] = Json.writes[AllTypesDTO]

  extension (zdt: ZonedDateTime)
    def utcTime = zdt.withZoneSameInstant(java.time.ZoneOffset.UTC)

  describe("Serializing and deserializing features to shape files") {
    it("should read a known shape file and produce expected values") {
      val knownShapeFile =
        new File(
          getClass.getClassLoader.getResource("demodata/geometry/YieldDuplicates/YieldDuplicates.shp").toURI)
      val features = ShapeFileSerialization.readFeatures(knownShapeFile)

      features.features should have size 40
      val feature1 = features.features.head

      val featuresMap = feature1.attr.attributes

      val compareMap = Map(
        "yKgPerHDry" -> DoubleAttribute(1083.8434116982774),
        "kgDry" -> DoubleAttribute(0.605317148074027),
        "MOIST" -> DoubleAttribute(16.5),
        "AREA" -> DoubleAttribute(0.00138006),
        "SPEED" -> DoubleAttribute(0.61),
        "YIELD" -> DoubleAttribute(1144.36),
        "FLOW" -> DoubleAttribute(1.59819996),
        "yBuPerADry" -> DoubleAttribute(17.26758472457719),
        "yKgPerHStd" -> DoubleAttribute(1282.6542027345758),
        "DIST" -> DoubleAttribute(61.1),
        "LON" -> DoubleAttribute(-92.50912476),
        "DURATION" -> DoubleAttribute(1.0),
        "ELEV" -> DoubleAttribute(306.94),
        "LAT" -> DoubleAttribute(43.01726151),
        "TIME" -> DoubleAttribute(1.223646909E9),
        "yBuPerAStd" -> DoubleAttribute(20.435)
      )
      featuresMap.view.filterKeys(_ != "the_geom").toMap should be(compareMap)

      // special check for the date handling
      val theDate = featuresMap("TIME").toTimeStamp.get

      theDate.utcTime.toString should be("2008-10-10T13:55:09Z")
    }

    it("should map the attributes to/from JSON when requested") {
      val knownShapeFile =
        new File(
          getClass.getClassLoader.getResource("demodata/geometry/YieldDuplicates/YieldDuplicates.shp").toURI)

      val justFeature: Feature[Geometry, ShapeAttributes] =
        ShapeFileSerialization.readFeature(knownShapeFile)

      val jsonFeature: Feature[Geometry, JsValue] =
        justFeature.mapAttribute(attr => attr.asJson)

      // and back again
      val shapeAgain = jsonFeature.mapAttribute(attr => attr.as[ShapeAttributes])

      justFeature.geometry.equalsWithinTolerance(shapeAgain.geometry) should be(true)

      for (k <- justFeature.attr.attributes.keySet) do
        val v1 = justFeature.attr.attributes(k).value
        val v2 = shapeAgain.attr.attributes(k).value

        v1 should be(v2)
    }
 
    it("should write back out a feature collection correctly") {
      val knownShapeFile =
        new File(
          getClass.getClassLoader.getResource("demodata/geometry/YieldDuplicates/YieldDuplicates.shp").toURI)
      val featureCollection = ShapeFileSerialization.readFeatures(knownShapeFile)

      featureCollection.features should have size 40
      featureCollection.collectGeometries[Puntal].length should be(40)

      val testFile = new File("target/export/yielddups.shp")
      Try(testFile.getParentFile.mkdirs())
      ShapeFileSerialization.writeFeatureCollectionToFile(testFile, featureCollection)

    }

    it("should read in and write out a polygon shape file without changes") {
      val polyShapeFile = new File(
        getClass.getClassLoader
          .getResource("demodata/geometry/Simplification/Simplification.shp")
          .toURI)

      val featureCollection = ShapeFileSerialization.readFeatures(polyShapeFile)

      featureCollection.features should have size 1
      featureCollection.collectGeometries[Polygonal].length should be(1)

      val testFile = new File("target/export/export.shp")
      Try(testFile.getParentFile.mkdirs())
      ShapeFileSerialization.writeFeatureCollectionToFile(testFile, featureCollection)

      featureCollection.features.head.geometry match
        case p: Polygonal[_] =>
          val allPolys = p.asMultiPolygon.componentGeometries
          allPolys should have size 1
          allPolys.head.getExteriorRing.size should be(215)
          allPolys.head.getInteriorRings.head.size should be(25)
        case _ => fail("Expected a polygonal geometry")
    }

    /*it("should result in the same feature after a round trip") {

      val tempDir = new File(new File("target/export"), UUID.randomUUID().toString)
      Try(tempDir.mkdirs())
      try
        forAll(normalizedFeatureGeneratorIowa4326) { feature =>
          val file = new File(tempDir, feature.id + ".shp")
          ShapeFileSerialization.writeFeatureToFile(file, feature)
          val readFeature = ShapeFileSerialization.readFeature(file)

          TestSupport.compareFeatures(feature, readFeature)
        }
      finally FileUtils.deleteDirectory(tempDir)
    }*/

    it("should serialize rich attributes to/from ShapeAttributes") {
      val richAttr =
        AllTypesDTO("hello", 10L, 1234.5, LocalDate.now(), ZonedDateTime.now().withNano(0))
      val asJson = Json.toJson(richAttr)
      println(Json.prettyPrint(asJson))
      val asShape = asJson.as[ShapeAttributes]
      val backToJson = Json.toJson(asShape)
      val backToDTO = backToJson.as[AllTypesDTO]
      backToDTO should be(richAttr)
    }

    /*it("should result in the same attributed feature after a round trip") {

      val tempDir = new File(new File("target/export"), UUID.randomUUID().toString)
      Try(tempDir.mkdirs())
      try {
        forAll(attributedFeatureGeneratorIowa4326) { feature =>
          val file = new File(tempDir, feature.id + ".shp")
          ShapeFileSerialization.writeAttributedFeatureToFile(file, feature)
          val readFeature = ShapeFileSerialization.readAttributedFeature[AllTypesDTO](file)

          val outFeatures = feature.attr
          val inFeatures = readFeature.get.attr

          outFeatures.date should be(inFeatures.date)
          outFeatures.zdt.zuluTime should be(inFeatures.zdt.zuluTime)
          outFeatures.zdt.defaultZone should be(inFeatures.zdt.defaultZone)
          outFeatures.str should be(inFeatures.str)
          outFeatures.num should be(inFeatures.num)
          outFeatures.real should be(inFeatures.real +- 1e-12)
        }
      } finally FileUtils.deleteDirectory(tempDir)
    }*/

  }

