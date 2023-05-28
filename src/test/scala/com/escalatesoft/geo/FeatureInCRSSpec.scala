package com.escalatesoft.geo

import com.escalatesoft.geo.crs.CRST.CRSDefinitions.EPSG_32615
import com.escalatesoft.geo.json.FeatureJsonCodec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Format
import play.api.libs.json.Json
import play.api.libs.json.JsValue
import java.io.File
import org.apache.commons.io.FileUtils
import java.nio.charset.StandardCharsets

class FeatureInCRSSpec extends AnyFunSpec with Matchers:
  import FeatureInCRSSpec.*
  import com.escalatesoft.geo.json.GeoJsonRawFormats.given
  import com.escalatesoft.geo.json.FeatureJsonCodec.given

  val justPolys = new File(getClass.getClassLoader.getResource("demodata/geometry/attributes_test.geojson").toURI)
  val featureCollectionJsonStr = FileUtils.readFileToString(justPolys, StandardCharsets.UTF_8)
  val featureCollectionJson = Json.parse(featureCollectionJsonStr)
  val collection: FeatureCollection[Geometry, JsValue] = featureCollectionJson.as[FeatureCollection[Geometry, JsValue]]

  describe("FeatureInCRS") {

    it("should allow attributes to be mapped without affecting the geom") {
      val ffCollection: FeatureCollection[Geometry, FamilyFarms] =
        collection.mapAttributes(_.as[FamilyFarms])

      val attrs1: FamilyFarms = ffCollection.features.head.attr

      attrs1.BUSINESSUNI should be("Farm Group")
      attrs1.FIELD should be("Field 1")
      attrs1.GROWER should be("ABC Farms")
      attrs1.FARM should be("Farm 1")
      attrs1.LOCATION should be("Smallsville")
      attrs1.FIELD_GUID should be("3d698dc2-5eab-4aeb-afde-bb7a674f6708")
      attrs1.VAR should be("Crop Production Services")
    

      val attrs2: FamilyFarms = ffCollection.features(1).attr

      attrs2.BUSINESSUNI should be("Farm Group")
      attrs2.FIELD should be("Field 2")
      attrs2.GROWER should be("ABC Farms")
      attrs2.FARM should be("Farm 2")
      attrs2.LOCATION should be("Smallsville")
      attrs2.FIELD_GUID should be("4d698dc2-5eab-4aeb-afde-bb7a674f6708")
      attrs2.VAR should be("Crop Production Services")

      for (i <- 0 to 1) {
        collection.features(i).geometry should be theSameInstanceAs ffCollection
          .features(i)
          .geometry
      }
    }

    it("should get, and allow update of id") {
      val id1 = collection.features(0).id
      val id2 = collection.features(1).id

      val f1 = collection.features(0).withId(Some("ID1"))
      val f2 = collection.features(1).withId(Some("ID2"))

      id1 should be("test_id")
      id2 should be("72ff2bbc-ac24-3ad6-b672-55010eba09a2")

      f1.id should be("ID1")
      f2.id should be("ID2")

      collection.features(0).id should be(id1)
      collection.features(1).id should be(id2)
    }

    it("should handle equality and hashcode properly") {
      val transformedUTM: FeatureCollectionInCRS[EPSG_32615, Geometry, JsValue] =
        collection.transformCRS[EPSG_32615]

      val transformedBack: FeatureCollection[Geometry, JsValue] =
        transformedUTM.normalize

      val alteredAttributes: FeatureCollection[Geometry, JsValue] =
        collection.mapAttributes(identity)

      for i <- 0 to 1 do
        transformedBack.features(i) == collection.features(i) should be(false)
        transformedBack.features(i).## should not be (collection.features(i).##)
        transformedBack.features(i).equalsWithinTolerance(collection.features(i)) should be(true)

        alteredAttributes.features(i) == collection.features(i) should be(true)
        alteredAttributes.features(i).## should be(collection.features(i).##)
    }

    it("should allow you to take some features from a collection") {
      val firstTwo: FeatureCollection[Geometry, JsValue] = collection.take(1)
      collection.length should be(2)
      firstTwo.features.length should be(1)
      firstTwo.features(0) should be(collection.features(0))

      // attempt to get the next item should fail
      intercept[IndexOutOfBoundsException] {
        firstTwo.features(1)
      }
    }

    it("should not create a new instance of the collection when normalized if the CRS is already 4326") {
      val transformedUTM: FeatureCollectionInCRS[EPSG_32615, Geometry, JsValue] =
        collection.transformCRS[EPSG_32615]

      val normalized1 = transformedUTM.normalize
      val normalized2 = normalized1.normalize

      normalized1 should be theSameInstanceAs (normalized2)
      normalized1 should not be theSameInstanceAs (transformedUTM)
    }

    it("should allow you to update the id") {
      collection.featureTypeId should be(None)
      val updated = collection.withFeatureTypeId(Some("new_id"))
      updated.featureTypeId should be(Some("new_id"))
      updated.features should be theSameInstanceAs (collection.features)
    }
  }


object FeatureInCRSSpec:
  case class FamilyFarms(
    BUSINESSUNI: String,
    FIELD: String,
    GROWER: String,
    FARM: String,
    LOCATION: String,
    FIELD_GUID: String,
    VAR: String
  )

  given Format[FamilyFarms] = Json.format[FamilyFarms]

