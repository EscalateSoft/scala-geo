package com.escalatesoft.geo.crs

import com.escalatesoft.geo.Point2D
import com.escalatesoft.geo.crs.CRST.CRSDefinitions.*
import com.escalatesoft.geo.crs.CRST.CRSDefinitions.given
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CRSDefsSpec extends AnyFunSpec with Matchers with TypeCheckedTripleEquals:
  describe("CRS definitions") {
    it("should identify a WGS 84 UTM zone CRS correctly") {
      val p1 = Point2D[EPSG_32615](0, 0)
      p1.isWGS84UtmZoneCRS should ===(true)
      
      val p2 = Point2D[EPSG_32711](0, 0)
      p2.isWGS84UtmZoneCRS should ===(true)

      val p3 = Point2D[EPSG_4326](0, 0)
      p3.isWGS84UtmZoneCRS should ===(false)
    }

    it("should look up the right UTM zone ID from the zone number") {
      val zone15N = CRST.epsgUtmCodeForZoneNumber(15, northernHemisphere = true)
      val zone16S = CRST.epsgUtmCodeForZoneNumber(16, northernHemisphere = false)
      val noSuchZone1 = CRST.epsgUtmCodeForZoneNumber(0, northernHemisphere = true)
      val noSuchZone2 = CRST.epsgUtmCodeForZoneNumber(61, northernHemisphere = false)

      zone15N should ===(Some("EPSG:32615"))
      zone16S should ===(Some("EPSG:32716"))
      noSuchZone1 should ===(None)
      noSuchZone2 should ===(None)
    }

    it("should look up the right UTM zone ID from a zone identifier") {
      val zone15N = CRST.epsgUtmCodeForZoneNumber("15N")
      val zone16S = CRST.epsgUtmCodeForZoneNumber("16S")

      zone15N should ===("EPSG:32615")
      zone16S should ===("EPSG:32716")
    }

    it("should list all registered CRS definitions") {
      val allCRS = CRST.listKnownCRSs
      allCRS should contain("EPSG:4326")
      allCRS should contain("EPSG:32615")
      allCRS should contain("EPSG:32616")
      allCRS should contain("EPSG:32715")
      allCRS should contain("EPSG:32716")

      allCRS should have size(148)
    }

    it("should construct an existential CRS type from a runtime CRS") {
      val anyCrs = CRST.crsFromId(CRST.epsgUtmCodeForGZD("19T"))
      val anyCrs2 = CRST.crsFromRuntime(anyCrs.SOME_CRS)

      anyCrs.SOME_CRS.crsId should ===("EPSG:32619")
      anyCrs2.SOME_CRS.crsId should ===("EPSG:32619")

      anyCrs.SOME_CRS.crsDef should ===(anyCrs2.SOME_CRS.crsDef)
      anyCrs.SOME_CRS.crsDef should be theSameInstanceAs(anyCrs2.SOME_CRS.crsDef)
    }
  }