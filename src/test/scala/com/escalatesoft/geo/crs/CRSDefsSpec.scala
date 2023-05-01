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
  }