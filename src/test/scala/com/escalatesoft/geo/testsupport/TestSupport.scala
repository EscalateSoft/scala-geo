package com.escalatesoft.geo.testsupport

import com.escalatesoft.geo.Feature
import com.escalatesoft.geo.Geometry
import com.escalatesoft.geo.shapefile.DateAttribute
import com.escalatesoft.geo.shapefile.DoubleAttribute
import com.escalatesoft.geo.shapefile.*

object TestSupport:

  def compareFeatures(
    feature1: Feature[Geometry, ShapeAttributes],
    feature2: Feature[Geometry, ShapeAttributes]
  ): Unit =

    import org.scalatest.matchers.should.Matchers.*

    feature1.geometry.equalsWithinTolerance(feature2.geometry) should be(true)

    val attributeMap1 = feature1.attr.attributes
    val attributeMap2 = feature2.attr.attributes

    attributeMap1 should have size (attributeMap2.size)

    for (name, value) <- attributeMap1 do
      val compareValue = attributeMap2(name)

      (value, compareValue) match
        case (DoubleAttribute(d), DoubleAttribute(d2)) => d should be(d2 +- 1e-12)
        case (DateAttribute(d1), DateAttribute(d2))    => d1.toLocalDate should be(d2.toLocalDate)
        case (v1, v2)                                  => v1 should be(v2)

