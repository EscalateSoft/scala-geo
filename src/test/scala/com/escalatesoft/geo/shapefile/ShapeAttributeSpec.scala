package com.escalatesoft.geo.shapefile

import com.escalatesoft.geo.shapefile.ShapeAttribute.JSONDateFormat
import org.scalacheck.Gen
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.JsNumber
import play.api.libs.json.JsObject
import play.api.libs.json.JsString

import java.time.Instant
import java.time.LocalDate
import java.time.ZoneId
import java.time.ZoneOffset
import java.time.ZonedDateTime
import java.time.temporal.ChronoField
import java.time.temporal.ChronoUnit
import java.util.Date
import java.util.TimeZone
import scala.util.Success
import scala.util.Try

class ShapeAttributeSpec extends AnyFunSpec with Matchers with ScalaCheckPropertyChecks:
  val arbitraryDateValue: Long = 1537376107348L

  val javaDateCompare: Date =
    val date = new Date(arbitraryDateValue)
    val zdt = ZonedDateTime.ofInstant(date.toInstant(), ZoneId.systemDefault())
    Date.from(zdt.toLocalDate.atStartOfDay.atZone(zdt.getZone).toInstant)

  val compareDate: LocalDate = ZonedDateTime
    .ofInstant(Instant.ofEpochMilli(arbitraryDateValue), TimeZone.getDefault.toZoneId)
    .toLocalDate

  describe("Shape file attributes") {
    they("should parse all supported types without error") {
      ShapeAttribute.parse(10) should be(Success(LongAttribute(10L)))
      ShapeAttribute.parse(10L) should be(Success(LongAttribute(10L)))
      ShapeAttribute.parse(-10: Short) should be(Success(LongAttribute(-10L)))
      ShapeAttribute.parse(1.25F) should be(Success(DoubleAttribute(1.25)))
      ShapeAttribute.parse(-1.25) should be(Success(DoubleAttribute(-1.25)))
      ShapeAttribute.parse("Hello world") should be(Success(StringAttribute("Hello world")))
      ShapeAttribute.parse("") should be(Success(StringAttribute("")))
      ShapeAttribute.parse(new java.util.Date(arbitraryDateValue)) should be(
        Success(DateAttribute(compareDate)))
      ShapeAttribute.parse(LocalDate.of(2018, 9, 19)) should be(Success(DateAttribute(compareDate)))
    }

    they("should round-trip back to shape file attributes") {
      ShapeAttribute.parse(10).get.toShapeField should be(10L)
      ShapeAttribute.parse(10L).get.toShapeField should be(10L)
      ShapeAttribute.parse(-10: Short).get.toShapeField should be(-10L)
      ShapeAttribute.parse(1.25F).get.toShapeField should be(1.25)
      ShapeAttribute.parse(-1.25).get.toShapeField should be(-1.25)
      ShapeAttribute.parse("Hello world").get.toShapeField should be("Hello world")
      ShapeAttribute.parse("").get.toShapeField should be("")
      ShapeAttribute.parse(new java.util.Date(arbitraryDateValue)).get.toShapeField should be(javaDateCompare)
      ShapeAttribute.parse(LocalDate.of(2018, 9, 19)).get.toShapeField should be(javaDateCompare)
    }

    they("should fail for unsupported types, particularly JSON") {
      intercept[IllegalStateException](ShapeAttribute.parse(-6: Byte).get)
      intercept[IllegalStateException](ShapeAttribute.parse(JsString("something")).get)
      intercept[IllegalStateException](ShapeAttribute.parse(JsObject(Map("something" -> JsNumber(10)))).get)
    }

    they("should work for all dates") {

      val dateGen: Gen[Date] = for {
        yearDelta <- Gen.choose(0, 20)
        dayOfYear <- Gen.choose(min = 1, max = 365)
      } yield {
        val localDate =
          LocalDate.ofYearDay(2000 + yearDelta, dayOfYear).atStartOfDay(ZoneId.systemDefault())
        Date.from(localDate.toInstant)
      }

      forAll(dateGen) { date =>
        val localDate = date.toInstant.atZone(ZoneId.systemDefault).toLocalDate
        val parsed1 = ShapeAttribute.parse(date)
        val parsed2 = ShapeAttribute.parse(localDate)
        parsed1.get.toShapeField should be(date)
        parsed2.get.toShapeField should be(date)

        parsed1.get.toTimeStamp.get.toLocalDate should be(localDate)
        parsed2.get.toTimeStamp.get.toLocalDate should be(localDate)
      }
    }

    they("should produce a valid timestamp from any date attribute representation") {
      val now = ZonedDateTime.now.withNano(0)
      val dateAttr = DateAttribute(now)
      val longAttr = ShapeAttribute.parse(now).get
      val doubleAttr = DoubleAttribute(now.toInstant().toEpochMilli().toDouble / 1000.0)
      val strAttr = StringAttribute(JSONDateFormat.format(Date.from(now.toInstant)))

      dateAttr.toTimeStamp.get should be(now)
      longAttr.toTimeStamp.get should be(now)
      doubleAttr.toTimeStamp.get should be(now)
      strAttr.toTimeStamp.get should be(now)
    }
  }

