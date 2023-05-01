package com.escalatesoft.geo.shapefile

import java.time.{Instant, LocalDate, ZoneId, ZonedDateTime}
import scala.util.{Failure, Success, Try}
import play.api.libs.json.Writes
import play.api.libs.json.JsNumber
import play.api.libs.json.JsString
import play.api.libs.json.Reads
import play.api.libs.json.JsSuccess
import play.api.libs.json.JsError

/**
  * A shape attribute ADT, there are really only 4 base attribute
  * types for shape files, and 5 data representations that we care
  * about right now. These are:
  *
  * String, represented as a StringAttribute
  *
  * Long, represented as a LongAttribute
  *
  * Double, represented as a DoubleAttribute
  *
  * Date (no time) represented as a DateAttribute
  *
  * TimeStamp represented and parsed out of either a LongAttribute or a
  * StringAttribute of the correct ISO date format.
  */
sealed trait ShapeAttribute extends Product with Serializable {
  def toShapeField: Any
  def toTimeStamp: Try[ZonedDateTime]
  private[geo] def value: Any
}

case class StringAttribute(value: String) extends ShapeAttribute:
  import ShapeAttribute.JSONDateFormat
  override def toShapeField: Any = value
  override def toTimeStamp: Try[ZonedDateTime] = Try {
    val date = JSONDateFormat.parse(value)
    ZonedDateTime.ofInstant(date.toInstant, ZoneId.systemDefault)
  }

case class LongAttribute(value: Long) extends ShapeAttribute:
  override def toShapeField: Any = value
  override def toTimeStamp: Try[ZonedDateTime] =
    Success(ZonedDateTime.ofInstant(Instant.ofEpochMilli(value * 1000), ZoneId.systemDefault))

object LongAttribute:
  def fromZonedDateTime(zdt: ZonedDateTime): LongAttribute = LongAttribute(zdt.toEpochSecond)

case class DoubleAttribute(value: Double) extends ShapeAttribute:
  override def toShapeField: Any = value
  override def toTimeStamp: Try[ZonedDateTime] =
    Success(ZonedDateTime.ofInstant(Instant.ofEpochSecond(value.toLong), ZoneId.systemDefault))

case class DateAttribute(value: ZonedDateTime) extends ShapeAttribute:
  override def toShapeField: Any =
    new java.util.Date(java.sql.Date.valueOf(value.toLocalDate).getTime)
  override def toTimeStamp: Try[ZonedDateTime] = Success(value)

object DateAttribute:
  def apply(d: java.util.Date): DateAttribute =
    DateAttribute(new java.sql.Date(d.getTime).toLocalDate)
  
  def apply(ld: LocalDate): DateAttribute =
    DateAttribute(ld.atStartOfDay(ZoneId.systemDefault))
  


object ShapeAttribute:
  def parse(x: Any): Try[ShapeAttribute] = x match
    case s: Short          => Success(LongAttribute(s))
    case i: Int            => Success(LongAttribute(i))
    case l: Long           => Success(LongAttribute(l))
    case f: Float          => Success(DoubleAttribute(f))
    case d: Double         => Success(DoubleAttribute(d))
    case d: java.util.Date => Success(DateAttribute(d))
    case d: LocalDate      => Success(DateAttribute(d))
    case z: ZonedDateTime  => Success(LongAttribute.fromZonedDateTime(z))
    case s: String         => Success(StringAttribute(s))
    case _                 => failShapeParse(x)

  private def failShapeParse(x: Any) = {
    Failure(
      new IllegalStateException(
        s"Could not parse $x of class ${x.getClass} to Shape File Attribute"))
  }

  // parse a datetime string like (2017-01-01 10:11:12.456) into a ZonedDateTime
  val JSONDateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")

  given Writes[ShapeAttribute] = {
    case LongAttribute(v)   => JsNumber(v)
    case DoubleAttribute(v) => JsNumber(v)
    case StringAttribute(s) => JsString(s)
    case DateAttribute(d)   => JsString(JSONDateFormat.format(d))
  }

  given Reads[ShapeAttribute] = {
    case JsNumber(v) if v.isValidLong => ShapeAttribute.parse(v.toLong).map(JsSuccess(_)).getOrElse(failParse(v.toString))
    case JsNumber(v) => ShapeAttribute.parse(v.toDouble).map(JsSuccess(_)).getOrElse(failParse(v.toString))
    case JsString(s) => ShapeAttribute.parse(s).map(JsSuccess(_)).getOrElse(failParse(s))
    case other => JsError(s"Could not parse ShapeAttribute $other")
  }

  private def failParse(msg: String): JsError = JsError(s"Could not parse ShapeAttribute $msg")

    //   private def maybeDate(s: String): ShapeAttribute =
    //     Try {
    //       val zdt = ZonedDateTime.from(JSONDateFormat.parse(s).toInstant)
    //       val atStartOfDay = zdt.truncatedTo(java.time.temporal.ChronoUnit.DAYS)
    //       if (atStartOfDay == zdt) DateAttribute(zdt) else LongAttribute.fromZonedDateTime(zdt)
    //     }.getOrElse(StringAttribute(s))
    // }
end ShapeAttribute