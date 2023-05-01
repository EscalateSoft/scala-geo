package com.escalatesoft.geo.json

import play.api.libs.json.*
import play.api.libs.json.Reads.*
import play.api.libs.functional.syntax.*
import org.locationtech.jts.geom.{Geometry => JTSGeometry}
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Point
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.geom.MultiPoint
import org.locationtech.jts.geom.MultiLineString
import org.locationtech.jts.geom.MultiPolygon
import com.escalatesoft.geo.MultiPolygon2D
import com.escalatesoft.geo.crs.CRST.CRSDefinitions.EPSG_4326
import com.escalatesoft.geo.Geometry
import com.escalatesoft.geo.MultiLineString2D
import com.escalatesoft.geo.MultiPoint2D
import com.escalatesoft.geo.Polygon2D
import com.escalatesoft.geo.LineString2D
import com.escalatesoft.geo.Point2D
import com.escalatesoft.geo.RawCoordinate2D

sealed trait RawGeoJson

sealed trait GeometryJson extends RawGeoJson:
  lazy val geometryFactory = new GeometryFactory()
  def toJTS: JTSGeometry = toJTSGF(geometryFactory)
  protected def toJTSGF(gf: GeometryFactory): JTSGeometry
  def toGeometry: Geometry[EPSG_4326]

object GeometryJson:
  def fromGeometry(g: Geometry[EPSG_4326]): GeometryJson = g match
    case p: Point2D[?] => Point2DJson.fromPoint2D(p)
    case ls: LineString2D[?] => LineString2DJson.fromLineString2D(ls)
    case pl: Polygon2D[?] => Polygon2DJson.fromPolygon2D(pl)
    case mp: MultiPoint2D[?] => MultiPoint2DJson.fromMultiPoint2D(mp)
    case mls: MultiLineString2D[?] => MultiLineString2DJson.fromMultiLineString2D(mls)
    case mpl: MultiPolygon2D[?] => MultiPolygon2DJson.fromMultiPolygon2D(mpl)
  

case class Point2DJson(coordinates: Seq[Double]) extends GeometryJson:
  override protected def toJTSGF(gf: GeometryFactory): JTSGeometry =
    gf.createPoint(new Coordinate(coordinates(0), coordinates(1)))
  def toPoint2D: Point2D[EPSG_4326] = Point2D(coordinates(0), coordinates(1))
  override def toGeometry: Point2D[EPSG_4326] = toPoint2D
object Point2DJson:
  def apply(x: Double, y: Double): Point2DJson = Point2DJson(Seq(x, y))
  def fromJTS(jts: Point): Point2DJson = Point2DJson(Array(jts.getX, jts.getY).toSeq)
  def fromPoint2D(p: Point2D[EPSG_4326]): Point2DJson = fromJTS(p.asJTS)
end Point2DJson

case class LineString2DJson(coordinates: Seq[Seq[Double]]) extends GeometryJson:
  override protected def toJTSGF(gf: GeometryFactory): LineString =
    val coords = coordinates.map { case Seq(x, y) => new Coordinate(x, y) }.toArray
    gf.createLineString(coords)
  def toLineString2D: LineString2D[EPSG_4326] = LineString2D(toJTSGF(geometryFactory))
  override def toGeometry: LineString2D[EPSG_4326] = toLineString2D
object LineString2DJson:
  def fromJTS(jts: LineString): LineString2DJson =
    LineString2DJson(jts.getCoordinates.toIndexedSeq.map(c => Array(c.getX, c.getY).toSeq))
  def fromLineString2D(ls: LineString2D[EPSG_4326]): LineString2DJson =
    fromJTS(ls.asJTS)
end LineString2DJson

case class Polygon2DJson(coordinates: Seq[Seq[Seq[Double]]]) extends GeometryJson:
  override protected def toJTSGF(gf: GeometryFactory): Polygon =
    val shell = coordinates.head.map { case Seq(x, y) => new Coordinate(x, y) }.toArray
    val holes = coordinates.tail.map { hole =>
      hole.map { case Seq(x, y) => new Coordinate(x, y) }.toArray
    }.toArray
    gf.createPolygon(gf.createLinearRing(shell), holes.map(gf.createLinearRing))
  def toPolygon2D: Polygon2D[EPSG_4326] = Polygon2D(toJTSGF(geometryFactory))
  override def toGeometry: Polygon2D[EPSG_4326] = toPolygon2D
object Polygon2DJson:
  def fromJTS(jts: Polygon): Polygon2DJson =
    val interiorRings:Seq[Seq[Seq[Double]]] = for (i <- 0 until jts.getNumInteriorRing)
      yield jts.getInteriorRingN(i).getCoordinates.toIndexedSeq.map(c => Array(c.getX, c.getY).toSeq)
    Polygon2DJson(jts.getCoordinates.map(c => Array(c.getX, c.getY).toSeq).toSeq +: interiorRings.toSeq)
  def fromPolygon2D(poly: Polygon2D[EPSG_4326]): Polygon2DJson =
    fromJTS(poly.asJTS)
end Polygon2DJson

case class MultiPoint2DJson(coordinates: Seq[Seq[Double]]) extends GeometryJson:
  override protected def toJTSGF(gf: GeometryFactory): MultiPoint =
    val coords = coordinates.map { case Seq(x, y) => new Coordinate(x, y) }.toArray
    gf.createMultiPointFromCoords(coords)
  def toMultiPoint2D: MultiPoint2D[EPSG_4326] =
    val coords = coordinates.map { case Seq(x, y) => RawCoordinate2D(x, y) }
    MultiPoint2D[EPSG_4326](coords)
  override def toGeometry: MultiPoint2D[EPSG_4326] = toMultiPoint2D
    
object MultiPoint2DJson:
  def fromJTS(jts: MultiPoint): MultiPoint2DJson =
    MultiPoint2DJson(jts.getCoordinates.map(c => Array(c.getX, c.getY).toSeq).toSeq)
  def fromMultiPoint2D(mp: MultiPoint2D[EPSG_4326]): MultiPoint2DJson =
    fromJTS(mp.asJTS)
end MultiPoint2DJson

case class MultiLineString2DJson(coordinates: Seq[Seq[Seq[Double]]]) extends GeometryJson:
  override protected def toJTSGF(gf: GeometryFactory): MultiLineString =
    val lines = coordinates.map { line =>
      val coords = line.map { case Seq(x, y) => new Coordinate(x, y) }.toArray
      gf.createLineString(coords)
    }.toArray
    gf.createMultiLineString(lines)
  def toMultiLineString2D: MultiLineString2D[EPSG_4326] =
    MultiLineString2D.fromJTS[EPSG_4326](toJTSGF(geometryFactory))
  override def toGeometry: MultiLineString2D[EPSG_4326] = toMultiLineString2D
object MultiLineString2DJson:
  def fromJTS(jts: MultiLineString): MultiLineString2DJson =
    val lines: Seq[Seq[Seq[Double]]] = for (i <- 0 until jts.getNumGeometries)
      yield jts.getGeometryN(i).getCoordinates.toIndexedSeq.map(c => Array(c.getX, c.getY).toSeq)
    MultiLineString2DJson(lines.toSeq)
  def fromMultiLineString2D(mls: MultiLineString2D[EPSG_4326]): MultiLineString2DJson =
    fromJTS(mls.asJTS)
end MultiLineString2DJson

case class MultiPolygon2DJson(coordinates: Seq[Seq[Seq[Seq[Double]]]]) extends GeometryJson:
  override protected def toJTSGF(gf: GeometryFactory): MultiPolygon =
    val polys = coordinates.map { poly =>
      val shell = poly.head.map { case Seq(x, y) => new Coordinate(x, y) }.toArray
      val holes = poly.tail.map { hole =>
        hole.map { case Seq(x, y) => new Coordinate(x, y) }.toArray
      }.toArray
      gf.createPolygon(gf.createLinearRing(shell), holes.map(gf.createLinearRing))
    }.toArray
    gf.createMultiPolygon(polys)
  def toMultiPolygon2D: MultiPolygon2D[EPSG_4326] =
    MultiPolygon2D.fromJTS[EPSG_4326](toJTSGF(geometryFactory))
  override def toGeometry: MultiPolygon2D[EPSG_4326] = toMultiPolygon2D
object MultiPolygon2DJson:
  def fromJTS(jts: MultiPolygon): MultiPolygon2DJson =
    val polys: Seq[Seq[Seq[Seq[Double]]]] = for (i <- 0 until jts.getNumGeometries)
      yield Polygon2DJson.fromJTS(jts.getGeometryN(i).asInstanceOf[Polygon]).coordinates
    MultiPolygon2DJson(polys.toSeq)
  def fromMultiPolygon2D(mp: MultiPolygon2D[EPSG_4326]): MultiPolygon2DJson =
    fromJTS(mp.asJTS)
end MultiPolygon2DJson

case class GeometryCollection2DJson(geometries: Seq[GeometryJson]) extends RawGeoJson

case class FeatureJson(
  id: Option[String],
  geometry: Option[GeometryJson],
  properties: Option[JsObject],
  `type`: String = "Feature",
) extends RawGeoJson

case class FeatureCollectionJson(
  features: Seq[FeatureJson],
  `type`: String = "FeatureCollection",
) extends RawGeoJson

object GeoJsonRawFormats:
  given Reads[Seq[Double]] = Reads[Seq[Double]] { jsValue =>
    jsValue.validate[Array[Double]].map(pts => IArray.from(pts))
  }

  given Reads[Point2DJson] = Json.reads[Point2DJson]
  given Reads[LineString2DJson] = Json.reads[LineString2DJson]
  given Reads[Polygon2DJson] = Json.reads[Polygon2DJson]
  given Reads[MultiPoint2DJson] = Json.reads[MultiPoint2DJson]
  given Reads[MultiLineString2DJson] = Json.reads[MultiLineString2DJson]
  given Reads[MultiPolygon2DJson] = Json.reads[MultiPolygon2DJson]
  given Reads[GeometryCollection2DJson] = Json.reads[GeometryCollection2DJson]
  
  given Reads[GeometryJson] = Reads[GeometryJson] { jsValue =>
    (jsValue \ "type") match
      case JsDefined(JsString("Point")) => Json.fromJson[Point2DJson](jsValue)
      case JsDefined(JsString("LineString")) => Json.fromJson[LineString2DJson](jsValue)
      case JsDefined(JsString("Polygon")) => Json.fromJson[Polygon2DJson](jsValue)
      case JsDefined(JsString("MultiPoint")) => Json.fromJson[MultiPoint2DJson](jsValue)
      case JsDefined(JsString("MultiLineString")) => Json.fromJson[MultiLineString2DJson](jsValue)
      case JsDefined(JsString("MultiPolygon")) => Json.fromJson[MultiPolygon2DJson](jsValue)
      case other => JsError(s"Unrecognized or missing type field in GeoJSON: $other")
  }

  given Reads[FeatureJson] = Json.reads[FeatureJson]
  given Reads[FeatureCollectionJson] = Json.reads[FeatureCollectionJson]

  given Writes[Point2DJson] = Json.writes[Point2DJson]
  given Writes[LineString2DJson] = Json.writes[LineString2DJson]
  given Writes[Polygon2DJson] = Json.writes[Polygon2DJson]
  given Writes[MultiPoint2DJson] = Json.writes[MultiPoint2DJson]
  given Writes[MultiLineString2DJson] = Json.writes[MultiLineString2DJson]
  given Writes[MultiPolygon2DJson] = Json.writes[MultiPolygon2DJson]

  given Writes[GeometryJson] = Writes[GeometryJson] { geom =>
    val geomJson = geom match
      case point: Point2DJson => Json.toJson(point)
      case lineString: LineString2DJson => Json.toJson(lineString)
      case polygon: Polygon2DJson => Json.toJson(polygon)
      case multiPoint: MultiPoint2DJson => Json.toJson(multiPoint)
      case multiLineString: MultiLineString2DJson => Json.toJson(multiLineString)
      case multiPolygon: MultiPolygon2DJson => Json.toJson(multiPolygon)

    geomJson.as[JsObject] + ("type" -> Json.toJson(geom.getClass.getSimpleName.replace("2DJson", "")))
  }

  given Writes[FeatureJson] = Json.writes[FeatureJson]
  given fcJson: Writes[FeatureCollectionJson] = Json.writes[FeatureCollectionJson]

end GeoJsonRawFormats