package com.escalatesoft.geo.shapefile

import play.api.libs.json.Reads
import play.api.libs.json.Json
import play.api.libs.json.Writes
import play.api.libs.json.JsValue

/**
  * Case class type that represents a sequence of permissable shape file attributes
  * @param attributes
  */
case class ShapeAttributes(attributes: Map[String, ShapeAttribute]):
  def asJson = Json.toJson(this)

/**
  * JSON Reads and Writes for ShapeAttributes
  *
  * Note, by using these codecs, and using JSON as an intermediate representation,
  * JSON can also be used to parse and encode attributes in features to shape files.
  * Of course it can fail since shape file attributes are limited, but it will
  * fail very early in both directions if that is the case.
  */
object ShapeAttributes:
  given Reads[ShapeAttributes] = { (json: JsValue) =>
    json.validate[Map[String, ShapeAttribute]].map(ShapeAttributes(_))
  }
  given Writes[ShapeAttributes] = { (shapeAttributes: ShapeAttributes) =>
    Json.toJson(shapeAttributes.attributes)
  }
end ShapeAttributes