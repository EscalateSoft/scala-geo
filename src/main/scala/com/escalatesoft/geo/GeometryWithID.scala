package com.escalatesoft.geo

trait GeometryWithID[CRS] { this: Geometry[CRS] =>
  val geometryId: String
}
