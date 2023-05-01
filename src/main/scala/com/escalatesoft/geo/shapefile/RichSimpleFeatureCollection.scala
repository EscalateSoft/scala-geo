package com.escalatesoft.geo.shapefile

import org.geotools.data.simple.SimpleFeatureCollection
import org.opengis.feature.simple.SimpleFeature

class RichSimpleFeatureCollection(val underlying: SimpleFeatureCollection) extends IterableOnce[SimpleFeature] {
  override def iterator: Iterator[SimpleFeature] = new Iterator[SimpleFeature] {
    val iter = underlying.features()
    override def hasNext: Boolean = iter.hasNext
    override def next(): SimpleFeature = iter.next()
  }
}

object ShapeFileConverters:
  given richSimpleFeatureCollection: Conversion[SimpleFeatureCollection, Seq[SimpleFeature]] =
    RichSimpleFeatureCollection(_).iterator.toSeq