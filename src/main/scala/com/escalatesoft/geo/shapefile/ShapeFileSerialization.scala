package com.escalatesoft.geo.shapefile

import com.escalatesoft.geo.FeatureCollectionInCRS
import com.escalatesoft.geo.Geometry
import com.escalatesoft.geo._
import com.escalatesoft.geo.crs.CRST
import com.escalatesoft.geo.crs.CRST.CRSDefinitions.EPSG_4326
import com.typesafe.scalalogging.LazyLogging
import org.geotools.data.DataStore
import org.geotools.data.DataStoreFinder
import org.geotools.data.DefaultTransaction
import play.api.libs.json.Json
import play.api.libs.json.Reads
import play.api.libs.json.Writes

import java.io.File
import java.net.URL
import scala.jdk.CollectionConverters.*
import scala.util.Try

object ShapeFileSerialization extends LazyLogging:
  import ShapeFileSupport._
  import ShapeAttributes.given

  class WithDataStore(dataStore: DataStore):
    def apply[T](fn: DataStore => T): T =
      try
        fn(dataStore)
      finally
        dataStore.dispose()
  end WithDataStore

  def readFeature(file: File): Feature[Geometry, ShapeAttributes] =
    readFeatureInCRS[EPSG_4326](file)

  def readFeature(url: URL): Feature[Geometry, ShapeAttributes] =
    readFeatureInCRS[EPSG_4326](url)

  def readFeatureInCRS[CRS: CRST](
      file: File): FeatureInCRS[CRS, Geometry, ShapeAttributes] = {
    readFeatureInCRS(file.toURI.toURL)
  }

  def readAttributedFeature[ATTR: Reads](file: File): Try[Feature[Geometry, ATTR]] =
    readAttributedFeature[ATTR](file.toURI.toURL)

  def readAttributedFeature[ATTR: Reads](url: URL): Try[Feature[Geometry, ATTR]] =
    Try {
      val rawFeature = readFeatureInCRS[EPSG_4326](url)
      Try(rawFeature.mapAttribute(attr => Json.toJson(attr).as[ATTR]))
    }.flatten

  private def getDataStore(url: URL): DataStore = {
    val fileParams = Map("url" -> url)
    DataStoreFinder.getDataStore(fileParams.asJava)
  }

  def readFeatureInCRS[CRS: CRST](url: URL): FeatureInCRS[CRS, Geometry, ShapeAttributes] =
    WithDataStore(getDataStore(url))(ds => ShapeFileSupport.readFeature[CRS](ds))

  def readFeatures(file: File): FeatureCollection[Geometry, ShapeAttributes] =
    readFeaturesInCRS[EPSG_4326](file)

  def readFeaturesInCRS[CRS: CRST](file: File): FeatureCollectionInCRS[CRS, Geometry, ShapeAttributes] =
    readFeaturesInCRS[CRS](file.toURI.toURL)

  def readFeatures(url: URL): FeatureCollection[Geometry, ShapeAttributes] =
    readFeaturesInCRS[EPSG_4326](url)

  def readFeaturesInCRS[CRS: CRST](
      url: URL): FeatureCollectionInCRS[CRS, Geometry, ShapeAttributes] =
    WithDataStore(getDataStore(url))(ds =>
      ShapeFileSupport.readFeaturesInCRS[CRS](ds, url.toString))

  def readAttributedFeatures[ATTR: Reads](url: URL): Try[FeatureCollectionInCRS[EPSG_4326, Geometry, ATTR]] =
    Try {
      val rawFeatures = readFeaturesInCRS[EPSG_4326](url)
      Try(rawFeatures.mapAttributes { sa => Json.toJson(sa).as[ATTR] })
    }.flatten

  def writeFeatureCollectionToFile[CRS: CRST](
      file: File,
      features: FeatureCollectionInCRS[CRS, Geometry, ShapeAttributes]): Unit = {
    val sfc = typedFeatureCollectionToSimpleFeatureCollection(features)
    val dataStore = buildDataStore(file, sfc.getSchema)
    val transaction = new DefaultTransaction("create")
    val featureStore = buildTransactionalFeatureStore(transaction, dataStore)
    saveToFile(transaction, featureStore, sfc)
  }

  def writeAttributedFeatureCollectionToFile[CRS: CRST, ATTR: Writes](
      file: File,
      features: FeatureCollectionInCRS[CRS, Geometry, ATTR]): Unit = {
    val rawFeatures = features.mapAttributes(a => Json.toJson(a).as[ShapeAttributes])
    writeFeatureCollectionToFile[CRS](file, rawFeatures)
  }

  def writeFeatureToFile[CRS: CRST](
      file: File,
      feature: FeatureInCRS[CRS, Geometry, ShapeAttributes]
  ): Unit = {
    writeFeatureCollectionToFile(file, FeatureCollectionInCRS(Seq(feature)))
  }

  def writeAttributedFeatureToFile[CRS: CRST, ATTR: Writes](
      file: File,
      feature: FeatureInCRS[CRS, Geometry, ATTR]): Unit = {

    val rawFeature = feature.mapAttribute(Json.toJson(_).as[ShapeAttributes])
    writeFeatureToFile[CRS](file, rawFeature)
  }
