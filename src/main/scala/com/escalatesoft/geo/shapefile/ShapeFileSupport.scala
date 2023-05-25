package com.escalatesoft.geo.shapefile

import com.escalatesoft.geo.FeatureCollectionInCRS
import com.escalatesoft.geo.FeatureInCRS
import com.escalatesoft.geo.Geometry
import com.escalatesoft.geo.crs.CRST
import com.typesafe.scalalogging.LazyLogging
import org.geotools.data.DataStore
import org.geotools.data.DataUtilities
import org.geotools.data.Transaction
import org.geotools.data.shapefile.ShapefileDataStore
import org.geotools.data.shapefile.ShapefileDataStoreFactory
import org.geotools.data.simple.SimpleFeatureCollection
import org.geotools.data.simple.SimpleFeatureSource
import org.geotools.data.simple.SimpleFeatureStore
import org.geotools.feature.simple.SimpleFeatureBuilder
import org.geotools.feature.simple.SimpleFeatureTypeBuilder
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.{CRS => GCRS}
import org.locationtech.jts.{geom => jts}
import org.opengis.feature.simple.SimpleFeature
import org.opengis.feature.simple.SimpleFeatureType
import org.opengis.referencing.operation.MathTransform

import java.io.File
import scala.jdk.CollectionConverters.given

import ShapeFileConverters.given

private[shapefile] object ShapeFileSupport extends LazyLogging:

  private[shapefile] def findTransform[CRS: CRST](featureSource: SimpleFeatureSource): MathTransform =
    val desiredCRS = summon[CRST[CRS]].crs
    val sourceCRS =
      val foundSchemaOption = Option(featureSource.getSchema.getCoordinateReferenceSystem)
      foundSchemaOption match
        case None =>
          logger.warn(s"SourceCRS is null, assuming ${desiredCRS.getName}")
          desiredCRS
        case Some(foundSchema) =>
          foundSchema

    val transform = GCRS.findMathTransform(sourceCRS, desiredCRS, true)

    if !transform.isIdentity then
      logger.debug(s"converting from ${sourceCRS.getName} to ${desiredCRS.getName}")

    transform

  private[shapefile] def buildDataStore(
    file: File,
    simpleFeatureType: SimpleFeatureType,
  ): ShapefileDataStore =
    val params = Map[String, java.io.Serializable](
      "url" -> file.toURI.toURL.asInstanceOf[java.io.Serializable])
    val dataStoreFactory = new ShapefileDataStoreFactory
    val dataStore =
      dataStoreFactory.createNewDataStore(params.asJava).asInstanceOf[ShapefileDataStore]
    dataStore.createSchema(simpleFeatureType)
    dataStore

  private[shapefile] def buildTransactionalFeatureStore(
      transaction: Transaction,
      dataStore: DataStore): SimpleFeatureStore =
    val typeName = dataStore.getTypeNames()(0)
    val featureSource = dataStore.getFeatureSource(typeName)
    val featureStore = featureSource.asInstanceOf[SimpleFeatureStore]
    featureStore.setTransaction(transaction)
    featureStore

  private[shapefile] def saveToFile(
    transaction: Transaction,
    featureStore: SimpleFeatureStore,
    featureCollection: SimpleFeatureCollection,
  ): Unit =
    try
      featureStore.addFeatures(featureCollection)
      transaction.commit()
    catch
      case e: Exception =>
        transaction.rollback()
        throw e
    finally
      transaction.close()

  private[shapefile] def typedFeatureToSimpleFeature[CRS: CRST](
    cf: FeatureInCRS[CRS, Geometry, ShapeAttributes],
  ): SimpleFeature =
    typedFeatureToSimpleFeature[CRS](cf, cf.id)

  private[shapefile] def typedFeatureToSimpleFeature[CRS: CRST](
    cf: FeatureInCRS[CRS, Geometry, ShapeAttributes],
    name: String,
  ): SimpleFeature =
    val crs = summon[CRST[CRS]].crs
    val geometry = cf.geometry.asJTS

    val typeBuilder = new SimpleFeatureTypeBuilder

    typeBuilder.setCRS(crs)
    typeBuilder.setName(name)

    typeBuilder.add("the_geom", geometry.getClass)
    val propSeq: Seq[(String, ShapeAttribute)] = cf.attr.attributes.toSeq

    propSeq.foreach { case (k, v) => typeBuilder.add(k, v.toShapeField.getClass) }

    val geometryType = typeBuilder.buildFeatureType

    val builder = new SimpleFeatureBuilder(geometryType)
    builder.add(geometry)

    propSeq.foreach { case (_, v) => builder.add(v.toShapeField) }

    builder.buildFeature(cf.id)

  private[shapefile] def typedFeatureCollectionToSimpleFeatureCollection[CRS: CRST](
      fc: FeatureCollectionInCRS[CRS, Geometry, ShapeAttributes],
  ): SimpleFeatureCollection =
    val name = fc.featureTypeId
    val sfs = fc.features.map(f => typedFeatureToSimpleFeature[CRS](f, name.getOrElse(f.id)))
    val asArray = sfs.toArray
    DataUtilities.collection(asArray: _*)
    

  private[shapefile] def simpleFeatureToTypedFeature[CRS: CRST](
    sf: SimpleFeature,
    mathTransform: MathTransform,
    isIdentity: Boolean,
  ): FeatureInCRS[CRS, Geometry, ShapeAttributes] =

    val id = Option(sf.getID)
    val attributes = shapeAttributesFromFeature(sf)
    val rawGeom = sf.getDefaultGeometry.asInstanceOf[jts.Geometry]
    val jtsGeom =
      if isIdentity then rawGeom
      else JTS.transform(rawGeom, mathTransform)
    val geometry = Geometry.fromJTS[CRS](jtsGeom)
    FeatureInCRS[CRS, Geometry, ShapeAttributes](geometry, attributes, setId = id)

  private def shapeAttributesFromFeature(sf: SimpleFeature): ShapeAttributes =
    val props = sf.getProperties.asScala

    val attrs = for
      prop <- props
      name = prop.getName.toString
      attr = ShapeAttribute.parse(prop.getValue)
      _ = if attr.isFailure then logger.warn(s"Unable to parse ${prop.getName}")
      if attr.isSuccess
    yield name -> attr.get

    ShapeAttributes(attrs.toMap)

  private[shapefile] def readFeature[CRS: CRST](
    dataStore: DataStore,
  ): FeatureInCRS[CRS, Geometry, ShapeAttributes] =
    val featSource: SimpleFeatureSource = featureSourceFromDataStore(dataStore)
    val mathTransform = findTransform[CRS](featSource)

    val feature =
      val fs = featSource.getFeatures().features()
      try fs.next()
      finally fs.close()
    
    simpleFeatureToTypedFeature(feature, mathTransform, mathTransform.isIdentity)

  private[shapefile] def featureSourceFromDataStore(dataStore: DataStore) =
    val typeName = dataStore.getTypeNames()(0)
    dataStore.getFeatureSource(typeName)

  private[shapefile] def readFeaturesInCRS[CRS: CRST](
    dataStore: DataStore,
    name: String,
  ): FeatureCollectionInCRS[CRS, Geometry, ShapeAttributes] =

    logger.debug(s"Begin reading from url $name")

    val featureSource: SimpleFeatureSource = featureSourceFromDataStore(dataStore)
    val mathTransform = findTransform[CRS](featureSource)

    val isIdentity = mathTransform.isIdentity

    val sfc = featureSource.getFeatures

    val features = sfc
      .filter(_.getDefaultGeometry != null)
      .map(sf => simpleFeatureToTypedFeature[CRS](sf, mathTransform, isIdentity))
      .toVector

    logger.debug(s"${features.size} features read from $name")
    FeatureCollectionInCRS(features, Option(sfc.getSchema.getTypeName))
