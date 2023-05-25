package com.escalatesoft.geo

import com.escalatesoft.geo.crs.CRST
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.{geom => jts}

/**
  * Represents a collection of multiple distinct LineString2Ds
  * @param jtsMls the (hidden) JTS geometry on which this is based
  * @tparam CRS the CRS of the 1D geometry
  */
class MultiLineString2D[CRS: CRST] private (jtsMls: jts.MultiLineString)
    extends Lineal[CRS]
    with GeometryCollection[CRS, LineString2D[CRS], jts.LineString]:

  @inline override protected[geo] def asJTS: jts.MultiLineString = jtsMls

  override def transformCRS[NEW_CRS: CRST]: MultiLineString2D[NEW_CRS] =
    val transformed = Geometry.transformCRS[CRS, NEW_CRS](jtsMls).asInstanceOf[jts.MultiLineString]
    new MultiLineString2D[NEW_CRS](transformed)

  def assignId(id: String): MultiLineString2D[CRS] with GeometryWithID[CRS] =
    new MultiLineString2D[CRS](jtsMls) with GeometryWithID[CRS]:
      override val geometryId: String = id

  override def toWKT: String = jtsMls.toText

  protected def makeComponent(jtsG: LineString): LineString2D[CRS] = LineString2D[CRS](jtsG)

object MultiLineString2D:
  private[geo] def fromJTS[CRS: CRST](jtsMls: jts.MultiLineString): MultiLineString2D[CRS] =
    new MultiLineString2D[CRS](jtsMls)

  /**
    * Construct a new MultiLineString2D from a sequence of LineString2Ds
    * @param lineStrings the sequence of LineStrings to include in this MultiLineString
    * @tparam CRS The CRS of the LineStrings, and the resulting MultiLineString
    * @return a new MultiLineString with all of the provided LineStrings in it
    */
  def apply[CRS: CRST](lineStrings: Seq[LineString2D[CRS]]): MultiLineString2D[CRS] =
    val factory = new jts.GeometryFactory

    val lineStringsArray: Array[LineString] = lineStrings.map(_.asJTS).toArray

    MultiLineString2D.fromJTS[CRS](factory.createMultiLineString(lineStringsArray))

  /**
    * Construct a new MultiLineString2D from a sequence of LineString2Ds
    * @param lineStrings the sequence of LineStrings to include in this MultiLineString
    * @param withId stable, unique String ID
    * @tparam CRS The CRS of the LineStrings, and the resulting MultiLineString
    * @return a new MultiLineString with all of the provided LineStrings in it
    */
  def apply[CRS: CRST](
    lineStrings: Seq[LineString2D[CRS]],
    withId: String
  ): MultiLineString2D[CRS] with GeometryWithID[CRS] =
    val factory = new jts.GeometryFactory

    val lineStringsArray: Array[LineString] = lineStrings.map(_.asJTS).toArray

    new MultiLineString2D[CRS](factory.createMultiLineString(lineStringsArray))
    with GeometryWithID[CRS]:
      val geometryId: String = withId
