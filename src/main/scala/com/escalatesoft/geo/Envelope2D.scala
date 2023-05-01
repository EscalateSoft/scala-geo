package com.escalatesoft.geo

import com.escalatesoft.geo.crs.CRST
import com.escalatesoft.geo.crs.CRST.CRSDefinitions.EPSG_4326
import com.github.davidmoten.geo.GeoHash

import scala.util.Try

/**
  * Represent a projection-aligned rectangular envelope for a given CRS, with
  * minDiag and maxDiag corners specified.
  * @param minDiag The (mathematically) minimum diagonal coordinates
  * @param maxDiag The (mathematically) maximum diagonal coordinates
  * @tparam CRS The coordinate reference system for the envelope
  */
case class Envelope2D[CRS: CRST] private (minDiag: Point2D[CRS], maxDiag: Point2D[CRS]):

  /**
    * Return a Polygon2D of the rectangular bounds of this envelope
    * in the same CRS as the envelope
    * @return Polygon2D of envelope bounds in same CRS
    */
  def boundsPolygon: Polygon2D[CRS] =
    Polygon2D.fromPoints(
      Seq(
        minDiag,
        Point2D[CRS](x = minDiag.x, y = maxDiag.y),
        maxDiag,
        Point2D[CRS](x = maxDiag.x, y = minDiag.y)
      )
    )

  /**
    * @return The width of the envelope, in CRS units, simply calculated
    * as maxDiag.x - minDiag.x
    */
  def width: Double = maxDiag.x - minDiag.x

  /**
    * @return The height of the envelope, in CRS units, simply calculated
    *         as maxDiag.y - minDiag.y
    */
  def height: Double = maxDiag.y - minDiag.y

  /**
    * @return The area of the envelope in CRS units squared
    */
  def area: Double = width * height

  /**
    * Given a target resolution (pixel size) in CRS units, return a count of
    * columns necessary to cover this envelope
    * @param resolution the desired pixel size of a raster fit over this envelope
    * @return A count of the number of columns to cover this envelope at the
    *         requested resolution.
    */
  def makeColumnCount(resolution: Double): Int = (width / resolution).round.toInt

  /**
    * Given a target resolution (pixel size) in CRS units, return a count of
    * rows necessary to cover this envelope
    * @param resolution the desired pixel size of a raster fit over this envelope
    * @return A count of the number of rows to cover this envelope at the
    *         requested resolution.
    */
  def makeRowCount(resolution: Double): Int = (height / resolution).round.toInt

  /**
    * The centroid of this envelope, calculated as the point midway between minDiag
    * and maxDiag in the envelope CRS
    */
  lazy val centroid: Point2D[CRS] =
    minDiag.updated(minDiag.x + width / 2.0, minDiag.y + height / 2.0)

  /**
    * Combine two envelopes to create an envelope of either the same size
    * or larger than the largest of the two.
    * @param other Another envelope, that must be in the same CRS as this one
    * @return new envelope completely covering both envelopes.
    */
  def combineWith(other: Envelope2D[CRS]): Envelope2D[CRS] =
    val minX = minDiag.x min other.minDiag.x
    val minY = minDiag.y min other.minDiag.y
    val maxX = maxDiag.x max other.maxDiag.x
    val maxY = maxDiag.y max other.maxDiag.y
    new Envelope2D[CRS](Point2D[CRS](minX, minY), Point2D[CRS](maxX, maxY))

object Envelope2D:

  /**
    * Construct an Envelope2D from any two points in a consistent CRS. The
    * new envelope will be the minimum envelope covering both points in the same
    * CRS.
    * @param p1 the first point
    * @param p2 the second point
    * @tparam CRS The CRS of the points and the resulting Envelope
    * @return The minimum size, projection aligned rectangle covering both points
    */
  def apply[CRS: CRST](p1: Point2D[CRS], p2: Point2D[CRS]): Envelope2D[CRS] =
    val minX = p1.x min p2.x
    val maxX = p1.x max p2.x
    val minY = p1.y min p2.y
    val maxY = p1.y max p2.y

    new Envelope2D[CRS](Point2D[CRS](minX, minY), Point2D[CRS](maxX, maxY))

  /**
    * Create an envelope from the given geohash string in EPSG_4326.
    * If the provided string is not a valid geohash, this method will
    * give back a Failed Try
    * @param hash the string containing the geohash
    * @return Envelope2D in WGS84 for the geohash area
    */
  def fromGeohash(hash: String): Try[Envelope2D[EPSG_4326]] = Try {
    val hashLength = hash.length

    val boxWidth = GeoHash.widthDegrees(hashLength)
    val boxHeight = GeoHash.heightDegrees(hashLength)

    val centroid = GeoHash.decodeHash(hash)

    val minDiag =
      Point2D[EPSG_4326](centroid.getLon - boxWidth / 2.0, centroid.getLat - boxHeight / 2.0)
    val maxDiag = minDiag.updated(x = minDiag.x + boxWidth, y = minDiag.y + boxHeight)

    Envelope2D[EPSG_4326](minDiag, maxDiag)
  }

