package com.escalatesoft.geo.crs

import com.typesafe.scalalogging.LazyLogging
import org.geotools.coverage.grid.GridCoverage2D
import org.geotools.gce.geotiff.GeoTiffReader
import org.geotools.referencing.{CRS => GCRS}
import org.opengis.referencing.crs.CoordinateReferenceSystem

import java.io.File
import scala.util.Try
import scala.util.matching.Regex

abstract class CRSDef(val crs: CoordinateReferenceSystem) extends Serializable:
  def crsId: String = crs.getIdentifiers.toArray.head.toString

  def this(crsId: String) =
    this(GCRS.decode(crsId, CRST.CRSDefinitions.longitudeFirst))


abstract class CRST[T]:
  type CRS_TYPE = T
  def apply(): CRSDef
  final lazy val crsDef: CRSDef = apply()
  final lazy val crs: CoordinateReferenceSystem = crsDef.crs
  final lazy val crsId: String = crsDef.crsId
  given CRST[CRS_TYPE] = this

  private val X_AXIS = 0

  lazy val (pointTolerance, areaTolerance) = Try {
    defaultTolerances(
      // TODO verify this is correct
      crs.getCoordinateSystem.getAxis(X_AXIS).getUnit.getSystemUnit.toString
    )
  }.getOrElse(1e-9 -> 1e-9)

  protected def defaultTolerances(unitStr: String): (Double, Double) =
    unitStr match
      case "m" => (1e-4, 1e-1)
      case _   => (1e-9, 1e-9)

abstract class CRSAngle[T] extends CRST[T]:
  given CRSAngle[CRS_TYPE] = this

abstract class CRSDegree[T] extends CRSAngle[T]:
  given CRSDegree[CRS_TYPE] = this
  override lazy val pointTolerance: Double = 1e-9
  override lazy val areaTolerance: Double = 1e-9

abstract class CRSLength[T] extends CRST[T]:
  given CRSLength[CRS_TYPE] = this

abstract class CRSMeter[T] extends CRSLength[T]:
  given CRSMeter[CRS_TYPE] = this
  override lazy val pointTolerance: Double = 1e-4
  override lazy val areaTolerance: Double = 1e-1

object CRST extends LazyLogging:

  trait SomeCRS extends Serializable:
    type SOME_CRS
    given SOME_CRS: CRST[SOME_CRS]

    override def toString: String = s"SomeCRS($SOME_CRS)"

    override def equals(obj: Any): Boolean =
      obj match
        case otherCRS: SomeCRS =>
          this.SOME_CRS == otherCRS.SOME_CRS
        case _ => false

    override def hashCode(): Int = SOME_CRS.##

  object CRSDefinitions:
    // After this point, all decoded CRSs will be X first, strictly
    System.setProperty("org.geotools.referencing.forceXY", "true")
    val longitudeFirst = true // use this to swap order of coords

    // general WGS84
    case class EPSG_4326() extends CRSDef("EPSG:4326")
    implicit object EPSG_4326 extends CRSDegree[EPSG_4326]

    // NAD83
    case class EPSG_4269() extends CRSDef("EPSG:4269")
    implicit object EPSG_4269 extends CRSDegree[EPSG_4269]

    // Albers Conical Equal Area
    case class EPSG_5070() extends CRSDef("EPSG:5070")
    implicit object EPSG_5070 extends CRSMeter[EPSG_5070]

    // NAD83 / UTM zone 01N
    case class EPSG_26901() extends CRSDef("EPSG:26901")
    implicit object EPSG_26901 extends CRSMeter[EPSG_26901]

    // NAD83 / UTM zone 02N
    case class EPSG_26902() extends CRSDef("EPSG:26902")
    implicit object EPSG_26902 extends CRSMeter[EPSG_26902]

    // NAD83 / UTM zone 03N
    case class EPSG_26903() extends CRSDef("EPSG:26903")
    implicit object EPSG_26903 extends CRSMeter[EPSG_26903]

    // NAD83 / UTM zone 04N
    case class EPSG_26904() extends CRSDef("EPSG:26904")
    implicit object EPSG_26904 extends CRSMeter[EPSG_26904]

    // NAD83 / UTM zone 05N
    case class EPSG_26905() extends CRSDef("EPSG:26905")
    implicit object EPSG_26905 extends CRSMeter[EPSG_26905]

    // NAD83 / UTM zone 06N
    case class EPSG_26906() extends CRSDef("EPSG:26906")
    implicit object EPSG_26906 extends CRSMeter[EPSG_26906]

    // NAD83 / UTM zone 07N
    case class EPSG_26907() extends CRSDef("EPSG:26907")
    implicit object EPSG_26907 extends CRSMeter[EPSG_26907]

    // NAD83 / UTM zone 08N
    case class EPSG_26908() extends CRSDef("EPSG:26908")
    implicit object EPSG_26908 extends CRSMeter[EPSG_26908]

    // NAD83 / UTM zone 09N
    case class EPSG_26909() extends CRSDef("EPSG:26909")
    implicit object EPSG_26909 extends CRSMeter[EPSG_26909]

    // NAD83 / UTM zone 10N
    case class EPSG_26910() extends CRSDef("EPSG:26910")
    implicit object EPSG_26910 extends CRSMeter[EPSG_26910]

    // NAD83 / UTM zone 11N
    case class EPSG_26911() extends CRSDef("EPSG:26911")
    implicit object EPSG_26911 extends CRSMeter[EPSG_26911]

    // NAD83 / UTM zone 12N
    case class EPSG_26912() extends CRSDef("EPSG:26912")
    implicit object EPSG_26912 extends CRSMeter[EPSG_26912]

    // NAD83 / UTM zone 13N
    case class EPSG_26913() extends CRSDef("EPSG:26913")
    implicit object EPSG_26913 extends CRSMeter[EPSG_26913]

    // NAD83 / UTM zone 14N
    case class EPSG_26914() extends CRSDef("EPSG:26914")
    implicit object EPSG_26914 extends CRSMeter[EPSG_26914]

    // NAD83 / UTM zone 15N
    case class EPSG_26915() extends CRSDef("EPSG:26915")
    implicit object EPSG_26915 extends CRSMeter[EPSG_26915]

    // NAD83 / UTM zone 16N
    case class EPSG_26916() extends CRSDef("EPSG:26916")
    implicit object EPSG_26916 extends CRSMeter[EPSG_26916]

    // NAD83 / UTM zone 17N
    case class EPSG_26917() extends CRSDef("EPSG:26917")
    implicit object EPSG_26917 extends CRSMeter[EPSG_26917]

    // NAD83 / UTM zone 18N
    case class EPSG_26918() extends CRSDef("EPSG:26918")
    implicit object EPSG_26918 extends CRSMeter[EPSG_26918]

    // NAD83 / UTM zone 19N
    case class EPSG_26919() extends CRSDef("EPSG:26919")
    implicit object EPSG_26919 extends CRSMeter[EPSG_26919]

    // NAD83 / UTM zone 20N
    case class EPSG_26920() extends CRSDef("EPSG:26920")
    implicit object EPSG_26920 extends CRSMeter[EPSG_26920]

    // NAD83 / UTM zone 21N
    case class EPSG_26921() extends CRSDef("EPSG:26921")
    implicit object EPSG_26921 extends CRSMeter[EPSG_26921]

    // NAD83 / UTM zone 22N
    case class EPSG_26922() extends CRSDef("EPSG:26922")
    implicit object EPSG_26922 extends CRSMeter[EPSG_26922]

    // NAD83 / UTM zone 23N
    case class EPSG_26923() extends CRSDef("EPSG:26923")
    implicit object EPSG_26923 extends CRSMeter[EPSG_26923]

    // NAD83 / Vermont
    case class EPSG_32145() extends CRSDef("EPSG:32145")
    implicit object EPSG_32145 extends CRSMeter[EPSG_32145]

    // UTM Zones for Northern Hemisphere

    // WGS84 UTM zone 01N
    case class EPSG_32601() extends CRSDef("EPSG:32601")
    implicit object EPSG_32601 extends CRSMeter[EPSG_32601]

    // WGS84 UTM zone 02N
    case class EPSG_32602() extends CRSDef("EPSG:32602")
    implicit object EPSG_32602 extends CRSMeter[EPSG_32602]

    // WGS84 UTM zone 03N
    case class EPSG_32603() extends CRSDef("EPSG:32603")
    implicit object EPSG_32603 extends CRSMeter[EPSG_32603]

    // WGS84 UTM zone 04N
    case class EPSG_32604() extends CRSDef("EPSG:32604")
    implicit object EPSG_32604 extends CRSMeter[EPSG_32604]

    // WGS84 UTM zone 05N
    case class EPSG_32605() extends CRSDef("EPSG:32605")
    implicit object EPSG_32605 extends CRSMeter[EPSG_32605]

    // WGS84 UTM zone 06N
    case class EPSG_32606() extends CRSDef("EPSG:32606")
    implicit object EPSG_32606 extends CRSMeter[EPSG_32606]

    // WGS84 UTM zone 07N
    case class EPSG_32607() extends CRSDef("EPSG:32607")
    implicit object EPSG_32607 extends CRSMeter[EPSG_32607]

    // WGS84 UTM zone 08N
    case class EPSG_32608() extends CRSDef("EPSG:32608")
    implicit object EPSG_32608 extends CRSMeter[EPSG_32608]

    // WGS84 UTM zone 09N
    case class EPSG_32609() extends CRSDef("EPSG:32609")
    implicit object EPSG_32609 extends CRSMeter[EPSG_32609]

    // WGS84 UTM zone 10N
    case class EPSG_32610() extends CRSDef("EPSG:32610")
    implicit object EPSG_32610 extends CRSMeter[EPSG_32610]

    // WGS84 UTM zone 11N
    case class EPSG_32611() extends CRSDef("EPSG:32611")
    implicit object EPSG_32611 extends CRSMeter[EPSG_32611]

    // WGS84 UTM zone 12N
    case class EPSG_32612() extends CRSDef("EPSG:32612")
    implicit object EPSG_32612 extends CRSMeter[EPSG_32612]

    // WGS84 UTM zone 13N
    case class EPSG_32613() extends CRSDef("EPSG:32613")
    implicit object EPSG_32613 extends CRSMeter[EPSG_32613]

    // WGS84 UTM zone 14N
    case class EPSG_32614() extends CRSDef("EPSG:32614")
    implicit object EPSG_32614 extends CRSMeter[EPSG_32614]

    // WGS84 UTM zone 15N
    case class EPSG_32615() extends CRSDef("EPSG:32615")
    implicit object EPSG_32615 extends CRSMeter[EPSG_32615]

    // WGS84 UTM zone 16N
    case class EPSG_32616() extends CRSDef("EPSG:32616")
    implicit object EPSG_32616 extends CRSMeter[EPSG_32616]

    // WGS84 UTM zone 17N
    case class EPSG_32617() extends CRSDef("EPSG:32617")
    implicit object EPSG_32617 extends CRSMeter[EPSG_32617]

    // WGS84 UTM zone 18N
    case class EPSG_32618() extends CRSDef("EPSG:32618")
    implicit object EPSG_32618 extends CRSMeter[EPSG_32618]

    // WGS84 UTM zone 19N
    case class EPSG_32619() extends CRSDef("EPSG:32619")
    implicit object EPSG_32619 extends CRSMeter[EPSG_32619]

    // WGS84 UTM zone 20N
    case class EPSG_32620() extends CRSDef("EPSG:32620")
    implicit object EPSG_32620 extends CRSMeter[EPSG_32620]

    // WGS84 UTM zone 21N
    case class EPSG_32621() extends CRSDef("EPSG:32621")
    implicit object EPSG_32621 extends CRSMeter[EPSG_32621]

    // WGS84 UTM zone 22N
    case class EPSG_32622() extends CRSDef("EPSG:32622")
    implicit object EPSG_32622 extends CRSMeter[EPSG_32622]

    // WGS84 UTM zone 23N
    case class EPSG_32623() extends CRSDef("EPSG:32623")
    implicit object EPSG_32623 extends CRSMeter[EPSG_32623]

    // WGS84 UTM zone 24N
    case class EPSG_32624() extends CRSDef("EPSG:32624")
    implicit object EPSG_32624 extends CRSMeter[EPSG_32624]

    // WGS84 UTM zone 25N
    case class EPSG_32625() extends CRSDef("EPSG:32625")
    implicit object EPSG_32625 extends CRSMeter[EPSG_32625]

    // WGS84 UTM zone 26N
    case class EPSG_32626() extends CRSDef("EPSG:32626")
    implicit object EPSG_32626 extends CRSMeter[EPSG_32626]

    // WGS84 UTM zone 27N
    case class EPSG_32627() extends CRSDef("EPSG:32627")
    implicit object EPSG_32627 extends CRSMeter[EPSG_32627]

    // WGS84 UTM zone 28N
    case class EPSG_32628() extends CRSDef("EPSG:32628")
    implicit object EPSG_32628 extends CRSMeter[EPSG_32628]

    // WGS84 UTM zone 29N
    case class EPSG_32629() extends CRSDef("EPSG:32629")
    implicit object EPSG_32629 extends CRSMeter[EPSG_32629]

    // WGS84 UTM zone 30N
    case class EPSG_32630() extends CRSDef("EPSG:32630")
    implicit object EPSG_32630 extends CRSMeter[EPSG_32630]

    // WGS84 UTM zone 31N
    case class EPSG_32631() extends CRSDef("EPSG:32631")
    implicit object EPSG_32631 extends CRSMeter[EPSG_32631]

    // WGS84 UTM zone 32N
    case class EPSG_32632() extends CRSDef("EPSG:32632")
    implicit object EPSG_32632 extends CRSMeter[EPSG_32632]

    // WGS84 UTM zone 33N
    case class EPSG_32633() extends CRSDef("EPSG:32633")
    implicit object EPSG_32633 extends CRSMeter[EPSG_32633]

    // WGS84 UTM zone 34N
    case class EPSG_32634() extends CRSDef("EPSG:32634")
    implicit object EPSG_32634 extends CRSMeter[EPSG_32634]

    // WGS84 UTM zone 35N
    case class EPSG_32635() extends CRSDef("EPSG:32635")
    implicit object EPSG_32635 extends CRSMeter[EPSG_32635]

    // WGS84 UTM zone 36N
    case class EPSG_32636() extends CRSDef("EPSG:32636")
    implicit object EPSG_32636 extends CRSMeter[EPSG_32636]

    // WGS84 UTM zone 37N
    case class EPSG_32637() extends CRSDef("EPSG:32637")
    implicit object EPSG_32637 extends CRSMeter[EPSG_32637]

    // WGS84 UTM zone 38N
    case class EPSG_32638() extends CRSDef("EPSG:32638")
    implicit object EPSG_32638 extends CRSMeter[EPSG_32638]

    // WGS84 UTM zone 39N
    case class EPSG_32639() extends CRSDef("EPSG:32639")
    implicit object EPSG_32639 extends CRSMeter[EPSG_32639]

    // WGS84 UTM zone 40N
    case class EPSG_32640() extends CRSDef("EPSG:32640")
    implicit object EPSG_32640 extends CRSMeter[EPSG_32640]

    // WGS84 UTM zone 41N
    case class EPSG_32641() extends CRSDef("EPSG:32641")
    implicit object EPSG_32641 extends CRSMeter[EPSG_32641]

    // WGS84 UTM zone 42N
    case class EPSG_32642() extends CRSDef("EPSG:32642")
    implicit object EPSG_32642 extends CRSMeter[EPSG_32642]

    // WGS84 UTM zone 43N
    case class EPSG_32643() extends CRSDef("EPSG:32643")
    implicit object EPSG_32643 extends CRSMeter[EPSG_32643]

    // WGS84 UTM zone 44N
    case class EPSG_32644() extends CRSDef("EPSG:32644")
    implicit object EPSG_32644 extends CRSMeter[EPSG_32644]

    // WGS84 UTM zone 45N
    case class EPSG_32645() extends CRSDef("EPSG:32645")
    implicit object EPSG_32645 extends CRSMeter[EPSG_32645]

    // WGS84 UTM zone 46N
    case class EPSG_32646() extends CRSDef("EPSG:32646")
    implicit object EPSG_32646 extends CRSMeter[EPSG_32646]

    // WGS84 UTM zone 47N
    case class EPSG_32647() extends CRSDef("EPSG:32647")
    implicit object EPSG_32647 extends CRSMeter[EPSG_32647]

    // WGS84 UTM zone 48N
    case class EPSG_32648() extends CRSDef("EPSG:32648")
    implicit object EPSG_32648 extends CRSMeter[EPSG_32648]

    // WGS84 UTM zone 49N
    case class EPSG_32649() extends CRSDef("EPSG:32649")
    implicit object EPSG_32649 extends CRSMeter[EPSG_32649]

    // WGS84 UTM zone 50N
    case class EPSG_32650() extends CRSDef("EPSG:32650")
    implicit object EPSG_32650 extends CRSMeter[EPSG_32650]

    // WGS84 UTM zone 51N
    case class EPSG_32651() extends CRSDef("EPSG:32651")
    implicit object EPSG_32651 extends CRSMeter[EPSG_32651]

    // WGS84 UTM zone 52N
    case class EPSG_32652() extends CRSDef("EPSG:32652")
    implicit object EPSG_32652 extends CRSMeter[EPSG_32652]

    // WGS84 UTM zone 53N
    case class EPSG_32653() extends CRSDef("EPSG:32653")
    implicit object EPSG_32653 extends CRSMeter[EPSG_32653]

    // WGS84 UTM zone 54N
    case class EPSG_32654() extends CRSDef("EPSG:32654")
    implicit object EPSG_32654 extends CRSMeter[EPSG_32654]

    // WGS84 UTM zone 55N
    case class EPSG_32655() extends CRSDef("EPSG:32655")
    implicit object EPSG_32655 extends CRSMeter[EPSG_32655]

    // WGS84 UTM zone 56N
    case class EPSG_32656() extends CRSDef("EPSG:32656")
    implicit object EPSG_32656 extends CRSMeter[EPSG_32656]

    // WGS84 UTM zone 57N
    case class EPSG_32657() extends CRSDef("EPSG:32657")
    implicit object EPSG_32657 extends CRSMeter[EPSG_32657]

    // WGS84 UTM zone 58N
    case class EPSG_32658() extends CRSDef("EPSG:32658")
    implicit object EPSG_32658 extends CRSMeter[EPSG_32658]

    // WGS84 UTM zone 59N
    case class EPSG_32659() extends CRSDef("EPSG:32659")
    implicit object EPSG_32659 extends CRSMeter[EPSG_32659]

    // WGS84 UTM zone 60N
    case class EPSG_32660() extends CRSDef("EPSG:32660")
    implicit object EPSG_32660 extends CRSMeter[EPSG_32660]

    // UTM Zones for Southern Hemisphere

    // WGS84 UTM zone 01S
    case class EPSG_32701() extends CRSDef("EPSG:32701")
    implicit object EPSG_32701 extends CRSMeter[EPSG_32701]

    // WGS84 UTM zone 02S
    case class EPSG_32702() extends CRSDef("EPSG:32702")
    implicit object EPSG_32702 extends CRSMeter[EPSG_32702]

    // WGS84 UTM zone 03S
    case class EPSG_32703() extends CRSDef("EPSG:32703")
    implicit object EPSG_32703 extends CRSMeter[EPSG_32703]

    // WGS84 UTM zone 04S
    case class EPSG_32704() extends CRSDef("EPSG:32704")
    implicit object EPSG_32704 extends CRSMeter[EPSG_32704]

    // WGS84 UTM zone 05S
    case class EPSG_32705() extends CRSDef("EPSG:32705")
    implicit object EPSG_32705 extends CRSMeter[EPSG_32705]

    // WGS84 UTM zone 06S
    case class EPSG_32706() extends CRSDef("EPSG:32706")
    implicit object EPSG_32706 extends CRSMeter[EPSG_32706]

    // WGS84 UTM zone 07S
    case class EPSG_32707() extends CRSDef("EPSG:32707")
    implicit object EPSG_32707 extends CRSMeter[EPSG_32707]

    // WGS84 UTM zone 08S
    case class EPSG_32708() extends CRSDef("EPSG:32708")
    implicit object EPSG_32708 extends CRSMeter[EPSG_32708]

    // WGS84 UTM zone 09S
    case class EPSG_32709() extends CRSDef("EPSG:32709")
    implicit object EPSG_32709 extends CRSMeter[EPSG_32709]

    // WGS84 UTM zone 10S
    case class EPSG_32710() extends CRSDef("EPSG:32710")
    implicit object EPSG_32710 extends CRSMeter[EPSG_32710]

    // WGS84 UTM zone 11S
    case class EPSG_32711() extends CRSDef("EPSG:32711")
    implicit object EPSG_32711 extends CRSMeter[EPSG_32711]

    // WGS84 UTM zone 12S
    case class EPSG_32712() extends CRSDef("EPSG:32712")
    implicit object EPSG_32712 extends CRSMeter[EPSG_32712]

    // WGS84 UTM zone 13S
    case class EPSG_32713() extends CRSDef("EPSG:32713")
    implicit object EPSG_32713 extends CRSMeter[EPSG_32713]

    // WGS84 UTM zone 14S
    case class EPSG_32714() extends CRSDef("EPSG:32714")
    implicit object EPSG_32714 extends CRSMeter[EPSG_32714]

    // WGS84 UTM zone 15S
    case class EPSG_32715() extends CRSDef("EPSG:32715")
    implicit object EPSG_32715 extends CRSMeter[EPSG_32715]

    // WGS84 UTM zone 16S
    case class EPSG_32716() extends CRSDef("EPSG:32716")
    implicit object EPSG_32716 extends CRSMeter[EPSG_32716]

    // WGS84 UTM zone 17S
    case class EPSG_32717() extends CRSDef("EPSG:32717")
    implicit object EPSG_32717 extends CRSMeter[EPSG_32717]

    // WGS84 UTM zone 18S
    case class EPSG_32718() extends CRSDef("EPSG:32718")
    implicit object EPSG_32718 extends CRSMeter[EPSG_32718]

    // WGS84 UTM zone 19S
    case class EPSG_32719() extends CRSDef("EPSG:32719")
    implicit object EPSG_32719 extends CRSMeter[EPSG_32719]

    // WGS84 UTM zone 20S
    case class EPSG_32720() extends CRSDef("EPSG:32720")
    implicit object EPSG_32720 extends CRSMeter[EPSG_32720]

    // WGS84 UTM zone 21S
    case class EPSG_32721() extends CRSDef("EPSG:32721")
    implicit object EPSG_32721 extends CRSMeter[EPSG_32721]

    // WGS84 UTM zone 22S
    case class EPSG_32722() extends CRSDef("EPSG:32722")
    implicit object EPSG_32722 extends CRSMeter[EPSG_32722]

    // WGS84 UTM zone 23S
    case class EPSG_32723() extends CRSDef("EPSG:32723")
    implicit object EPSG_32723 extends CRSMeter[EPSG_32723]

    // WGS84 UTM zone 24S
    case class EPSG_32724() extends CRSDef("EPSG:32724")
    implicit object EPSG_32724 extends CRSMeter[EPSG_32724]

    // WGS84 UTM zone 25S
    case class EPSG_32725() extends CRSDef("EPSG:32725")
    implicit object EPSG_32725 extends CRSMeter[EPSG_32725]

    // WGS84 UTM zone 26S
    case class EPSG_32726() extends CRSDef("EPSG:32726")
    implicit object EPSG_32726 extends CRSMeter[EPSG_32726]

    // WGS84 UTM zone 27S
    case class EPSG_32727() extends CRSDef("EPSG:32727")
    implicit object EPSG_32727 extends CRSMeter[EPSG_32727]

    // WGS84 UTM zone 28S
    case class EPSG_32728() extends CRSDef("EPSG:32728")
    implicit object EPSG_32728 extends CRSMeter[EPSG_32728]

    // WGS84 UTM zone 29S
    case class EPSG_32729() extends CRSDef("EPSG:32729")
    implicit object EPSG_32729 extends CRSMeter[EPSG_32729]

    // WGS84 UTM zone 30S
    case class EPSG_32730() extends CRSDef("EPSG:32730")
    implicit object EPSG_32730 extends CRSMeter[EPSG_32730]

    // WGS84 UTM zone 31S
    case class EPSG_32731() extends CRSDef("EPSG:32731")
    implicit object EPSG_32731 extends CRSMeter[EPSG_32731]

    // WGS84 UTM zone 32S
    case class EPSG_32732() extends CRSDef("EPSG:32732")
    implicit object EPSG_32732 extends CRSMeter[EPSG_32732]

    // WGS84 UTM zone 33S
    case class EPSG_32733() extends CRSDef("EPSG:32733")
    implicit object EPSG_32733 extends CRSMeter[EPSG_32733]

    // WGS84 UTM zone 34S
    case class EPSG_32734() extends CRSDef("EPSG:32734")
    implicit object EPSG_32734 extends CRSMeter[EPSG_32734]

    // WGS84 UTM zone 35S
    case class EPSG_32735() extends CRSDef("EPSG:32735")
    implicit object EPSG_32735 extends CRSMeter[EPSG_32735]

    // WGS84 UTM zone 36S
    case class EPSG_32736() extends CRSDef("EPSG:32736")
    implicit object EPSG_32736 extends CRSMeter[EPSG_32736]

    // WGS84 UTM zone 37S
    case class EPSG_32737() extends CRSDef("EPSG:32737")
    implicit object EPSG_32737 extends CRSMeter[EPSG_32737]

    // WGS84 UTM zone 38S
    case class EPSG_32738() extends CRSDef("EPSG:32738")
    implicit object EPSG_32738 extends CRSMeter[EPSG_32738]

    // WGS84 UTM zone 39S
    case class EPSG_32739() extends CRSDef("EPSG:32739")
    implicit object EPSG_32739 extends CRSMeter[EPSG_32739]

    // WGS84 UTM zone 40S
    case class EPSG_32740() extends CRSDef("EPSG:32740")
    implicit object EPSG_32740 extends CRSMeter[EPSG_32740]

    // WGS84 UTM zone 41S
    case class EPSG_32741() extends CRSDef("EPSG:32741")
    implicit object EPSG_32741 extends CRSMeter[EPSG_32741]

    // WGS84 UTM zone 42S
    case class EPSG_32742() extends CRSDef("EPSG:32742")
    implicit object EPSG_32742 extends CRSMeter[EPSG_32742]

    // WGS84 UTM zone 43S
    case class EPSG_32743() extends CRSDef("EPSG:32743")
    implicit object EPSG_32743 extends CRSMeter[EPSG_32743]

    // WGS84 UTM zone 44S
    case class EPSG_32744() extends CRSDef("EPSG:32744")
    implicit object EPSG_32744 extends CRSMeter[EPSG_32744]

    // WGS84 UTM zone 45S
    case class EPSG_32745() extends CRSDef("EPSG:32745")
    implicit object EPSG_32745 extends CRSMeter[EPSG_32745]

    // WGS84 UTM zone 46S
    case class EPSG_32746() extends CRSDef("EPSG:32746")
    implicit object EPSG_32746 extends CRSMeter[EPSG_32746]

    // WGS84 UTM zone 47S
    case class EPSG_32747() extends CRSDef("EPSG:32747")
    implicit object EPSG_32747 extends CRSMeter[EPSG_32747]

    // WGS84 UTM zone 48S
    case class EPSG_32748() extends CRSDef("EPSG:32748")
    implicit object EPSG_32748 extends CRSMeter[EPSG_32748]

    // WGS84 UTM zone 49S
    case class EPSG_32749() extends CRSDef("EPSG:32749")
    implicit object EPSG_32749 extends CRSMeter[EPSG_32749]

    // WGS84 UTM zone 50S
    case class EPSG_32750() extends CRSDef("EPSG:32750")
    implicit object EPSG_32750 extends CRSMeter[EPSG_32750]

    // WGS84 UTM zone 51S
    case class EPSG_32751() extends CRSDef("EPSG:32751")
    implicit object EPSG_32751 extends CRSMeter[EPSG_32751]

    // WGS84 UTM zone 52S
    case class EPSG_32752() extends CRSDef("EPSG:32752")
    implicit object EPSG_32752 extends CRSMeter[EPSG_32752]

    // WGS84 UTM zone 53S
    case class EPSG_32753() extends CRSDef("EPSG:32753")
    implicit object EPSG_32753 extends CRSMeter[EPSG_32753]

    // WGS84 UTM zone 54S
    case class EPSG_32754() extends CRSDef("EPSG:32754")
    implicit object EPSG_32754 extends CRSMeter[EPSG_32754]

    // WGS84 UTM zone 55S
    case class EPSG_32755() extends CRSDef("EPSG:32755")
    implicit object EPSG_32755 extends CRSMeter[EPSG_32755]

    // WGS84 UTM zone 56S
    case class EPSG_32756() extends CRSDef("EPSG:32756")
    implicit object EPSG_32756 extends CRSMeter[EPSG_32756]

    // WGS84 UTM zone 57S
    case class EPSG_32757() extends CRSDef("EPSG:32757")
    implicit object EPSG_32757 extends CRSMeter[EPSG_32757]

    // WGS84 UTM zone 58S
    case class EPSG_32758() extends CRSDef("EPSG:32758")
    implicit object EPSG_32758 extends CRSMeter[EPSG_32758]

    // WGS84 UTM zone 59S
    case class EPSG_32759() extends CRSDef("EPSG:32759")
    implicit object EPSG_32759 extends CRSMeter[EPSG_32759]

    // WGS84 UTM zone 60S
    case class EPSG_32760() extends CRSDef("EPSG:32760")
    implicit object EPSG_32760 extends CRSMeter[EPSG_32760]

    // MODIS
    // See http://spatialreference.org/ref/sr-org/6974/
    private val modisWKT =
      """
        |PROJCS["MODIS Sinusoidal",
        |    GEOGCS["WGS 84",
        |        DATUM["WGS_1984",
        |            SPHEROID["WGS 84",6378137,298.257223563,
        |                AUTHORITY["EPSG","7030"]],
        |            AUTHORITY["EPSG","6326"]],
        |        PRIMEM["Greenwich",0,
        |            AUTHORITY["EPSG","8901"]],
        |        UNIT["degree",0.01745329251994328,
        |            AUTHORITY["EPSG","9122"]],
        |        AUTHORITY["EPSG","4326"]],
        |    PROJECTION["Sinusoidal"],
        |    PARAMETER["false_easting",0.0],
        |    PARAMETER["false_northing",0.0],
        |    PARAMETER["central_meridian",0.0],
        |    PARAMETER["semi_major",6371007.181],
        |    PARAMETER["semi_minor",6371007.181],
        |    UNIT["m",1.0],
        |    AUTHORITY["SR-ORG","6974"]]
      """.stripMargin

    val MODIS_Sinusoidal_CRS: CoordinateReferenceSystem =
      GCRS.parseWKT(modisWKT)
    case class MODIS_Sinusoidal() extends CRSDef(MODIS_Sinusoidal_CRS)
    implicit object MODIS_Sinusoidal extends CRSMeter[MODIS_Sinusoidal]

    // OSGB36 for Great Britain
    case class EPSG_7405() extends CRSDef("EPSG:7405")
    implicit object EPSG_7405 extends CRSMeter[EPSG_7405]

    object DisplayOnly:
      case class EPSG_3857() extends CRSDef("EPSG:3857")
      implicit object EPSG_3857 extends CRSMeter[EPSG_3857]

    private def crsTuple(companion: CRST[_]): (String, CRST[_]) =
      (companion.crsId, companion)

    // Make this volatile so we don't barf when multiple threads initialize this
    @volatile lazy val _registered: Map[String, CRST[_]] =
      Seq(
        // NAD83
        EPSG_4269,
        // WGS 84 global
        EPSG_4326,
        // Albers Conical Equal Area
        EPSG_5070,
        // NAD83 UTM zones
        EPSG_26901,
        EPSG_26902,
        EPSG_26903,
        EPSG_26904,
        EPSG_26905,
        EPSG_26906,
        EPSG_26907,
        EPSG_26908,
        EPSG_26909,
        EPSG_26910,
        EPSG_26911,
        EPSG_26912,
        EPSG_26913,
        EPSG_26914,
        EPSG_26915,
        EPSG_26916,
        EPSG_26917,
        EPSG_26918,
        EPSG_26919,
        EPSG_26920,
        EPSG_26921,
        EPSG_26922,
        EPSG_26923,
        EPSG_32145,
        // WGS 84 UTM zones (N)
        EPSG_32601,
        EPSG_32602,
        EPSG_32603,
        EPSG_32604,
        EPSG_32605,
        EPSG_32606,
        EPSG_32607,
        // WGS 84 UTM zones for North America
        EPSG_32608,
        EPSG_32609,
        EPSG_32610,
        EPSG_32611,
        EPSG_32612,
        EPSG_32613,
        EPSG_32614,
        EPSG_32615,
        EPSG_32616,
        EPSG_32617,
        EPSG_32618,
        EPSG_32619,
        EPSG_32620,
        EPSG_32621,
        // WGS 84 UTM zones (N)
        EPSG_32622,
        EPSG_32623,
        EPSG_32624,
        EPSG_32625,
        EPSG_32626,
        EPSG_32627,
        EPSG_32628,
        EPSG_32629,
        EPSG_32630,
        EPSG_32631,
        EPSG_32632,
        EPSG_32633,
        EPSG_32634,
        EPSG_32635,
        EPSG_32636,
        EPSG_32637,
        EPSG_32638,
        EPSG_32639,
        EPSG_32640,
        EPSG_32641,
        EPSG_32642,
        EPSG_32643,
        EPSG_32644,
        EPSG_32645,
        EPSG_32646,
        EPSG_32647,
        EPSG_32648,
        EPSG_32649,
        EPSG_32650,
        EPSG_32651,
        EPSG_32652,
        EPSG_32653,
        EPSG_32654,
        EPSG_32655,
        EPSG_32656,
        EPSG_32657,
        EPSG_32658,
        EPSG_32659,
        EPSG_32660,
        // WGS 84 UTM zones (S)
        EPSG_32701,
        EPSG_32702,
        EPSG_32703,
        EPSG_32704,
        EPSG_32705,
        EPSG_32706,
        EPSG_32707,
        EPSG_32708,
        EPSG_32709,
        EPSG_32710,
        EPSG_32711,
        EPSG_32712,
        EPSG_32713,
        EPSG_32714,
        EPSG_32715,
        EPSG_32716,
        EPSG_32717,
        EPSG_32718,
        EPSG_32719,
        EPSG_32720,
        EPSG_32721,
        EPSG_32722,
        EPSG_32723,
        EPSG_32724,
        EPSG_32725,
        EPSG_32726,
        EPSG_32727,
        EPSG_32728,
        EPSG_32729,
        EPSG_32730,
        EPSG_32731,
        EPSG_32732,
        EPSG_32733,
        EPSG_32734,
        EPSG_32735,
        EPSG_32736,
        EPSG_32737,
        EPSG_32738,
        EPSG_32739,
        EPSG_32740,
        EPSG_32741,
        EPSG_32742,
        EPSG_32743,
        EPSG_32744,
        EPSG_32745,
        EPSG_32746,
        EPSG_32747,
        EPSG_32748,
        EPSG_32749,
        EPSG_32750,
        EPSG_32751,
        EPSG_32752,
        EPSG_32753,
        EPSG_32754,
        EPSG_32755,
        EPSG_32756,
        EPSG_32757,
        EPSG_32758,
        EPSG_32759,
        EPSG_32760,
        // OSGB 36 :-)
        EPSG_7405
      ).map(c => crsTuple(c)).toMap

    def apply(): Seq[String] = _registered.keys.toVector

  def crsFromId(id: String): SomeCRS =
    optCrsFromId(id).getOrElse(
      throw new IllegalArgumentException(s"CRS $id has not been registered.")
    )

  def optCrsFromId(id: String): Option[SomeCRS] =
    for ccrs <- CRSDefinitions._registered.get(id) yield
      new SomeCRS:
        type SOME_CRS = ccrs.crs.type
        val SOME_CRS: CRST[ccrs.crs.type] = ccrs.asInstanceOf[CRST[SOME_CRS]]

  def crsFromRuntime(ccrs: CRST[_]): SomeCRS =
    new SomeCRS:
      type SOME_CRS = ccrs.crs.type
      val SOME_CRS: CRST[SOME_CRS] = ccrs.asInstanceOf[CRST[SOME_CRS]]

  def crsFromWKT(wkt: String): SomeCRS =
    crsFromGeotools(GCRS.parseWKT(wkt))

  def crsFromGeotools(gcrs: CoordinateReferenceSystem): SomeCRS =
    val crsDefn = new CRSDef(gcrs) {}

    new SomeCRS:
      type SOME_CRS = gcrs.type
      val SOME_CRS: CRST[SOME_CRS] = new CRST[SOME_CRS]:
        def apply(): CRSDef = crsDefn

  def crsFromTiff(tiffFile: File): SomeCRS =
    val reader: GeoTiffReader = new GeoTiffReader(tiffFile)
    try
      val gc2d = reader.read(null)
      val coordRefSys: CoordinateReferenceSystem =
        gc2d.getCoordinateReferenceSystem2D
      val matched = for
        coordref <- coordRefSys.getIdentifiers.toArray.headOption
        matchedCRS <- CRST.optCrsFromId(coordref.toString)
      yield matchedCRS

      matched.getOrElse(crsFromGeotools(coordRefSys))
    finally Try(reader.dispose())


  def epsgUtmCodeForZoneNumber(zoneNumber: Int): String =
    f"EPSG:326$zoneNumber%02d"

  val nhPattern: Regex = """(\d{2})N""".r
  val shPattern: Regex = """(\d{2})S""".r
  val numPattern: Regex = """(\d{2})""".r
  def epsgUtmCodeForZoneNumber(zoneNumber: String): String = zoneNumber match
    case nhPattern(zn)  => s"EPSG:326$zn"
    case shPattern(zn)  => s"EPSG:327$zn"
    case numPattern(zn) => s"EPSG:326$zn"
    case _ =>
      throw new IllegalArgumentException(s"Bad UTM Zone String $zoneNumber")

  // Grid Zone Designators are defined as part of MGRS:
  // https://en.wikipedia.org/wiki/Military_Grid_Reference_System
  def epsgUtmCodeForGZD(gridZoneDesignator: String): String =
    val gzdSouth: Regex = """^(\d{2})[CDEFGHHKLM]$""".r // intentionally excludes I
    val gzdNorth: Regex = """^(\d{2})[NPQRSTUVWX]$""".r // intentionally excludes O

    gridZoneDesignator match
      case gzdNorth(zn) => s"EPSG:326$zn"
      case gzdSouth(zn) => s"EPSG:327$zn"
      case _ =>
        throw new IllegalArgumentException(
          s"Bad or unsupported Grid Zone Designator $gridZoneDesignator"
        )

  def listKnownCRSs: Seq[String] =
    CRSDefinitions._registered.keys.toSeq.sorted
