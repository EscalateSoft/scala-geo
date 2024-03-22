scalaVersion := "3.3.3"

name := "scala-geo"
organization := "com.escalatesoft"
version := "0.1"

val geotoolsVersion = "27.1"
val jtsVersion = "1.19.0"
val apacheCommonsIoVersion = "2.11.0"
val geohashVersion = "0.8.0"
val jaiCoreVersion = "1.1.3"
val jaiExtVersion = "1.1.24"
val imageIOExtVersion = "1.3.2"
val scalaLoggingVersion = "3.9.4"
val logbackVersion = "1.2.10"


coverageExcludedFiles := "^.*src_managed.*$;^docs/.*$;^.*\\.md$;^.*\\.yml$"
coverageExcludedPackages := "<empty>;.*\\.generators\\..*;.*\\.models\\..*"
coverageMinimumStmtTotal := 55
coverageFailOnMinimum := true
coverageOutputCobertura := true
coverageOutputXML := true

resolvers ++= Seq(
  "JBoss" at "https://repository.jboss.org/maven2",
  "OSGeo" at "https://download.osgeo.org/webdav/geotools/",
  "OpenGeo" at "https://repo.osgeo.org/repository/release/",
  "Typesafe" at "https://repo.typesafe.com/typesafe/releases/",
  "Geotools" at "https://repo.osgeo.org/repository/geotools-releases/",
  "Geotoolkit" at "https://maven.geotoolkit.org",
  "Boundless" at "https://repo.osgeo.org/repository/Geoserver-releases/",
  "Atlassian Releases" at "https://maven.atlassian.com/public/",
  "Atlassian Maven" at "https://maven.atlassian.com/content/repositories/atlassian-public/",
  "mvnrepository" at "https://mvnrepository.com/artifact",
  "MavenRepo1" at "https://repo1.maven.org/maven2",
  Resolver.jcenterRepo
)

libraryDependencies ++= Seq(
  // this hardcoded path for jai_core is because the maven central jai is STILL missing the jar file
  "javax.media" % "jai_core"     % "1.1.3" from "https://repository.jboss.org/maven2/javax/media/jai-core/1.1.3/jai-core-1.1.3.jar",
  "javax.media" % "jai_codec"    % "1.1.3",
  "javax.media" % "jai_imageio"  % "1.1",
  // Guava
  "com.google.guava" % "guava" % "31.1-jre",
  // Apache
  "commons-io" % "commons-io" % apacheCommonsIoVersion,
  // GeoHash goodness
  "com.github.davidmoten" % "geo" % geohashVersion,
  // Geotools
  "org.geotools" % "geotools" % geotoolsVersion,
  "org.geotools" % "gt-coverage" % geotoolsVersion,
  "org.geotools" % "gt-epsg-hsql" % geotoolsVersion,
  "org.geotools" % "gt-main" % geotoolsVersion,
  "org.geotools" % "gt-referencing" % geotoolsVersion,
  "org.geotools" % "gt-geotiff" % geotoolsVersion,
  "org.geotools" % "gt-shapefile" % geotoolsVersion,
  "org.geotools" % "gt-metadata" % geotoolsVersion,
  "org.geotools" % "gt-opengis" % geotoolsVersion,
  "org.geotools" % "gt-cql" % geotoolsVersion,
  "org.geotools" % "gt-image" % geotoolsVersion,
  "org.geotools.jdbc" % "gt-jdbc-postgis" % geotoolsVersion,
  // JTS
  "org.locationtech.jts" % "jts-core" % jtsVersion,
  // JAI extensions
  //"org.jaitools" % "jt-vectorize" % jaiToolsVersion,
  // "javax.media" % "jai_imageio" % "1.1.1",
  // "javax.media" % "jai_core" % jaiCoreVersion,
  "it.geosolutions.imageio-ext" % "imageio-ext-utilities" % "1.4.7",
  //"it.geosolutions.jaiext" % "jaiext" % jaiExtVersion pomOnly(),
  //"com.github.jai-imageio" % "jai-imageio-core" % jaiToolsVersion,
  //"it.geosolutions.imageio-ext" % "imageio-ext" % imageIOExtVersion,
  //"it.geosolutions.imageio-ext" % "imageio-ext-tiff" % imageIOExtVersion,
  // logging
  "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
  "ch.qos.logback" % "logback-classic" % logbackVersion,
  // JSON
  "com.typesafe.play" %% "play-json" % "2.10.0-RC7",
  // Scalactic and scalatest
  "org.scalactic" %% "scalactic" % "3.2.15",
  "org.scalatest" %% "scalatest" % "3.2.15" % "test",
  "org.scalatestplus" %% "scalacheck-1-17" % "3.2.15.0" % "test",
  // Javax measure
  "javax.measure" % "unit-api" % "2.1.3",
)

scalacOptions ++= Seq("-deprecation")
// scalacOptions ++= Seq("-new-syntax", "-rewrite")
// scalacOptions ++= Seq("-indent", "-rewrite")
//scalacOptions ++= Seq("-new-syntax")
