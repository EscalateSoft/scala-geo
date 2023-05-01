package com.escalatesoft.geo

import scala.annotation.targetName

object Units:
  opaque type Meters = Double
  object Meters:
    def apply(d: Double): Meters = d

  opaque type Kilometers = Double
  object Kilometers:
    def apply(d: Double): Kilometers = d

  extension (meters: Meters)
    def +(other: Meters): Meters = meters + other
    def -(other: Meters): Meters = meters - other
    @targetName("multSqM") def *(mult: Double): Meters = meters * mult
    def *(other: Meters): SquareMeters = meters * other
    def /(div: Double): Meters = meters / div
    def toMeters: Double = meters
    def toKilometers: Double = meters / 1000.0

  def metersToKilometers(meters: Meters): Kilometers = meters / 1000.0

  extension (kilometers: Kilometers)
    @targetName("km+") def +(other: Kilometers): Kilometers = kilometers + other
    @targetName("km-") def -(other: Kilometers): Kilometers = kilometers - other
    @targetName("km*") def *(mult: Double): Kilometers = kilometers * mult
    @targetName("km/") def /(div: Double): Kilometers = kilometers / div
    @targetName("kmToM") def toMeters: Double = kilometers * 1000.0
    @targetName("kmToKm") def toKilometers: Double = kilometers

  opaque type SquareMeters = Double
  object SquareMeters:
    def apply(d: Double): SquareMeters = d

  
