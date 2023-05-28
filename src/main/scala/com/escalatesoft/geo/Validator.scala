package com.escalatesoft.geo

import org.locationtech.jts.operation.valid.TopologyValidationError

/**
  * A validation mixin for geometries. If present, the validated method
  * when called on a geometry will either return the original geometry
  * or a JTS TopologyValidationError in a union type.
  * @tparam T the geometry type on which to provide the validated behavior
  * @return either the original geometry or a JTS TopologyValidationError
  */
trait Validator[T]:
  def validated: TopologyValidationError | T
