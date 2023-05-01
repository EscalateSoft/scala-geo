package com.escalatesoft.geo

import org.locationtech.jts.operation.valid.TopologyValidationError

/**
  * A validation mixin for geometries. If present, the validated method
  * when called on a geometry will either return the original geometry in
  * a Right, or a JTS TopologyValidationError in a Left.
  * @tparam T the geometry type on which to provide the validated behavior
  */
trait Validator[T]:
  def validated: Either[TopologyValidationError, T]
