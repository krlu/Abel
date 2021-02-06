package org.bu.abel.factorization.polynomial

import org.bu.abel.types.polynomials.RealPolynomial

trait PolynomialFactorizationAlgo extends (RealPolynomial => Seq[RealPolynomial])
