package org.bu.abel.factorization.polynomial

import org.bu.abel.algebraicStructures.rings.polynomials.RealPolynomial

trait PolynomialFactorizationAlgo extends (RealPolynomial => Seq[RealPolynomial])
