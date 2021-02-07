package org.bu.abel;

import org.apache.commons.math3.complex.Complex;

public class FFT {

    public static int precision = 0;

    private static Complex[] fft(Complex[] a, int level) {
        int n = a.length;

        if (n == 1)
            return a;

        Complex e = new Complex(Math.E);
        Complex principal = e.pow(Complex.I.multiply(2*Math.PI/n)).conjugate();
        principal = Tools.round(principal, 10);
        Complex omega = new Complex(1);

        int halfUp = (int)Math.ceil((double)n/2);
        int halfDown = n/2;

        Complex[] a0 = new Complex[halfUp];
        Complex[] a1 = new Complex[halfDown];
        for (int i = 0; i < a0.length; i++)
            a0[i] = a[i*2];
        for (int i = 0; i < a1.length; i++)
            a1[i] = a[i*2 + 1];

        // recursive calls
        Complex[] y0 = fft(a0, level + 1);
        Complex[] y1 = fft(a1, level + 1);

        // combine y0 and y1 back into a single vector
        Complex[] y = new Complex[n];
        for (int k = 0; k < halfDown; k++) {
            omega = Tools.round(omega, 5);

            y[k] 			= y0[k].add(omega.multiply(y1[k]));
            y[k + halfDown] = y0[k].subtract(omega.multiply(y1[k]));

            omega = omega.multiply(principal);
        }

        // y is assumed to be a column vector
        return y;
    }

    private static Complex[] transform(Complex[] vector, boolean inverse) {
        if (inverse)
            for (int i = 0; i < vector.length; i++)
                vector[i] = vector[i].conjugate();
        vector = fft(vector, 0);
        if (inverse) {
            for (int i = 0; i < vector.length; i++) {
                vector[i] = vector[i].conjugate();
                vector[i] = vector[i].divide(vector.length);
            }
        }
        return vector;
    }

    public static Complex[] transform(double[] vector) {
        return transform(Tools.toComplex(vector), false);
    }

    public static double[] inverse(Complex[] vector) {
        return Tools.toDouble(transform(vector, true), precision);
    }

}