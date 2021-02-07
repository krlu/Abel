package org.bu.abel;

import org.apache.commons.math3.complex.Complex;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Collections;

public class Multiplication {

    public static double[] fftMultiply(double[] a, double[] b) {
        // find the smallest power of two that will contain the resulting vector
        int n = a.length + b.length - 1;
        int exp = 1;
        while (Math.pow(2, exp) < n)
            exp++;
        int length = (int) Math.pow(2, exp);
        a = pad(a, length);
        b = pad(b, length);
//        System.out.println(Arrays.toString(a));
//        System.out.println(Arrays.toString(b));

        double[] c = new double[length];
        Complex[] aPrime = FFT.transform(a);
        Complex[] bPrime = FFT.transform(b);

        // pairwise multiplication
        Complex[] cPrime = new Complex[length];
        for (int i = 0; i < length; i++)
            cPrime[i] = aPrime[i].multiply(bPrime[i]);
//        System.out.println(Arrays.toString(cPrime));
        c = FFT.inverse(cPrime);
//        System.out.println(Arrays.toString(c));
        c = removeExtra(c, n);

        return c;
    }

    private static double[] pad(double[] vector, int size) {
        double[] padded = new double[size];
        for (int i = 0; i < size; i++) {
            if (i < vector.length)
                padded[i] = vector[i];
            else
                padded[i] = 0.0;
        }
        return padded;
    }

    private static double[] removeExtra(double[] vector, int size) {
        double[] newVector = new double[size];
        for (int i = 0; i < vector.length; i++)
            if (i < size)
                newVector[i] = vector[i];
        return newVector;
    }

    public static int[] fftMultiply(int[] a, int[] b) {
        return Tools.toInteger(fftMultiply(Tools.toDouble(a), Tools.toDouble(b)));
    }

    public static double[] standardMultiply(double[] a, double[] b) {
        double[] c = new double[a.length + b.length - 1];
        for (int k = 0; k < c.length; k++) {
            for (int i = 0; i <= k; i++) {
                if (i < a.length && k - i < b.length) {
                    c[k] += a[i]*b[k-i];
                }
            }
        }
        return c;
    }

    public static int[] standardMultiply(int[] a, int[] b) {
        return Tools.toInteger(standardMultiply(Tools.toDouble(a), Tools.toDouble(b)));
    }

}