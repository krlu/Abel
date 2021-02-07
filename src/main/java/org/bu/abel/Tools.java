package org.bu.abel;

import org.apache.commons.math3.complex.Complex;

public class Tools {

    public static Complex round(Complex value, int decimal) {
        double real = value.getReal();
        double imaginary = value.getImaginary();
        return new Complex(round(real, decimal), round(imaginary, decimal));
    }

    public static double round(double value, int decimal) {
        int factor = (int) Math.pow(10, decimal);
        return (double)Math.round(value * factor) / factor;
    }

    public static Complex[] toComplex(double[] vector) {
        Complex[] complex = new Complex[vector.length];
        for (int i = 0; i < vector.length; i++)
            complex[i] = new Complex(vector[i]);
        return complex;
    }

    public static double[] toDouble(Complex[] vector, int precision) {
        double[] primitive = new double[vector.length];
        for (int i = 0; i < vector.length; i++) {
            if (precision > 0)
                primitive[i] = round(vector[i].getReal(), precision);
            else
                primitive[i] = vector[i].getReal();
        }
        return primitive;
    }

    public static double[] toDouble(int[] vector, int precision) {
        double[] primitive = new double[vector.length];
        for (int i = 0; i < vector.length; i++) {
            if (precision > 0)
                primitive[i] = round(vector[i], precision);
            else
                primitive[i] = vector[i];
        }
        return primitive;
    }

    public static double[] toDouble(int[] vector) {
        return toDouble(vector, 0);
    }

    public static int[] toInteger(double[] vector) {
        int[] primitive = new int[vector.length];
        for (int i = 0; i < vector.length; i++)
            primitive[i] = (int) (vector[i] + 0.5);
        return primitive;
    }

}