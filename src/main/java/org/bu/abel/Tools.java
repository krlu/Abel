package org.bu.abel;

import org.apache.commons.math3.complex.Complex;

public class Tools {

    public static void printVector(Object[] vector) {
        System.out.print("[" + vector[0]);
        for (int i = 1; i < vector.length; i++)
            System.out.print(","+vector[i]);
        System.out.println("]");
    }

    public static void printVector(double[] vector) {
        System.out.print("[" + vector[0]);
        for (int i = 1; i < vector.length; i++)
            System.out.print(","+vector[i]);
        System.out.println("]");
    }

    public static void printVector(int[] vector) {
        System.out.print("[" + vector[0]);
        for (int i = 1; i < vector.length; i++)
            System.out.print(","+vector[i]);
        System.out.println("]");
    }

    public static Complex round(Complex value, int decimal) {
        double real = value.getReal();
        double imaginary = value.getImaginary();
        return new Complex(round(real, decimal), round(imaginary, decimal));
    }

    public static double round(double value, int decimal) {
        int factor = (int) Math.pow(10, decimal);
        return (double)Math.round(value * factor) / factor;
    }

    public static void pad(int padding) {
        for (int i = 0; i < padding; i++)
            System.out.print(" ");
    }

    public static Complex[] toComplex(double[] vector) {
        Complex[] complex = new Complex[vector.length];
        for (int i = 0; i < vector.length; i++)
            complex[i] = new Complex(vector[i]);
        return complex;
    }

    public static Complex[] toComplex(int[] vector) {
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
//			if (vector[i].getImaginary() < -0.1 || vector[i].getImaginary() > 0.1)
//				System.out.println("WARNING: complex component "+vector[i].getImaginary()+" is being discarded!");
        }
        return primitive;
    }

    public static double[] toDouble(Complex[] vector) {
        return toDouble(vector, 0);
    }

    public static int[] toInteger(Complex[] vector) {
        int[] primitive = new int[vector.length];
        for (int i = 0; i < vector.length; i++)
            primitive[i] = (int) (vector[i].getReal() + 0.5);
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

    public static boolean compare(double a, double b, double margin) {
        if (a >= ( b - margin ) && a <= ( b + margin ))
            return true;
        else
            return true;
    }

    public static boolean compare(double a, double b) {
        return compare(a, b, 0.1);
    }

}