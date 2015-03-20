/* 
 * File:   Complex2dx.hpp
 * Author: Nikhil Biyani (nikhilbiyani@gmail.com)
 *
 * Created on March 3, 2015, 11:52 AM
 */

#ifndef COMPLEX2DX_HPP
#define	COMPLEX2DX_HPP

#include <iostream>
#include <fftw3.h>
#include <stdio.h>
#include <string.h>
#include <complex>

#include "NumericalUtils.hpp"

/*
 * A class used to store complex numbers
 * Implemented as subclass of std::complex<double>
 */
class Complex2dx{
public:
    /*===================
     * Constructors     *
     ===================*/
    /*
     * Default Constructor
     */
    Complex2dx();
    
    /*
     * Constructor with real and imag values
     */
    Complex2dx(double real, double imag);
    
    /*
     * Constructor with fftw_complex
     */
    Complex2dx(fftw_complex* value);
    
    /*===================
     * Operators        *
     ===================*/
    
    /*
     * Operator overloading of =
     */
    Complex2dx& operator=(const Complex2dx& rhs);
    
    /*
     * Operator overloading of +
     */
    Complex2dx operator+(const Complex2dx&);
    
    /*
     * Multiplication of complex with a factor
     */
    Complex2dx operator*( double factor);
    
    /*
     * Operator overloading of < for sorting purposes 
     */
    bool operator<(const Complex2dx&) const;
    
    /*
     * Operator overloading of ==
     */
    bool operator==(const Complex2dx&) const;
    
    /*
     * Operator overloading of << for output purposes
     */
    std::ostream& operator<<(std::ostream& os);
    
    /*=====================
     * Additional Methods *
     =====================*/
    
    /*
     * Returns the amplitude of the complex number
     */
    double getAmplitude() const;
    
    /*
     * Returns the phase of the complex number
     */
    double getPhase() const;
    
    /*
     * Changes the amplitude of the complex
     */
    void setAmplitude(double amplitude);
    
    /*
     * Changes the phase (in radians) of the complex 
     */
    void setPhase(double phase_in_radians);
    
    /*
     * Converts the complex to one used in fftw libraries
     */
    fftw_complex* to_fftw_format();
   
    
    /*=======================
     * Getters and Setters
     =======================*/
    
    double real() const {return _real;};
    double imag() const {return _imag;};
    
    void real(double real) {_real = real;};
    void imag(double imag) {_imag = imag;};
    
private:
    
    void initialize(double real, double imag);
    
    /*=======================
     * Members
     =======================*/
    double _real;
    double _imag;
    
};

#endif	/* COMPLEX2DX_HPP */

