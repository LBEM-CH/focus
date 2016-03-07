/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef COMPLEX2DX_HPP
#define	COMPLEX2DX_HPP

#include <iostream>
#include <complex>

namespace tdx
{

    /**
     * A class used to store complex data in 2dx.
     * Internally stores data in real and imaginary double values.
     * Also provides handy methods to directly set/get amplitudes and
     * phases.
     */
    class Complex
    {
    public:

        /**
         * Default constructor which initializes to 0 + 0i 
         */
        Complex();

        /**
         * Constructor with real and imag part
         * @param real
         * @param imag
         */
        Complex(double real, double imag);

        /**
         * Operator overloading of = operator.
         * Equates the real and imag part of rhs to this object
         * @param rhs
         * @return equated instance
         */
        Complex& operator=(const Complex& rhs);

        /**
         * Operator overloading of + operator.
         * Adds up the real and imag part of this object and rhs.
         * @param rhs
         * @return modified this object
         */
        Complex operator+(const Complex& rhs);

        /**
         * Declaration of multiplication of a double with complex2dx
         * @param factor
         * @return 
         */
        Complex operator*(double factor);

        /**
         * Declaration of multiplication of a Complex with another Complex
         * @param another complex
         * @return multiplied complex
         */
        Complex operator*(const Complex& other);

        /**
         * Operator overloading of < operator
         * Amplitudes (aka abs) is used to check the condition.
         * @param rhs
         * @return result of this < rhs? 
         */
        bool operator<(const Complex& rhs) const;

        /**
         * Operator overloading of ==
         * Checks for the equality of both real and imag part
         * @param rhs
         * @return result of this == rhs
         */
        bool operator==(const Complex& rhs) const;

        /**
         * Returns the real part of the complex
         * @return real
         */
        double real() const;

        /**
         * Returns the imag part of the complex
         * @return 
         */
        double imag() const;

        /**
         * Evaluates and returns the amplitude of the complex
         * @return amplitude
         */
        double amplitude() const;

        /**
         * Evaluates and returns the phase of the complex
         * @return phase in radians
         */
        double phase() const;

        /**
         * Evaluates the intensity of the complex
         * @return intensity
         */
        double intensity() const;

        /**
         * Changes/Sets the real part of the complex
         * @param real
         */
        void set_real(double real);

        /**
         * Changes/Sets the imaginary part of the complex
         * @param imag
         */
        void set_imag(double imag);

        /**
         * Changes the amplitude of the complex with the given value
         * @param amplitude
         */
        void set_amplitude(double amplitude);

        /**
         * Changes the phase of the complex with the given value
         * @param phase in radians
         */
        void set_phase(double phase);

        /**
         * Returns the conjugate of the current complex
         * @return Complex conjugate
         */
        Complex conjugate();


    private:

        /**
         * Member initializer function with real and imag values
         * @param real
         * @param imag
         */
        void initialize(double real, double imag);

        /**
         * Real part of the complex number
         */
        double _real;

        /**
         * Imaginary part of the complex number
         */
        double _imag;



    }; // class Complex2dx

} // namespace volume_processing_2dx

#endif	/* COMPLEX2DX_HPP */

