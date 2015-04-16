/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef COMPLEX2DX_HPP
#define	COMPLEX2DX_HPP

#include <iostream>
#include <complex>

namespace volume
{
    namespace data
    {
        /**
         * A class used to store complex data in 2dx.
         * Internally stores data in real and imaginary double values.
         * Also provides handy methods to directly set/get amplitudes and
         * phases.
         */
        class Complex2dx
        {
        public:
            
            /**
             * Default constructor which initializes to 0 + 0i 
             */
            Complex2dx();
            
            /**
             * Constructor with real and imag part
             * @param real
             * @param imag
             */
            Complex2dx(double real, double imag);
            
            /**
             * Copy constructor
             * @param copy: to be copied from!
             */
            Complex2dx(const Complex2dx& copy);
            
            /**
             * Operator overloading of = operator.
             * Equates the real and imag part of rhs to this object
             * @param rhs
             * @return equated instance
             */
            Complex2dx& operator=(const Complex2dx& rhs);
            
            /**
             * Operator overloading of + operator.
             * Adds up the real and imag part of this object and rhs.
             * @param rhs
             * @return modified this object
             */
            Complex2dx operator+(const Complex2dx& rhs);
            
            /**
             * Declaration of multiplication of a double with complex2dx
             * @param factor
             * @return 
             */
            Complex2dx operator*( double factor);
            
            /**
             * Operator overloading of < operator
             * Amplitudes (aka abs) is used to check the condition.
             * @param rhs
             * @return result of this < rhs? 
             */
            bool operator<(const Complex2dx& rhs) const;
            
            /**
             * Operator overloading of ==
             * Checks for the equality of both real and imag part
             * @param rhs
             * @return result of this == rhs
             */
            bool operator==(const Complex2dx& rhs) const;
            
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
        
    } // namespace data_structures
    
} // namespace volume_processing_2dx

#endif	/* COMPLEX2DX_HPP */

