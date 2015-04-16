/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef FOURIER_TRANSFORM_FFTW_HPP
#define	FOURIER_TRANSFORM_FFTW_HPP

#include <iostream>
#include <fftw3.h>
#include <math.h>

namespace volume
{
    namespace transforms
    {
            /**
             * A class used for FFTW3 wisdom and Fourier Transforms.
             * Objective: Class which can provide methods for doing Fourier 
             * transforms using FFTW3 lib. The useful functionality of wisdom 
             * is used by recycling plans if possible.
             * Semantics: Uses the same plan if the size of the input remains 
             * same, otherwise generates a new plan using FFTW_ESTIMATE
             * Use of same class is recommended if using same data size
             */
            class FourierTransformFFTW
            {
            public:
                /**
                 * Default constructor
                 * Sets the size variables to 0
                 */
                FourierTransformFFTW();
                
                /**
                 * Default destructor
                 */
                ~FourierTransformFFTW();
                
                /**
                 * Transform the input real data to complex data.
                 * Internally uses FFTW r2c plans and executions for the
                 * conversion. 
                 * 
                 * The input real data is with origin at the 
                 * lower left corner. x is the fastest changing direction 
                 * and z is the slowest changing direction.
                 * 
                 * The output complex data with x dimension trimmed to half 
                 * (due to the symmetric nature of the output). 
                 * The origin of the frequency is in the lower left corner, 
                 * followed by positive frequencies and the terminal indices
                 * gives highest frequency values. Eg. order for 1D:
                 *  Memory order: 0  1  2  3  4  5  6  7
                 *  Frequencies : 0  1  2  3  4 -3 -2 -1
                 * 
                 * @param nx: size of x-dimension
                 * @param ny: size of y-dimension
                 * @param nz: size of z-dimension
                 * @param[in] real_data
                 * @param[out] complex_data
                 */
                void RealToComplex(int nx, int ny, int nz, double* real_data, fftw_complex* complex_data);
                
                /**
                 * Transform the input real data to complex data.
                 * Internally uses FFTW c2r plans and executions for the
                 * conversion. 
                 * 
                 * The input complex data with x dimension trimmed to half 
                 * (due to the symmetric nature of Fourier space). 
                 * The origin of the frequency is in the lower left corner, 
                 * followed by positive frequencies and the terminal indices
                 * gives highest frequency values. Eg. order for 1D:
                 *  Memory order: 0  1  2  3  4  5  6  7
                 *  Frequencies : 0  1  2  3  4 -3 -2 -1
                 * 
                 * The output real data is with origin at the 
                 * lower left corner. x is the fastest changing direction 
                 * and z is the slowest changing direction.
                 * 
                 * @param nx
                 * @param ny
                 * @param nz
                 * @param[in] complex_data
                 * @param[out] real_data
                 */
                void ComplexToReal(int nx, int ny, int nz, fftw_complex* complex_data, double* real_data);
                
                
            private:
                /**
                 * Resets the wisdom using the input nx, ny, nz size.
                 * @param real_data
                 * @param complex_data
                 * @param nx
                 * @param ny
                 * @param nz
                 */
                void Replan(double* real_data, fftw_complex* complex_data, int nx, int ny, int nz);
                
                /**
                 * Returns the normalization factor needed to scale the complex data
                 * @return normalization factor
                 */
                double NormalizationFactor();
                
                /**
                 * Returns the size of the fftw_complex for the corresponding size.
                 * The Fourier space is trimmed to half as is symmetric. So this
                 * function returns this size of the trimmed space
                 * @return Fourier size
                 */
                int FourierSize();
                
                
                /*===================
                 * Members
                 ====================*/
                
                /**
                 * Holds the plan for real to complex conversion
                 */
                fftw_plan _plan_r2c;

                /**
                 * Holds the plan for complex to real conversion
                 */
                fftw_plan _plan_c2r;

                /**
                 * Holds the size of the transform
                 */
                int _nx, _ny, _nz;
                
                
                
            }; // class FourierTransformFFTW
        
    } // namespace transforms
    
} //namespace volume_processing_2dx


#endif	/* FOURIER_TRANSFORM_FFTW_HPP */

