#ifndef FFTCALCULATOR_HPP_PTT4NBZ6
#define FFTCALCULATOR_HPP_PTT4NBZ6

namespace SingleParticle2dx
{
	namespace Utilities
	{
		class FFTCalculator;
	}
}

#include <fftw3.h>
//#include "fftw3_mkl.h"

#include "../Typedefs.hpp"

namespace SingleParticle2dx
{
	
	namespace Utilities
	{
		
		/**
		 *  @brief     SingleParticle2dx Utilities class 
		 *  @details   Contains static functions to perform forward and backward Fourier transformations
		 *             for 2d and 3d datasets. 
		 *  @author    Sebastian Scherer
		 *  @version   0.1
		 *  @date      2012
		 *  @note      All Fourier transformations are normalized and the low frequency components are 
		               placed in the middle of the transformation. In general we guaranty that 
		               a = IFFT(FFT(a))
		 *  @note      Two different behaviors of the 3d version of the functions
		 *  @warning   Uses FFTW-plans with fixed size dependent on the global configurations of the 
		               single particle reconstruction
		 *  @copyright GNU Public License
		 */
		class FFTCalculator
		{
			
		public:

			/** Size type */
			typedef SingleParticle2dx::size_type size_type;


			/** Value type */
			typedef SingleParticle2dx::value_type value_type;


			/** Complex number type */
			typedef SingleParticle2dx::fft_type fft_type;


			/** 2d real space array type */
			typedef SingleParticle2dx::real_array2d_type real_array2d_type;


			/** 2d Fourier space array type */
			typedef SingleParticle2dx::fft_array2d_type fft_array2d_type;


			/** 3d real space array type */
			typedef SingleParticle2dx::real_array3d_type real_array3d_type;


			/** 3d Fourier space array type */
			typedef SingleParticle2dx::fft_array3d_type fft_array3d_type;


			/**
			 *  @brief      Performs a backward Fourier transformation in 2d
			 *  @details    The function allocates input and output FFTW-arrays, fills them and performs
			                the inverse 2d Fourier transformation by means of FFTW3. Afterward the 
			                result is written to real_data and all allocated FFTW-arrays are freed. 
			 *  @param[in]  fourier_data Pointer to the Fourier space input data
			 *  @param[out] real_data Pointer to the real space output data
			 *  @post       real_data = IFFT(fourier_data)
			 *  @note       FFTW plan loaded from FFTPlanContainer
			 *  @warning    Make sure that the array dimensions match the global configuration of the 
			                reconstruction.
			 */
			static void performBackwardFFT (fft_array2d_type* fourier_data, real_array2d_type* real_data);


			/**
			 *  @brief      Performs a forward Fourier transformation in 2d
			 *  @details    The function allocates input and output FFTW-arrays, fills them and performs
			                the forward 2d Fourier transformation by means of FFTW3. Afterward the 
			                result is written to fourier_data and all allocated FFTW-arrays are freed. 
			 *  @param[in]  real_data Pointer to the real space input data
			 *  @param[out] fourier_data Pointer to the Fourier space output data
			 *  @post       fourier_data = FFT(real_data)
			 *  @note       FFTW plan loaded from FFTPlanContainer
			 *  @warning    Make sure that the array dimensions match the global configuration of the 
			                reconstruction.
			 */
			static void performForwardFFT (real_array2d_type* real_data, fft_array2d_type* fourier_data);


			/**
			 *  @brief      Performs a backward Fourier transformation in 3d
			 *  @details    The present function supports two different array sizes, i.g. nx and 
			                factor*nx. Dependent on the size of the passed array the function loads the 
			                appropriate FFTW-plan from the FFTPlanContainer and executes the plan.
			                The function allocates input and output FFTW-arrays, fills them and performs
			                the inverse 3d Fourier transformation by means of FFTW3. Afterward the 
			                result is written to real_data and all allocated FFTW-arrays are freed. 
			 *  @param[in]  fourier_data Pointer to the Fourier space input data
			 *  @param[out] real_data Pointer to the real space output data
			 *  @post       real_data = IFFT(fourier_data)
			 *  @note       FFTW plan loaded from FFTPlanContainer
			 *  @warning    Make sure that the array dimensions match the global configuration of the 
			                reconstruction.
			 */
			static void performBackwardFFT (fft_array3d_type* fourier_data, real_array3d_type* real_data);


			/**
			 *  @brief      Performs a forward Fourier transformation in 3d
			 *  @details    The present function supports two different array sizes, i.g. nx and 
			                factor*nx. Dependent on the size of the passed array the function loads the 
			                appropriate FFTW-plan from the FFTPlanContainer and executes the plan.
			                The function allocates input and output FFTW-arrays, fills them and performs
			                the forward 3d Fourier transformation by means of FFTW3. Afterward the 
			                result is written to fourier_data and all allocated FFTW-arrays are freed. 
			 *  @param[in]  real_data Pointer to the real space input data
			 *  @param[out] fourier_data Pointer to the Fourier space output data
			 *  @post       fourier_data = FFT(real_data)
			 *  @note       FFTW plan loaded from FFTPlanContainer
			 *  @warning    Make sure that the array dimensions match the global configuration of the 
			                reconstruction.
			 */
			static void performForwardFFT (real_array3d_type* real_data, fft_array3d_type* fourier_data);
			
			/**
			 *  @brief      Calculates the normalization factor for the 2d Fourier transformation
			 *  @details    In order to fulfill a = IFFT(FFT(a)) we have to normalize the forward and
			                backward Fourier transformation as FFTW3's convention guaranties 
			                a = N*IFFT(FFT(a)), where N is the number of elements of a. This function 
			                calculates the normalization factor which equals the square root of N.
			 *  @param[in]  nx Size of the array in x-direction
			 *  @param[in]  ny Size of the array in y-direction
			 *  @return     Normalization factor
			 */
			static value_type getScalingFactor2d (size_type nx, size_type ny);


			/**
			 *  @brief      Calculates the normalization factor for the 3d Fourier transformation
			 *  @details    In order to fulfill a = IFFT(FFT(a)) we have to normalize the forward and
			                backward Fourier transformation as FFTW3's convention guaranties 
			                a = N*IFFT(FFT(a)), where N is the number of elements of a. This function 
			                calculates the normalization factor which equals the square root of N.
			 *  @param[in]  nx Size of the array in x-direction
			 *  @param[in]  ny Size of the array in y-direction
			 *  @param[in]  nz Size of the array in z-direction
			 *  @return     Normalization factor
			 */
			static value_type getScalingFactor3d (size_type nx, size_type ny, size_type nz);		
			
		};
		
	} /* Utilities */
	
} /* SingleParticle2dx */


#endif /* end of include guard: FFTCALCULATOR_HPP_PTT4NBZ6 */
