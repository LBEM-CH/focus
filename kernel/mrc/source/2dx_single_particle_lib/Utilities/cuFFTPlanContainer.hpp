#ifdef USE_CUDA

#ifndef SINGLE_PARTICLE_2DX_CUFFTPLANCONTAINER_HPP
#define SINGLE_PARTICLE_2DX_CUFFTPLANCONTAINER_HPP

namespace SingleParticle2dx
{
	namespace Utilities
	{
		class cuFFTPlanContainer;
	}
}


#include <cufft.h>
#include <cuda.h>

#include <boost/scoped_ptr.hpp>

#include "../Typedefs.hpp"


namespace SingleParticle2dx
{
	namespace Utilities
	{
		
		/**
		 *  @brief     Global FFTW plan container class
		 *  @details   Singleton instance holding all the required FFTW3 plans. The size of the plans 
		               depend on the global settings, which are directly loaded from ConfigContainer.
		 *  @author    Sebastian Scherer
		 *  @version   0.1
		 *  @date      2012
		 *  @note      Singleton Pattern
		 *  @copyright GNU Public License
		 */
		class cuFFTPlanContainer
		{

		public:

			/** Type storing the size of the particle image */
			typedef SingleParticle2dx::size_type size_type;
			
			/** 2d real space array type */
			typedef SingleParticle2dx::real_array2d_type real_array2d_type;


			/** 3d real space array type */
			typedef SingleParticle2dx::real_array3d_type real_array3d_type;


			/**
			 *  @brief      Get global instance function
			 *  @details    First the function checks whether the global object is already constructed. 
			                If so, the function returns a pointer to the global instance. Otherwise the 
			                object is constructed before the pointer gets returned.
			 *  @return     Pointer to the single instance
			 *  @note       thread-safe constructor call guarantied
			 */
			static cuFFTPlanContainer* Instance ();


			/**
			 *  @brief      Destructor
			 *  @details    Deletes the global object
			 *  @post       All memory freed
			 */
			virtual ~cuFFTPlanContainer ();


			/**
			 *  @brief      Returns the 2d forward FFTW-plan
			 *  @details    Plan is used the perform a forward 2d Fourier transformation
			 *  @return     2d forward FFTW-plan
			 */
			cufftHandle& getPlan_2d ();


			/**
			 *  @brief      Returns the 2d forward FFTW-plan large (4 times)
			 *  @details    Plan is used the perform a forward 2d Fourier transformation
			 *  @return     2d forward FFTW-plan
			 */
			cufftHandle& getPlan_4_2d ();


			/**
			 *  @brief      Returns the 3d forward FFTW-plan
			 *  @details    Plan is used the perform a forward 3d Fourier transformation
			 *  @return     3d forward FFTW-plan
			 */
			cufftHandle& getPlan_3d ();

			cudaStream_t& getStream();

			
			real_array2d_type* getSmallPowArray2D();
			real_array2d_type* getLargePowArray2D();
			real_array3d_type* getPowArray3D();
			
			cufftComplex* getHostArray();
			cufftComplex* getDeviceArray();

		protected:

			/**
			 *  @brief      Constructor 
			 *  @details    Creates all plans later used by the single particle reconstruction 
			                application. The size of the plans depend on the global settings, which are
			                directly loaded from ConfigContainer.
			 *  @post       Global FFTPlanContainer container initialized
			 *  @note       Constructor is protected and thus can not be called from outside the class
			 */
			cuFFTPlanContainer ();


			/**
			 *  @brief      Deletes all plans by means of fftw_free
			 *  @post       All memory freed
			 */
			void deletePlans ();

			
			void setupPowArrays (size_type n);

		private:

			/** Pointer to the global instance of this class */
			static boost::scoped_ptr<cuFFTPlanContainer> m_instance;

			boost::scoped_ptr<real_array2d_type> m_powf_array_2d;
			boost::scoped_ptr<real_array2d_type> m_powf_array_2d_large;
			boost::scoped_ptr<real_array3d_type> m_powf_array_3d;
			
			std::vector<cufftHandle> m_small_2d_plan;
			std::vector<cufftHandle> m_large_2d_plan;
			std::vector<cufftHandle> m_3d_plan;
			std::vector<cudaStream_t> m_streams;
			
			std::vector<cufftComplex*> m_device_p;
			std::vector<cufftComplex*> m_host_p;

		};
		
	} /* Utilities */
	
} /* SingleParticle2dx */

#endif /* end of include guard: SINGLE_PARTICLE_2DX_CUFFTPLANCONTAINER_HPP */

#endif
