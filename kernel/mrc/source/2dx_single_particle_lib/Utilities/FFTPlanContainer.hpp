#ifndef SINGLE_PARTICLE_2DX_FFTPLANCONTAINER_HPP
#define SINGLE_PARTICLE_2DX_FFTPLANCONTAINER_HPP

namespace SingleParticle2dx
{
	namespace Utilities
	{
		class FFTPlanContainer;
	}
}


#include <fftw3.h>
//#include "fftw3_mkl.h"

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
		class FFTPlanContainer
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
			static FFTPlanContainer* Instance ();


			/**
			 *  @brief      Destructor
			 *  @details    Deletes the global object
			 *  @post       All memory freed
			 */
			virtual ~FFTPlanContainer ();


			/**
			 *  @brief      Returns the 2d forward FFTW-plan
			 *  @details    Plan is used the perform a forward 2d Fourier transformation
			 *  @return     2d forward FFTW-plan
			 */
			fftwf_plan& getFW_2d ();


			/**
			 *  @brief      Returns the 2d backward FFTW-plan
			 *  @details    Plan is used the perform a backward 2d Fourier transformation
			 *  @return     2d backward FFTW-plan
			 */
			fftwf_plan& getBW_2d ();


			/**
			 *  @brief      Returns the 2d forward FFTW-plan large (4 times)
			 *  @details    Plan is used the perform a forward 2d Fourier transformation
			 *  @return     2d forward FFTW-plan
			 */
			fftwf_plan& getFW_4_2d ();


			/**
			 *  @brief      Returns the 2d backward FFTW-plan large (4 times)
			 *  @details    Plan is used the perform a backward 2d Fourier transformation
			 *  @return     2d backward FFTW-plan
			 */
			fftwf_plan& getBW_4_2d ();


			/**
			 *  @brief      Returns the 3d forward FFTW-plan
			 *  @details    Plan is used the perform a forward 3d Fourier transformation
			 *  @return     3d forward FFTW-plan
			 */
			fftwf_plan& getFW_3d ();


			/**
			 *  @brief      Returns the 3d backward FFTW-plan
			 *  @details    Plan is used the perform a backward 3d Fourier transformation
			 *  @return     3d backward FFTW-plan
			 */
			fftwf_plan& getBW_3d ();
			
			real_array2d_type* getSmallPowArray2D();
			real_array2d_type* getLargePowArray2D();
			real_array3d_type* getPowArray3D();

		protected:

			/**
			 *  @brief      Constructor 
			 *  @details    Creates all plans later used by the single particle reconstruction 
			                application. The size of the plans depend on the global settings, which are
			                directly loaded from ConfigContainer.
			 *  @post       Global FFTPlanContainer container initialized
			 *  @note       Constructor is protected and thus can not be called from outside the class
			 */
			FFTPlanContainer ();


			/**
			 *  @brief      Deletes all plans by means of fftw_free
			 *  @post       All memory freed
			 */
			void deletePlans ();


			/**
			 *  @brief      Ensures the correctness of all constructed plans 
			 *  @details    Checks the allocation of all plans. If the test fails the plans are deleted
			 *              and an exception is thrown.
			 *  @post       Plans legal or exception thrown
			 */
			void checkPlans ();
			
			void setupPowArrays (size_type n);

		private:

			/** Pointer to the global instance of this class */
			static boost::scoped_ptr<FFTPlanContainer> m_instance;


			/** 2d forward FFTW-plan */
			fftwf_plan m_p2d_fw;


			/** 2d backward FFTW-plan */
			fftwf_plan m_p2d_bw;


			/** large (4 times) 2d forward FFTW-plan */
			fftwf_plan m_p2d_4_fw;


			/** large (4 times) 2d backward FFTW-plan */
			fftwf_plan m_p2d_4_bw;


			/** 3d forward FFTW-plan */
			fftwf_plan m_p3d_fw;


			/** 3d backward FFTW-plan */
			fftwf_plan m_p3d_bw;
			
			boost::scoped_ptr<real_array2d_type> m_powf_array_2d;
			boost::scoped_ptr<real_array2d_type> m_powf_array_2d_large;
			boost::scoped_ptr<real_array3d_type> m_powf_array_3d;	

		};
		
	} /* Utilities */
	
} /* SingleParticle2dx */

#endif /* end of include guard: SINGLE_PARTICLE_2DX_FFTPLANCONTAINER_HPP */
