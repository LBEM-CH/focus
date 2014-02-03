#ifndef ABSTRACTFINDBESTPROJECTIONMETHOD_HPP_PKVVDN8M
#define ABSTRACTFINDBESTPROJECTIONMETHOD_HPP_PKVVDN8M


namespace SingleParticle2dx
{
	namespace Methods
	{		
		class AbstractFindBestProjectionMethod;
	}
}


namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class Particle;
		class Reconstruction3d;
	} 
}


#include "../../Typedefs.hpp"


namespace SingleParticle2dx
{
	
	namespace Methods
	{
		
		/**
		 *  @brief     Abstract base find best projection method
		 *  @details   
		 *  @author    Sebastian Scherer
		 *  @version   0.1
		 *  @date      2012
		 *  @copyright GNU Public License.
		 */
		class AbstractFindBestProjectionMethod
		{
			
		public:
			
			/** value type */
			typedef SingleParticle2dx::value_type value_type;
			
			
			/** size type */
			typedef SingleParticle2dx::size_type size_type;
			
			
			/** Type storing a 2d array of complex numbers */
			typedef SingleParticle2dx::fft_array2d_type fft_array2d_type;
			
			
			/** Type storing a 2d array of real numbers */
			typedef SingleParticle2dx::real_array2d_type real_array2d_type;			
			
			
			/**
			 *  @brief      Determines best projection and store the corresponding information
			 *  @details    Call a abstact method implemented by means of the strategy pattern
			 *  @param[in]  p For which the projection is optimized
			 *  @post       Best projection found, orientation and optimal shift stored in p
			 */
			virtual void determineBestProjection (SingleParticle2dx::DataStructures::Particle& p, bool useneighbors, bool write_debug_output = false) = 0;

		protected:
		
			/** Context from which the method is called */
			SingleParticle2dx::DataStructures::Reconstruction3d* m_context;
			
		};
		
	} /* Algorithms */
	
} /* SingleParticle2dx */

#endif /* end of include guard: ABSTRACTFINDBESTPROJECTIONMETHOD_HPP_PKVVDN8M */
