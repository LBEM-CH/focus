#ifndef MAXSEARCHALGORITHM_HPP_21JGB7LN
#define MAXSEARCHALGORITHM_HPP_21JGB7LN


namespace SingleParticle2dx
{
	namespace Methods
	{
		class AbstractPeakSearchMethod;
	}	
}


namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class Orientation;
		class Projection2d;
		class Reconstruction3d;
		class ParticleShift;
	} 
}


#include "../../Typedefs.hpp"
#include "../../DataStructures.hpp"


namespace SingleParticle2dx
{
	namespace Methods
	{
		/**
		 *  @brief     Abstract max search method class
		 *  @details   
		 *  @author    Sebastian Scherer
		 *  @version   0.1
		 *  @date      2012
		 *  @copyright GNU Public License
		 */
		class AbstractPeakSearchMethod
		{
		public:
			
			/** Type storing a real value */
			typedef SingleParticle2dx::value_type value_type;
			
			
			/** Type storing a 2d array of real values */
			typedef SingleParticle2dx::real_array2d_type real_array2d_type;
						
			
			/**
			 *  @brief      Finds the highest CC-Peak and stores shift
			 *  @details    Call a abstact method implemented by means of the strategy pattern
			 *  @param[in]  cc_array Cross-Correlation profile
			 *  @param[out] shift Optimal shift
			 *  @post       Particle p inserted by means of back-projection
			 */
			virtual value_type findMaximalElementAndSetShift(real_array2d_type& cc_array, SingleParticle2dx::DataStructures::ParticleShift& shift) = 0;
			
		};
		
	} /* Algorithms */
	
} /* SingleParticle2dx */

#endif /* end of include guard: MAXSEARCHALGORITHM_HPP_21JGB7LN */

