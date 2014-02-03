#ifndef ABSTRACTCALCCCMETHODS_HPP_22CEOMBM
#define ABSTRACTCALCCCMETHODS_HPP_22CEOMBM

namespace SingleParticle2dx
{
	namespace Methods
	{
		class AbstractCalcCCMethod;
	}
}

namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class Particle;
		class Projection2d;
	} 
}


#include "../../Typedefs.hpp"


namespace SingleParticle2dx
{
	
	namespace Methods
	{
		
		/**
		 *  @brief     Abstract base class for calculating a cross-correlation
		 *  @details   
		 *  @author    Sebastian Scherer
		 *  @version   0.1
		 *  @date      2012
		 *  @copyright GNU Public License.
		 */
		class AbstractCalcCCMethod
		{
			
		public:
			
			/** Type storing a 2d array of real numbers */
			typedef SingleParticle2dx::real_array2d_type real_array2d_type;
			
			
			/** Type storing a boost::multi_array index*/
			typedef fft_array3d_type::index_range range;
			
			
			/**
			 *  @brief      Calculates the cross-correlation of template and particle
			 *  @details    
			 *  @param[in]  part Particle
			 *  @param[in]  proj Projection
			 *  @param[out] res cross-correlation profile
			 *  @post       res filled with cross-correlation profile
			 */
			virtual void calculateCrossCorrelation( SingleParticle2dx::DataStructures::Particle& part, SingleParticle2dx::DataStructures::Projection2d& proj, real_array2d_type& res ) = 0;
			
		};
		
	} /* Methods */
	
} /* SingleParticle2dx */

#endif /* end of include guard: ABSTRACTCALCCCMETHODS_HPP_22CEOMBM */
