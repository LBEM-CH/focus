#ifndef BASICCALCCCMETHODS_HPP_GZK54HUY
#define BASICCALCCCMETHODS_HPP_GZK54HUY


namespace SingleParticle2dx
{
	namespace Methods
	{
		class BasicCalcCCMethod;
	}
}


#include "AbstractCalcCCMethod.hpp"


namespace SingleParticle2dx
{
	namespace Methods
	{
		
		/**
		 *  @brief     Basic CC-calculation method
		 *  @details   
		 *  @author    Sebastian Scherer
		 *  @version   0.1
		 *  @date      2012
		 *  @copyright GNU Public License.
		 */
		class BasicCalcCCMethod : public SingleParticle2dx::Methods::AbstractCalcCCMethod
		{
			
		public:
		
			/**
			 *  @brief      Default constructor
			 *  @details    
			 *  @post       Method ready to use
			 */
			BasicCalcCCMethod ();
			
			
			/**
			 *  @brief      Destructor
			 *  @details    All the allocated memory is freed afer calling the Destructor
			 */
			virtual ~BasicCalcCCMethod ();
			
			
			/**
			 *  @brief      Calculates the cross-correlation of template and particle
			 *  @details    
			 *  @param[in]  part Particle
			 *  @param[in]  proj Projection
			 *  @param[out] res cross-correlation profile
			 *  @post       res filled with cross-correlation profile
			 */
			virtual void calculateCrossCorrelation( SingleParticle2dx::DataStructures::Particle& part, SingleParticle2dx::DataStructures::Projection2d& proj, real_array2d_type& res );

		};
		
	} /* Methods */
	
} /* SingleParticle2dx */

#endif /* end of include guard: BASICCALCCCMETHODS_HPP_GZK54HUY */
