#ifndef CCFINDBESTPROJECTIONMETHOD_HPP_ED5Q4Z0W
#define CCFINDBESTPROJECTIONMETHOD_HPP_ED5Q4Z0W

namespace SingleParticle2dx
{	
	namespace Methods
	{
		class CCFindBestProjectionMethod;
	}
}


#include "AbstractFindBestProjectionMethod.hpp"
#include "../TrialAngleGenerators.hpp"

#include "../../Methods.hpp"
#include "../TrialAngleGenerators/InterfaceHasTrialAngleGenerator.hpp"


namespace SingleParticle2dx
{
	
	namespace Methods
	{
		
		/**
		 *  @brief     CC find best projection method
		 *  @details   
		 *  @author    Sebastian Scherer
		 *  @version   0.1
		 *  @date      2012
		 *  @copyright GNU Public License.
		 */
		class CCFindBestProjectionMethod : public SingleParticle2dx::Methods::AbstractFindBestProjectionMethod, public SingleParticle2dx::Methods::InterfaceHasTrialAngleGenerator
		{
			
		public:
			
			/**
			 *  @brief      Default constructor
			 *  @details    
			 *  @param[in]  context Raw pointer to the context
			 *  @post       Method ready to use
			 */
			CCFindBestProjectionMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context);
			
			
			/**
			 *  @brief      Destructor
			 *  @details    All the allocated memory is freed afer calling the Destructor
			 */
			virtual ~CCFindBestProjectionMethod ();
			
			
			/**
			 *  @brief      Determines best projection and store the corresponding information
			 *  @details    Call a abstact method implemented by means of the strategy pattern
			 *  @param[in]  p For which the projection is optimized
			 *  @post       Best projection found, orientation and optimal shift stored in p
			 */
			virtual void determineBestProjection (SingleParticle2dx::DataStructures::Particle& p, bool useneighbors, bool write_debug_output = false);
			
		private:
		
			/** peak search strategy */
			boost::scoped_ptr<SingleParticle2dx::Methods::AbstractPeakSearchMethod> m_peaksearch_strategy;

			
			/** cc calc strategy */
			boost::scoped_ptr<SingleParticle2dx::Methods::AbstractCalcCCMethod> m_clac_cc_strategy;
		
		};
		
	} /* Algorithms */
	
} /* SingleParticle2dx */

#endif /* end of include guard: CCFINDBESTPROJECTIONMETHOD_HPP_ED5Q4Z0W */
