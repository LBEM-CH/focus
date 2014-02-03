#ifndef DUMMYREFINEMENTMETHOD_HPP_QKESYUSL
#define DUMMYREFINEMENTMETHOD_HPP_QKESYUSL


namespace SingleParticle2dx
{
	namespace Methods
	{		
		class DummyRefinementMethod;
	}
}


#include "AbstractRefinementMethod.hpp"
#include "../FindBestProjectionMethods.hpp"


namespace SingleParticle2dx
{
	namespace Methods
	{
		
		/**
		 *  @brief     Cross-correlation refinement class
		 *  @details   
		 *  @author    Sebastian Scherer
		 *  @version   0.1
		 *  @date      2012
		 *  @copyright GNU Public License
		 */
		class DummyRefinementMethod : public SingleParticle2dx::Methods::AbstractRefinementMethod
		{
			
		public:
		
			/**
			 *  @brief      Constructor
			 *  @details    Costum constructor
			 *  @param[in]  context Context of the method
			 */
			DummyRefinementMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context);
			
			
			/**
			 *  @brief      Destructor
			 *  @details    All the allocated memory is freed afer calling the Destructor
			 */
			virtual ~DummyRefinementMethod ();
			
			
			/**
			 *  @brief      Update the reconstruction
			 *  @details    Done by cross-correlation
			 *  @param[in]  c ParticleContainer to refine
			 */
			virtual void updateReconstruction(SingleParticle2dx::DataStructures::ParticleContainer& c, bool useneighbors, bool write_debug_output = false);
		
		};
		
	} /* Algorithms */
	
} /* SingleParticle2dx */

#endif /* end of include guard: DUMMYREFINEMENTMETHOD_HPP_QKESYUSL */
