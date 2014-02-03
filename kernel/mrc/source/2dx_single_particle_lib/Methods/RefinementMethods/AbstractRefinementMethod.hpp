#ifndef ABSTRACTREFINEMENTMETHOD_HPP_2CYAD3AW
#define ABSTRACTREFINEMENTMETHOD_HPP_2CYAD3AW


namespace SingleParticle2dx
{
	namespace Methods
	{
		class AbstractRefinementMethod;
	}
}


namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class Reconstruction3d;
		class ParticleContainer;
	}
}


#include "../../Typedefs.hpp"


namespace SingleParticle2dx
{
	
	namespace Methods
	{
		
		/**
		 *  @brief     Abstract refinement class
		 *  @details   
		 *  @author    Sebastian Scherer
		 *  @version   0.1
		 *  @date      2012
		 *  @copyright GNU Public License
		 */
		class AbstractRefinementMethod
		{
			
		public:
			
			/** Type storing the size of the particle image */
			typedef SingleParticle2dx::size_type size_type;
			
			
			/**
			 *  @brief      Update the reconstruction
			 *  @details    
			 *  @param[in]  c ParticleContainer to refine
			 */
			virtual void updateReconstruction(SingleParticle2dx::DataStructures::ParticleContainer& c, bool useneighbors, bool write_debug_output = false) = 0;

		protected:
			
			/** Context from which the method is called */
			SingleParticle2dx::DataStructures::Reconstruction3d* m_context;
			
		};
		
	} /* Algorithms */
	
} /* SingleParticle2dx */

#endif /* end of include guard: ABSTRACTREFINEMENTMETHOD_HPP_2CYAD3AW */
