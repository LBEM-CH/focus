#ifndef ABSTRACTRESTOREMISSINGCONEMETHODS_HPP_5MC7L2NO
#define ABSTRACTRESTOREMISSINGCONEMETHODS_HPP_5MC7L2NO

namespace SingleParticle2dx
{
	namespace Methods
	{		
		class AbstractRestoreMissingConeMethod;
	}
}

namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class Reconstruction3d;
	} 
}


#include "../../Typedefs.hpp"

namespace SingleParticle2dx
{
	
	namespace Methods
	{
		
		/**
		 *  @brief     Abstract restore missing cone class
		 *  @details   
		 *  @author    Sebastian Scherer
		 *  @version   0.1
		 *  @date      2012
		 *  @copyright GNU Public License
		 */
		class AbstractRestoreMissingConeMethod
		{
		public:
			
			typedef SingleParticle2dx::real_array3d_type real_array3d_type;
			typedef SingleParticle2dx::fft_array3d_type fft_array3d_type;
		
			/**
			 *  @brief      Restore the missing cone
			 *  @details    
			 */
			virtual void restoreMissingCone() = 0;

		protected:
		
			/** Context from which the method is called */
			SingleParticle2dx::DataStructures::Reconstruction3d* m_context;
			
		};
		
	} /* Methods */
	
} /* SingleParticle2dx */


#endif /* end of include guard: ABSTRACTRESTOREMISSINGCONEMETHODS_HPP_5MC7L2NO */
