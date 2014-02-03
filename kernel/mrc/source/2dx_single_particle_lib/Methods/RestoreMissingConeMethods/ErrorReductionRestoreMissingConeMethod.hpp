#ifndef ERRORREDUCTIONRESTOREMISSINGCONEMETHOD_HPP_OIGJP8L7
#define ERRORREDUCTIONRESTOREMISSINGCONEMETHOD_HPP_OIGJP8L7

namespace SingleParticle2dx
{
	namespace Methods
	{
		class ErrorReductionRestoreMissingConeMethod;
	}
}


#include "AbstractRestoreMissingConeMethod.hpp"

namespace SingleParticle2dx
{
	
	namespace Methods
	{
		
		/**
		 *  @brief     HIO restore missing cone class
		 *  @details   
		 *  @author    Sebastian Scherer
		 *  @version   0.1
		 *  @date      2012
		 *  @copyright GNU Public License
		 */
		class ErrorReductionRestoreMissingConeMethod : public SingleParticle2dx::Methods::AbstractRestoreMissingConeMethod
		{
		public:
		
			/**
			 *  @brief      Constructor
			 *  @details    Costum constructor
			 *  @param[in]  context Context of the method
			 */
			ErrorReductionRestoreMissingConeMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context);
			
			
			/**
			 *  @brief      Destructor
			 *  @details    All the allocated memory is freed afer calling the Destructor
			 */
			virtual ~ErrorReductionRestoreMissingConeMethod ();
			
			
			/**
			 *  @brief      Restore the missing cone
			 *  @details    
			 */
			virtual void restoreMissingCone();

		};
		
	} /* Methods */
	
} /* SingleParticle2dx */

#endif /* end of include guard: ERRORREDUCTIONRESTOREMISSINGCONEMETHOD_HPP_OIGJP8L7 */


