#ifndef ABSTRACTINTERPOLATE2DMETHOD_HPP_3SPN0LQ1
#define ABSTRACTINTERPOLATE2DMETHOD_HPP_3SPN0LQ1


namespace SingleParticle2dx
{
	namespace Methods
	{		
		class AbstractInterpolate2dMethod;
	}
}


#include <vector>

#include "../../Typedefs.hpp"


namespace SingleParticle2dx
{
	
	namespace Methods
	{
	
		/**
		 *  @brief     Abstract base interpolate method
		 *  @details   
		 *  @author    Sebastian Scherer
		 *  @version   0.1
		 *  @date      2012
		 *  @copyright GNU Public License.
		 */
		class AbstractInterpolate2dMethod
		{
			
		public:
			
			/** Internal data type used for storing an angular value */
			typedef SingleParticle2dx::value_type value_type;
			
			
			/**
			 *  @brief      Interpolate
			 *  @details    
			 *  @param[in]  dx Offset in x-direction (0 = top left, 1 = top right)
			 *  @param[in]  dy Offset in x-direction (0 = top left, 1 = top right)
			 *  @param[in]  val Value to interpolate
			 *  @param[out] res Result (down left, down right, up left, up right)
			 */
			virtual void interpolate2d(value_type dx, value_type dy, value_type val, std::vector<value_type>& res) = 0;

		};
		
	} /* AbstractInterpolate2dMethod */
	
} /* SingleParticle2dx */

#endif /* end of include guard: ABSTRACTINTERPOLATE2DMETHOD_HPP_3SPN0LQ1 */
