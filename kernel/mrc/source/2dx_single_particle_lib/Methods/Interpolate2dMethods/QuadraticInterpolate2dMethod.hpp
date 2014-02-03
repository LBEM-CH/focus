#ifndef QUADRATICINTERPOLATE2DMETHOD_HPP_QX5A9VGI
#define QUADRATICINTERPOLATE2DMETHOD_HPP_QX5A9VGI


namespace SingleParticle2dx
{
	namespace Methods
	{
		class QuadraticInterpolate2dMethod;
	}
}


#include "AbstractInterpolate2dMethod.hpp"


namespace SingleParticle2dx
{
	namespace Methods
	{
	
		/**
		 *  @brief     quad interpolate method
		 *  @details   
		 *  @author    Sebastian Scherer
		 *  @version   0.1
		 *  @date      2012
		 *  @copyright GNU Public License.
		 */
		class QuadraticInterpolate2dMethod : public SingleParticle2dx::Methods::AbstractInterpolate2dMethod
		{

		public:
		
			/**
			 *  @brief      Default constructor
			 *  @details    
			 *  @post       Method ready to use
			 */
			QuadraticInterpolate2dMethod ();
			
			
			/**
			 *  @brief      Destructor
			 *  @details    All the allocated memory is freed afer calling the Destructor
			 */
			virtual ~QuadraticInterpolate2dMethod ();
			
			
			/**
			 *  @brief      Interpolate quadratic 
			 *  @details    
			 *  @param[in]  dx Offset in x-direction (0 = top left, 1 = top right)
			 *  @param[in]  dy Offset in x-direction (0 = top left, 1 = top right)
			 *  @param[in]  val Value to interpolate
			 *  @param[out] res Result (down left, down right, up left, up right)
			 */
			virtual void interpolate2d(value_type dx, value_type dy, value_type val, std::vector<value_type>& res);

		};
		
	} /* Methods */
	
} /* SingleParticle2dx */

#endif /* end of include guard: QUADRATICINTERPOLATE2DMETHOD_HPP_QX5A9VGI */
