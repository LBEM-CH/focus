#ifndef REALSPACEPROJECTIONALGORITHM_HPP
#define REALSPACEPROJECTIONALGORITHM_HPP

namespace SingleParticle2dx
{
	namespace Methods
	{
		class RealSpaceProjectionMethod;
	}
}

#include <vector>

#include <eigen3/Eigen/Dense>

#include "AbstractProjectionMethod.hpp"
#include "../../DataStructures.hpp"
#include "../../Typedefs.hpp"
#include "../../Methods.hpp"

namespace SingleParticle2dx
{
	namespace Methods
	{
		class AbstractProjectionMethod;
	} 
}

namespace SingleParticle2dx
{
	
	namespace Methods
	{
		
		/**
		 *  @brief     Projection class preforming real space projections
		 *  @details   
		 *  @author    Sebastian Scherer
		 *  @version   0.1
		 *  @date      2012
		 *  @copyright GNU Public License
		 */
		class RealSpaceProjectionMethod : public SingleParticle2dx::Methods::AbstractProjectionMethod
		{
		public:
			
			/** Type of a generic rotation */
			typedef Eigen::AngleAxis<value_type> Rotation;
		
		
			/** Type of a 3d vector */
			typedef Eigen::Vector3f Vector3d;
			
						
			/**
			 *  @brief      Constructor
			 *  @details    Costum constructor
			 *  @param[in]  context Context of the method
			 */
			RealSpaceProjectionMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context);
			
			
			/**
			 *  @brief      Destructor
			 *  @details    All the allocated memory is freed afer calling the Destructor
			 */
			virtual ~RealSpaceProjectionMethod ();

			
			/**
			 *  @brief      Calculates the projection in direction o
			 *  @details    
			 *  @param[in]  o Direction of the projection to calculate
			 *  @param[out] p Reference where to store the calculated projection
			 */
			virtual void calculateProjection(SingleParticle2dx::DataStructures::Orientation& o, SingleParticle2dx::DataStructures::Projection2d& p);
			
			
			/**
			 *  @brief      Interpolate
			 *  @details    Wrapper functions calling the stored interpolation strategy
			 *  @param[in]  dx Offset in x-direction (0 = top left, 1 = top right)
			 *  @param[in]  dy Offset in x-direction (0 = top left, 1 = top right)
			 *  @param[in]  val Value to interpolate
			 *  @param[out] res Result (down left, down right, up left, up right)
			 */
			void interpolate2d(value_type dx, value_type dy, value_type val, std::vector<value_type>& res);
			
			
			virtual void prepareForProjections(SingleParticle2dx::DataStructures::ParticleContainer& cont)
			{
				return;
			}
			
		private:
		
			/** interpolation strategy */
			boost::scoped_ptr<SingleParticle2dx::Methods::AbstractInterpolate2dMethod> m_iterpolate_strategy;

		};
		
	} /* Algorithms */	
	
} /* SingleParticle2dx */

#endif /* end of include guard: REALSPACEPROJECTIONALGORITHM_HPP */
