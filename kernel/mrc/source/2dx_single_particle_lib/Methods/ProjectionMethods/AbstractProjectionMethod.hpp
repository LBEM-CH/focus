#ifndef PROJECTIONALGORITHM_HPP
#define PROJECTIONALGORITHM_HPP


namespace SingleParticle2dx
{
	namespace Methods
	{
		class AbstractProjectionMethod;
	} 
} 

namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class Orientation;
		class Projection2d;
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
		 *  @brief     Abstract Projection class
		 *  @details   
		 *  @author    Sebastian Scherer
		 *  @version   0.1
		 *  @date      2012
		 *  @copyright GNU Public License
		 */
		class AbstractProjectionMethod
		{
			
		public:
			
			/** Internal data type used for storing an angular value */
			typedef SingleParticle2dx::value_type value_type;
		
			
			/** Size type */
			typedef SingleParticle2dx::size_type size_type;
			
			
			/**
			 *  @brief      Called before the first projection with a new 3D model
			 *  @details    Use this function to reset model depedent data fields for the next projection run
			 */
			virtual void prepareForProjections(SingleParticle2dx::DataStructures::ParticleContainer& cont) = 0;
			
			
			/**
			 *  @brief      Calculates the projection in direction o
			 *  @details    
			 *  @param[in]  o Direction of the projection to calculate
			 *  @param[out] p Reference where to store the calculated projection
			 */
			virtual void calculateProjection(SingleParticle2dx::DataStructures::Orientation& o, SingleParticle2dx::DataStructures::Projection2d& p) = 0;
			
			virtual void clearProjections()
			{
				return;
			}
			
			
		protected:
			
			/** Context from which the method is called */
			SingleParticle2dx::DataStructures::Reconstruction3d* m_context;
			
		};
		
	} /* Algorithms */
	
} /* SingleParticle2dx */

#endif /* end of include guard: PROJECTIONALGORITHM_HPP */
