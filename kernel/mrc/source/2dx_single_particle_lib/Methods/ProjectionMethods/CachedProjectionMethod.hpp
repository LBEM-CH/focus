#ifndef CACHEDPROJECTIONMETHOD_HPP_244FOD6I
#define CACHEDPROJECTIONMETHOD_HPP_244FOD6I


namespace SingleParticle2dx
{
	namespace Methods
	{
		class CachedProjectionMethod;
	} 
}


#include "AbstractProjectionMethod.hpp"
#include "../../DataStructures.hpp"
#include "../../Typedefs.hpp"
#include "../../Methods.hpp"

#include "../TrialAngleGenerators/InterfaceHasTrialAngleGenerator.hpp"




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
		class CachedProjectionMethod : public SingleParticle2dx::Methods::AbstractProjectionMethod, public SingleParticle2dx::Methods::InterfaceHasTrialAngleGenerator
		{
		public:
				
			typedef SingleParticle2dx::real_array2d_type real_array2d_type;
			typedef SingleParticle2dx::real_array3d_type real_array3d_type;
			
							
			/**
			 *  @brief      Constructor
			 *  @details    Costum constructor
			 *  @param[in]  context Context of the method
			 */
			CachedProjectionMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context);
			
			
			/**
			 *  @brief      Destructor
			 *  @details    All the allocated memory is freed afer calling the Destructor
			 */
			virtual ~CachedProjectionMethod ();
			
			
			virtual void prepareForProjections(SingleParticle2dx::DataStructures::ParticleContainer& cont);

			
			/**
			 *  @brief      Calculates the projection in direction o
			 *  @details    
			 *  @param[in]  o Direction of the projection to calculate
			 *  @param[out] p Reference where to store the calculated projection
			 */
			virtual void calculateProjection(SingleParticle2dx::DataStructures::Orientation& o, SingleParticle2dx::DataStructures::Projection2d& p);
			
			virtual void clearProjections();
			
		private:
		
			struct classcomp
			{
				bool operator() (const SingleParticle2dx::DataStructures::Orientation& lhs, const SingleParticle2dx::DataStructures::Orientation& rhs) const
				{
					//return lhs<rhs;
					if ( lhs.getTLTAXIS() < rhs.getTLTAXIS() )
					{
						return true;
					}
					else
					{
						if ( (lhs.getTLTAXIS()==rhs.getTLTAXIS()) && (lhs.getTLTANG()<rhs.getTLTANG()) )
						{
							return true;
						}
						else
						{
							if ( (lhs.getTLTAXIS()==rhs.getTLTAXIS()) && (lhs.getTLTANG()==rhs.getTLTANG()) && (lhs.getTAXA()<rhs.getTAXA()) )
							{
								return true;
							}
						}
					}
					return false;
				}
			};
			
			/** internal storage container */
			std::map<SingleParticle2dx::DataStructures::Orientation, SingleParticle2dx::DataStructures::Projection2d, classcomp> proj_map;
			
			boost::scoped_ptr<SingleParticle2dx::Methods::AbstractProjectionMethod> m_projector; 
			
		};
		
	} /* Algorithms */	
	
} /* SingleParticle2dx */

#endif /* end of include guard: CACHEDPROJECTIONMETHOD_HPP_244FOD6I */
