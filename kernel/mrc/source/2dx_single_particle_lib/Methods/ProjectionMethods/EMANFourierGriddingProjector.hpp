#ifndef EMANFOURIERGRIDDINGPROJECTOR_HPP_I5ZWFCQD
#define EMANFOURIERGRIDDINGPROJECTOR_HPP_I5ZWFCQD

namespace SingleParticle2dx
{
	namespace Methods
	{
		class EMANFourierGriddingProjector;
	} 
}


#include "AbstractProjectionMethod.hpp"
#include "../../DataStructures.hpp"
#include "../../Typedefs.hpp"
#include "../../Methods.hpp"

#include <projector.h>



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
		class EMANFourierGriddingProjector : public SingleParticle2dx::Methods::AbstractProjectionMethod
		{
		public:
				
			typedef SingleParticle2dx::real_array2d_type real_array2d_type;
			typedef SingleParticle2dx::real_array3d_type real_array3d_type;
			
			
								
			/**
			 *  @brief      Constructor
			 *  @details    Costum constructor
			 *  @param[in]  context Context of the method
			 */
			EMANFourierGriddingProjector (SingleParticle2dx::DataStructures::Reconstruction3d* context);
			
			
			/**
			 *  @brief      Destructor
			 *  @details    All the allocated memory is freed afer calling the Destructor
			 */
			virtual ~EMANFourierGriddingProjector ();
			
			
			virtual void prepareForProjections(SingleParticle2dx::DataStructures::ParticleContainer& cont);

			
			/**
			 *  @brief      Calculates the projection in direction o
			 *  @details    
			 *  @param[in]  o Direction of the projection to calculate
			 *  @param[out] p Reference where to store the calculated projection
			 */
			virtual void calculateProjection(SingleParticle2dx::DataStructures::Orientation& o, SingleParticle2dx::DataStructures::Projection2d& p);
			
		private:
		
			EMAN::Projector* m_proj;
			EMAN::EMData* m_3dmodel;
			float* m_float_data_3d;

		};
		
	} /* Algorithms */	
	
} /* SingleParticle2dx */


#endif /* end of include guard: EMANFOURIERGRIDDINGPROJECTOR_HPP_I5ZWFCQD */
