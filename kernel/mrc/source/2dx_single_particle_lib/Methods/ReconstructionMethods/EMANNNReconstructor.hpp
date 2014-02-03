#ifndef EMANNNRECONSTRUCTOR_HPP_YC8YVKH9
#define EMANNNRECONSTRUCTOR_HPP_YC8YVKH9

namespace SingleParticle2dx
{
	namespace Methods
	{
		class EMANNNReconstructor;
	}
}

#include <emdata.h>
#include <emobject.h>

#include "AbstractReconstructionMethod.hpp"
#include "../../Typedefs.hpp"
#include <reconstructor.h>

namespace SingleParticle2dx
{
	namespace Methods
	{
		class EMANNNReconstructor : public SingleParticle2dx::Methods::AbstractReconstructionMethod
		{
			
		public:
		
			typedef SingleParticle2dx::fft_type fft_type;
			
			
			typedef SingleParticle2dx::real_array2d_type real_array2d_type;
			
			
			typedef SingleParticle2dx::real_array3d_type real_array3d_type;
			
			
			EMANNNReconstructor (SingleParticle2dx::DataStructures::Reconstruction3d* context);
			
			
			virtual ~EMANNNReconstructor ();
			
			
			virtual void insertParticle(SingleParticle2dx::DataStructures::Particle& p);
			
			
			virtual void setupForBackProjection();
			
			
			virtual void finishReconstruction();
						
			
			virtual void insertData(SingleParticle2dx::DataStructures::ParticleContainer &c);
			
		private:
			
			EMAN::Dict m_params;
			EMAN::Reconstructor* m_rec;
		
		};
		
	} /* Methods */
	
} /* SingleParticle2dx */

#endif /* end of include guard: EMANNNRECONSTRUCTOR_HPP_YC8YVKH9 */
