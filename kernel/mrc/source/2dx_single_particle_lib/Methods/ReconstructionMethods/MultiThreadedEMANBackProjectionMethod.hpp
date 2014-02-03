#ifndef MULTITHREADEDEMANBACKPROJECTION_HPP_UG9E3U8X
#define MULTITHREADEDEMANBACKPROJECTION_HPP_UG9E3U8X

namespace SingleParticle2dx
{
	namespace Methods
	{
		class MultiThreadedEMANBackprojectionMethod;
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
		class MultiThreadedEMANBackprojectionMethod : public SingleParticle2dx::Methods::AbstractReconstructionMethod
		{
			
		public:
		
			typedef SingleParticle2dx::fft_type fft_type;
			
			
			typedef SingleParticle2dx::real_array2d_type real_array2d_type;
			
			
			typedef SingleParticle2dx::real_array3d_type real_array3d_type;
			
			
			MultiThreadedEMANBackprojectionMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context);
			
			
			virtual ~MultiThreadedEMANBackprojectionMethod ();
			
			
			virtual void insertParticle(SingleParticle2dx::DataStructures::Particle& p);
			
			
			virtual void setupForBackProjection();
			
			
			virtual void finishReconstruction();
						
			
			virtual void insertData(SingleParticle2dx::DataStructures::ParticleContainer &c);
			
		private:
			
			std::vector<EMAN::Dict*> m_params;
			
			
			std::vector<EMAN::Reconstructor*> m_rec;
			
			
			std::vector<value_type> m_counter;
			
			
			size_type m_bad_counter;
		
		};
		
	} /* Methods */
	
} /* SingleParticle2dx */

#endif /* end of include guard: MULTITHREADEDEMANBACKPROJECTION_HPP_UG9E3U8X */
