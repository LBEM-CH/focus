#ifndef FFTEMANBACKPROJECTIONMETHOD_HPP_OCUU8945
#define FFTEMANBACKPROJECTIONMETHOD_HPP_OCUU8945

namespace SingleParticle2dx
{
	namespace Methods
	{
		class FFTEMANBackprojectionMethod;
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
		class FFTEMANBackprojectionMethod : public SingleParticle2dx::Methods::AbstractReconstructionMethod
		{
			
		public:
		
			typedef SingleParticle2dx::fft_type fft_type;
			
			
			typedef SingleParticle2dx::real_array2d_type real_array2d_type;
			
			
			typedef SingleParticle2dx::real_array3d_type real_array3d_type;
			
			
			FFTEMANBackprojectionMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context);
			
			
			virtual ~FFTEMANBackprojectionMethod ();
			
		
			virtual void insertParticle(SingleParticle2dx::DataStructures::Particle& p);
			
			
			virtual void setupForBackProjection();
			virtual void finishReconstruction();
			virtual void insertData(SingleParticle2dx::DataStructures::ParticleContainer &c);
			
		private:
			
			EMAN::Transform t;
			EMAN::Dict rot;
			EMAN::Dict m_params;
			EMAN::Reconstructor* m_rec;
		
		};
		
	} /* Methods */
	
} /* SingleParticle2dx */

#endif /* end of include guard: FFTEMANBACKPROJECTIONMETHOD_HPP_OCUU8945 */
