#ifndef BACKPROJECTIONRECONSTRUCTIONMETHOD_HPP_IBW48SMH
#define BACKPROJECTIONRECONSTRUCTIONMETHOD_HPP_IBW48SMH

namespace SingleParticle2dx
{
	namespace Methods
	{
		class BackprojectionReconstructionMethod;
	}
}

#include <eigen3/Eigen/Dense>

#include "AbstractReconstructionMethod.hpp"
#include "../../Typedefs.hpp"

namespace SingleParticle2dx
{
	namespace Methods
	{
		class BackprojectionReconstructionMethod : public SingleParticle2dx::Methods::AbstractReconstructionMethod
		{
		public:
			/** Type of a generic rotation */
			typedef Eigen::AngleAxis<value_type> Rotation;
		
		
			/** Type of a 3d vector */
			typedef Eigen::Vector3f Vector3d;

			
			typedef SingleParticle2dx::fft_type fft_type;

			
			typedef SingleParticle2dx::real_array3d_type real_array3d_type;

			
			BackprojectionReconstructionMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context);


			virtual ~BackprojectionReconstructionMethod ();

			
			virtual void insertParticle(SingleParticle2dx::DataStructures::Particle& p);

			
			std::pair<size_type, size_type> calcLowerAndUpperIndex( value_type xobs );

			
			void adjust3DIndices( std::vector<size_type>& ind );

			
			value_type calcInterpolationWeight(std::vector<value_type>& ind);
			
			
			virtual void setupForBackProjection();
			
			
			virtual void finishReconstruction();
						
			
			virtual void insertData(SingleParticle2dx::DataStructures::ParticleContainer &c);
			
			
		private:

			boost::scoped_ptr<real_array3d_type> m_norm_volume;
		
		};
		
	} /* Methods */
	
} /* SingleParticle2dx */

#endif /* end of include guard: BACKPROJECTIONRECONSTRUCTIONMETHOD_HPP_IBW48SMH */
