#ifndef ABSTRACTRECONSTRUCTIONMETHOD_HPP_VGDRRYHP
#define ABSTRACTRECONSTRUCTIONMETHOD_HPP_VGDRRYHP

namespace SingleParticle2dx
{
	namespace Methods
	{
		class AbstractReconstructionMethod;
	}
}


namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class ParticleContainer;
		class Reconstruction3d;
		class Particle;
	} 
}

#include <boost/scoped_ptr.hpp>

#include "../../Typedefs.hpp"

namespace SingleParticle2dx
{
	namespace Methods
	{
		class AbstractReconstructionMethod
		{
			
		public:
			
			/** value type */
			typedef SingleParticle2dx::value_type value_type;
			
			
			/** size type */
			typedef SingleParticle2dx::size_type size_type;
			
			
			virtual void setupForBackProjection() = 0;
			
			
			virtual void finishReconstruction() = 0;
						
			
			virtual void insertData(SingleParticle2dx::DataStructures::ParticleContainer &c) = 0;
			
			
			void updateReconstruction(SingleParticle2dx::DataStructures::ParticleContainer &c)
			{
				this->setupForBackProjection();
				this->insertData(c);
				this->finishReconstruction();
			}
			
			
		protected:

			/** Context from which the method is called */
			SingleParticle2dx::DataStructures::Reconstruction3d* m_context;

		};
		
	} /* Methods */
	
} /* SingleParticle2dx */

#endif /* end of include guard: ABSTRACTRECONSTRUCTIONMETHOD_HPP_VGDRRYHP */
