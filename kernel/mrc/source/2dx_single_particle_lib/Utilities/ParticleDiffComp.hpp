#ifndef PARTICLEDIFFCOMP_HPP_B5U0WZ8Z
#define PARTICLEDIFFCOMP_HPP_B5U0WZ8Z

namespace SingleParticle2dx
{
	namespace Utilities
	{
		class ParticleDiffComp;
	}
}


#include "../DataStructures/Particle.hpp"

#include "../Typedefs.hpp"

namespace SingleParticle2dx
{
	namespace Utilities
	{
		class ParticleDiffComp
		{
		public:
			ParticleDiffComp (size_type x, size_type y)
			{
				m_x = x;
				m_y = y;
			}
			
			bool operator() (SingleParticle2dx::DataStructures::Particle* p1, SingleParticle2dx::DataStructures::Particle* p2)
			{
				value_type x1 = p1->getGlobalParticleInformation().getPositionX();
				value_type y1 = p1->getGlobalParticleInformation().getPositionY();
				
				value_type x2 = p2->getGlobalParticleInformation().getPositionX();
				value_type y2 = p2->getGlobalParticleInformation().getPositionY();
				
				value_type r1 = (x1-m_x)*(x1-m_x) + (y1-m_y)*(y1-m_y);
				value_type r2 = (x2-m_x)*(x2-m_x) + (y2-m_y)*(y2-m_y);
				
				return (r1<r2);
			}
			
			
		private:
			/* data */
			
			size_type m_x;
			size_type m_y;
			
		};
		
	} /* Utilities */
	
} /* SingleParticle2dx */

#endif /* end of include guard: PARTICLEDIFFCOMP_HPP_B5U0WZ8Z */
