#ifndef CLASSIFICATIONFUNCTIONS_HPP_HEFZ7QX2
#define CLASSIFICATIONFUNCTIONS_HPP_HEFZ7QX2

namespace SingleParticle2dx
{
	namespace Utilities
	{
		class ClassificationFunctions;
	}
}




#include "../Typedefs.hpp"
#include "../DataStructures.hpp"

namespace SingleParticle2dx
{
	namespace Utilities
	{
		class ClassificationFunctions
		{
			
		public:
		
			static void kMeans(SingleParticle2dx::DataStructures::ParticleContainer cont, size_type number_of_classes, std::vector<SingleParticle2dx::DataStructures::Particle>& result);
						
			
		};
		
	} /* Utilities */
	
} /* SingleParticle2dx */

#endif /* end of include guard: CLASSIFICATIONFUNCTIONS_HPP_HEFZ7QX2 */
