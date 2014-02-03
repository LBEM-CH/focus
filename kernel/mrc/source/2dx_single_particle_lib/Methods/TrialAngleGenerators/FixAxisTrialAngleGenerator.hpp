#ifndef FIXAXISTRIALANGLEGENERATOR_CPP_QLW6A971
#define FIXAXISTRIALANGLEGENERATOR_CPP_QLW6A971

namespace SingleParticle2dx
{
	namespace Methods
	{
		class FixAxisTrialAngleGenerator;
	}
}

#include "AbstractTrialAngleGenerator.hpp"


namespace SingleParticle2dx
{
	namespace Methods
	{
		class FixAxisTrialAngleGenerator : public SingleParticle2dx::Methods::AbstractTrialAngleGenerator
		{
			
		public:
		
			virtual void generateAngles(SingleParticle2dx::DataStructures::Orientation init_o, std::vector<SingleParticle2dx::DataStructures::Orientation>& o );
	
		};
		
	} /* Methods */
	
} /* SingleParticle2dx */

#endif /* end of include guard: FIXAXISTRIALANGLEGENERATOR_CPP_QLW6A971 */
