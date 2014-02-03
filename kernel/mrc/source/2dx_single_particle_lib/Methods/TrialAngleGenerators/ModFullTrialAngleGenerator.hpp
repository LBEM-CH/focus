#ifndef MODFULLTRIALANGLEGENERATOR_CPP_M43Y6J7E
#define MODFULLTRIALANGLEGENERATOR_CPP_M43Y6J7E

namespace SingleParticle2dx
{
	namespace Methods
	{
		class ModFullTrialAngleGenerator;
	}
}

#include "AbstractTrialAngleGenerator.hpp"


namespace SingleParticle2dx
{
	namespace Methods
	{
		class ModFullTrialAngleGenerator : public SingleParticle2dx::Methods::AbstractTrialAngleGenerator
		{
			
		public:
		
			virtual void generateAngles(SingleParticle2dx::DataStructures::Orientation init_o, std::vector<SingleParticle2dx::DataStructures::Orientation>& o );
	
		};
		
	} /* Methods */
	
} /* SingleParticle2dx */

#endif /* end of include guard: MODFULLTRIALANGLEGENERATOR_CPP_M43Y6J7E */
