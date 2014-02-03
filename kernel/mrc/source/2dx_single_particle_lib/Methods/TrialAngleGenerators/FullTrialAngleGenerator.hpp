#ifndef FULLTRIALANGLEGENERATOR_HPP_TXYVUZQ3
#define FULLTRIALANGLEGENERATOR_HPP_TXYVUZQ3

namespace SingleParticle2dx
{
	namespace Methods
	{
		class FullTrialAngleGenerator;
	}
}

#include "AbstractTrialAngleGenerator.hpp"


namespace SingleParticle2dx
{
	namespace Methods
	{
		class FullTrialAngleGenerator : public SingleParticle2dx::Methods::AbstractTrialAngleGenerator
		{
			
		public:
		
			virtual void generateAngles(SingleParticle2dx::DataStructures::Orientation init_o, std::vector<SingleParticle2dx::DataStructures::Orientation>& o );
	
		};
		
	} /* Methods */
	
} /* SingleParticle2dx */

#endif /* end of include guard: FULLTRIALANGLEGENERATOR_HPP_TXYVUZQ3 */
