#ifndef DUMMYTRIALANGLEGENERATOR_HPP_1M28QVV7
#define DUMMYTRIALANGLEGENERATOR_HPP_1M28QVV7


namespace SingleParticle2dx
{
	namespace Methods
	{
		class DummyTrialAngleGenerator;
	}
}

#include "AbstractTrialAngleGenerator.hpp"


namespace SingleParticle2dx
{
	namespace Methods
	{
		class DummyTrialAngleGenerator : public SingleParticle2dx::Methods::AbstractTrialAngleGenerator
		{
			
		public:
		
			virtual void generateAngles(SingleParticle2dx::DataStructures::Orientation init_o, std::vector<SingleParticle2dx::DataStructures::Orientation>& o );
	
		};
		
	} /* Methods */
	
} /* SingleParticle2dx */


#endif /* end of include guard: DUMMYTRIALANGLEGENERATOR_HPP_1M28QVV7 */
