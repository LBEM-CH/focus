#ifndef ROTTRIALANGLEGENERATOR_HPP_5KQNAINB
#define ROTTRIALANGLEGENERATOR_HPP_5KQNAINB


namespace SingleParticle2dx
{
	namespace Methods
	{
		class RotTrialAngleGenerator;
	}
}


#include "AbstractTrialAngleGenerator.hpp"


namespace SingleParticle2dx
{
	namespace Methods
	{
		class RotTrialAngleGenerator : public SingleParticle2dx::Methods::AbstractTrialAngleGenerator
		{
			
		public:
			
			virtual void generateAngles(SingleParticle2dx::DataStructures::Orientation init_o, std::vector<SingleParticle2dx::DataStructures::Orientation>& o );

		};
		
	} /* Methods */
	
} /* SingleParticle2dx */

#endif /* end of include guard: ROTTRIALANGLEGENERATOR_HPP_5KQNAINB */
