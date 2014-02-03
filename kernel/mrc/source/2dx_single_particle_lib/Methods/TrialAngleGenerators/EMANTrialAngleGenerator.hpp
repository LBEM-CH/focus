#ifndef EMANTRIALANGLEGENERATOR_HPP_Q5Q2CL1D
#define EMANTRIALANGLEGENERATOR_HPP_Q5Q2CL1D

namespace SingleParticle2dx
{
	namespace Methods
	{
		class EMANTrialAngleGenerator;
	}
}


#include "AbstractTrialAngleGenerator.hpp"


namespace SingleParticle2dx
{
	namespace Methods
	{
		class EMANTrialAngleGenerator : public SingleParticle2dx::Methods::AbstractTrialAngleGenerator
		{
			
		public:
			
			virtual void generateAngles(SingleParticle2dx::DataStructures::Orientation init_o, std::vector<SingleParticle2dx::DataStructures::Orientation>& o );

		};
		
	} /* Methods */
	
} /* SingleParticle2dx */

#endif /* end of include guard: EMANTRIALANGLEGENERATOR_HPP_Q5Q2CL1D */
