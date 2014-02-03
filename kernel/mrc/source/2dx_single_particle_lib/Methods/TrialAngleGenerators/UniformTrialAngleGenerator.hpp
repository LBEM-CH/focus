#ifndef UNIFORMTRIALANGLEGNERATORS_HPP_AH9DUG2V
#define UNIFORMTRIALANGLEGNERATORS_HPP_AH9DUG2V

namespace SingleParticle2dx
{
	namespace Methods
	{
		class UniformTrialAngleGenerator;
	}
}

#include "AbstractTrialAngleGenerator.hpp"


namespace SingleParticle2dx
{
	namespace Methods
	{
		class UniformTrialAngleGenerator : public SingleParticle2dx::Methods::AbstractTrialAngleGenerator
		{
			
		public:
		
			virtual void generateAngles(SingleParticle2dx::DataStructures::Orientation init_o, std::vector<SingleParticle2dx::DataStructures::Orientation>& o );
	
		};
		
	} /* Methods */
	
} /* SingleParticle2dx */


#endif /* end of include guard: UNIFORMTRIALANGLEGNERATORS_HPP_AH9DUG2V */
