#ifndef INPLANETRIALANGLEGENERATOR_HPP_YHHNCN4D
#define INPLANETRIALANGLEGENERATOR_HPP_YHHNCN4D

namespace SingleParticle2dx
{
	namespace Methods
	{
		class InPlaneTrialAngleGenerator;
	}
}


#include "AbstractTrialAngleGenerator.hpp"


namespace SingleParticle2dx
{
	namespace Methods
	{
		class InPlaneTrialAngleGenerator : public SingleParticle2dx::Methods::AbstractTrialAngleGenerator
		{
			
		public:
			
			virtual void generateAngles(SingleParticle2dx::DataStructures::Orientation init_o, std::vector<SingleParticle2dx::DataStructures::Orientation>& o );

		};
		
	} /* Methods */
	
} /* SingleParticle2dx */


#endif /* end of include guard: INPLANETRIALANGLEGENERATOR_HPP_YHHNCN4D */
