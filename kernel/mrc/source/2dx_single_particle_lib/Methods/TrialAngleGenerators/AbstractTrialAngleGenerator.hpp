#ifndef ABSTRACTTRIALANGLEGNERATOR_HPP_3VUDOZ0V
#define ABSTRACTTRIALANGLEGNERATOR_HPP_3VUDOZ0V

namespace SingleParticle2dx
{
	namespace Methods
	{
		class AbstractTrialAngleGenerator;
	}
}

namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class Orientation;
	}
}


#include "../../Typedefs.hpp"


namespace SingleParticle2dx
{
	namespace Methods
	{
		class AbstractTrialAngleGenerator
		{
			
		public:
			
			typedef SingleParticle2dx::value_type value_type;
			
			//AbstractTrialAngleGenerator ();
			//virtual ~AbstractTrialAngleGenerator ();
			
			virtual void generateAngles(SingleParticle2dx::DataStructures::Orientation init_o, std::vector<SingleParticle2dx::DataStructures::Orientation>& o ) = 0;

		};
		
	} /* Methods */
	
} /* SingleParticle2dx */

#endif /* end of include guard: ABSTRACTTRIALANGLEGNERATOR_HPP_3VUDOZ0V */
