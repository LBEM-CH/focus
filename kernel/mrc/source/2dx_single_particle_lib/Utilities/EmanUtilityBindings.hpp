#ifndef EMANUTILITYBINDINGS_HPP_P7C7DVAD
#define EMANUTILITYBINDINGS_HPP_P7C7DVAD

namespace SingleParticle2dx
{
	namespace Utilities
	{
		class EmanUtilityBindings;
	}
}

#include "../DataStructures/Orientation.hpp"
#include "../Typedefs.hpp"


namespace SingleParticle2dx
{
	namespace Utilities
	{
		class EmanUtilityBindings
		{
		public:
			
			typedef SingleParticle2dx::value_type value_type;
			
			static void rotate(SingleParticle2dx::DataStructures::Orientation& o, value_type d_phi, value_type d_theta, value_type d_psi);
			
		};
	}
}




#endif /* end of include guard: EMANUTILITYBINDINGS_HPP_P7C7DVAD */
