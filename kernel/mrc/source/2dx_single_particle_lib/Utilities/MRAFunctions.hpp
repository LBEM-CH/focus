#ifndef MRAFUNCTIONS_HPP_HEFZ7QX2
#define MRAFUNCTIONS_HPP_HEFZ7QX2

namespace SingleParticle2dx
{
	namespace Utilities
	{
		class MRAFunctions;
	}
}


#include <eigen3/Eigen/Dense>

#include "../Typedefs.hpp"
#include "../DataStructures.hpp"

namespace SingleParticle2dx
{
	namespace Utilities
	{
		class MRAFunctions
		{
			
		public:
			
			typedef SingleParticle2dx::value_type value_type;
			typedef SingleParticle2dx::size_type size_type;
			typedef SingleParticle2dx::real_array2d_type real_array2d_type;
			
			static void calculateMRAReconstruction ( SingleParticle2dx::DataStructures::ParticleContainer& cont, std::vector<SingleParticle2dx::DataStructures::Reconstruction3d>& rec_vec);
		};
		
	} /* Utilities */
	
} /* SingleParticle2dx */

#endif /* end of include guard: MRAFUNCTIONS_HPP_HEFZ7QX2 */
