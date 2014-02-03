#ifndef AVERAGEWEIGHTER_HPP_NIDU6HD5
#define AVERAGEWEIGHTER_HPP_NIDU6HD5

namespace SingleParticle2dx
{
	namespace Utilities
	{
		class AverageWeighter;
	}
}

#include "../Typedefs.hpp"



namespace SingleParticle2dx
{
	namespace Utilities
	{
		class AverageWeighter
		{
		public:
			
			typedef SingleParticle2dx::value_type value_type;
			typedef SingleParticle2dx::size_type size_type;
			
			AverageWeighter(size_type mode, size_type n, value_type tilt);
			
			value_type getWeight();
			
		private:
			
			size_type m_mode;
			size_type m_n;
			value_type m_tilt;
			
		};
	}
}


#endif /* end of include guard: AVERAGEWEIGHTER_HPP_NIDU6HD5 */
