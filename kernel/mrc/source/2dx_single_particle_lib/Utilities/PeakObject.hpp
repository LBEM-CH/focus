#ifndef PEAKOBJECT_HPP_4B8D5NNU
#define PEAKOBJECT_HPP_4B8D5NNU

namespace SingleParticle2dx
{
	namespace Utilities
	{
		struct PeakObject;
	}
}

#include "../Typedefs.hpp"

namespace SingleParticle2dx
{
	namespace Utilities
	{
		struct PeakObject
		{
			typedef SingleParticle2dx::value_type value_type;
			value_type m_x;
			value_type m_y;
			value_type m_peak;
		};
		
		
		inline bool comparePeaks(PeakObject p1, PeakObject p2)
		{
			return (p1.m_peak >= p2.m_peak);
		}
		
	} /* Utilities */
	
} /* SingleParticle2dx */

#endif /* end of include guard: PEAKOBJECT_HPP_4B8D5NNU */
