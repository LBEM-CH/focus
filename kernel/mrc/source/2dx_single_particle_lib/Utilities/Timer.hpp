#ifndef TIMER_HPP_EYXX4J5
#define TIMER_HPP_EYXX4J5

#include <sys/time.h>
#include <stdio.h>

namespace SingleParticle2dx
{
	namespace Utilities
	{
		class SystemTimer
		{
			
		public:
			
			SystemTimer()
			{
				m_timeStart = 0.0f;
				m_timeStart = GetTime();
			}
			
			float GetTime()
			{
				struct timeval tv;
				gettimeofday(&tv, 0);
				return (double) ((tv.tv_sec) * 1000000 + tv.tv_usec) * 1e-6 - m_timeStart;
			}
			
		private:
			float m_timeStart;
			
		};
		
	} /* Utilities */
	
} /* SingleParticle2dx */

#endif /* end of include guard: TIMER_HPP_EYXX4J5 */
