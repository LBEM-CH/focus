#ifndef CONVANALYZER_HPP_PPG14OF2
#define CONVANALYZER_HPP_PPG14OF2

namespace SingleParticle2dx
{
	namespace Utilities
	{
		class ConvAnalyzer;
	}
}

#include <vector>

#include "../Typedefs.hpp"


namespace SingleParticle2dx
{
	namespace Utilities
	{
		class ConvAnalyzer
		{
		public:
			
			typedef SingleParticle2dx::value_type value_type;
			typedef SingleParticle2dx::size_type size_type;
			
			ConvAnalyzer (value_type t, size_type n);
			virtual ~ConvAnalyzer ();
			
			void addChange(value_type rhs);
			void reset();
			
			bool isConverged();
			bool isCycling();

		private:
			
			std::vector<value_type> m_data;
			
			value_type m_conv_threshold;
			value_type m_cyclic_number;
			
		};
	}
}


#endif /* end of include guard: CONVANALYZER_HPP_PPG14OF2 */
