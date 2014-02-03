#ifndef CLUSTERUTILITYFUNCTIONS_HPP_HEFZ7QX2
#define CLUSTERUTILITYFUNCTIONS_HPP_HEFZ7QX2

namespace SingleParticle2dx
{
	namespace Utilities
	{
		class ClusterUtilityFunctions;
	}
}

#include <vector>

#include "../Typedefs.hpp"


namespace SingleParticle2dx
{
	namespace Utilities
	{
		class ClusterUtilityFunctions
		{
			public:
			
				typedef SingleParticle2dx::size_type size_type;
			
				static void printLoad(std::vector<std::vector<size_type> > load);
				static void balanceLoad_naiv(size_type np, size_type n_images, std::vector<std::vector<int> >& load);
				static void balanceLoad_sizedep(size_type np, size_type n_images, std::vector<std::string> folder_content, std::vector<std::vector<size_type> >& load);
				static void balanceLoad_optimal_sizedep(size_type np, size_type n_images, std::vector<std::string> folder_content, std::vector<std::vector<size_type> >& load);
				
				
			protected:
				
				static size_type findSmallestIndex(std::vector<size_type>& vec);
				
				static bool custumComperator (std::pair<size_type,size_type> i,std::pair<size_type,size_type> j)
				{
					return (i.second>j.second);
				}
				

		};
	}
}

#endif
