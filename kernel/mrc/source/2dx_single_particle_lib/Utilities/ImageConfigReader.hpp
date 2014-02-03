#ifndef IMAGECONFIGREADER_HPP_CKHSGSCT
#define IMAGECONFIGREADER_HPP_CKHSGSCT

namespace SingleParticle2dx
{
	namespace Utilities
	{
		class ImageConfigReader;
	}
}

#include <string>
#include <map>
#include "../Typedefs.hpp"


namespace SingleParticle2dx
{
	namespace Utilities
	{
		class ImageConfigReader
		{
		public:
			
			typedef SingleParticle2dx::value_type value_type;
			
			ImageConfigReader (std::string filename);
			virtual ~ImageConfigReader ();
			
			std::vector<value_type> getConfigElement (std::string key);
			
			void printImageConfigFile();

		private:
			std::map<std::string, std::vector<value_type> > m_data;
		};
		
	} /* Utilities */
	
} /* SingleParticle2dx */

#endif /* end of include guard: IMAGECONFIGREADER_HPP_CKHSGSCT */
