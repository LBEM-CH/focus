#ifndef MERGEVARIABLEREADER_HPP_CKHSGSCT
#define MERGEVARIABLEREADER_HPP_CKHSGSCT

namespace SingleParticle2dx
{
	namespace Utilities
	{
		class MergeVariableReader;
	}
}


#include <string>
#include <map>
#include "../Typedefs.hpp"


namespace SingleParticle2dx
{
	namespace Utilities
	{
		class MergeVariableReader
		{
			
		public:
			
			typedef SingleParticle2dx::value_type value_type;
			
			
			MergeVariableReader (std::string filename);
			
			
			virtual ~MergeVariableReader ();
			
			
			std::vector<value_type> getFloatElement (std::string key);
			
			
			std::vector<std::string> getStringElement (std::string key);
			
			
			void printConfigFile();

		private:
			
			std::map<std::string, std::vector<value_type> > m_float_data;
			std::map<std::string, std::vector<std::string> > m_string_data;
		};
		
	} /* Utilities */
	
} /* SingleParticle2dx */

#endif /* end of include guard: MERGEVARIABLEREADER_HPP_CKHSGSCT */
