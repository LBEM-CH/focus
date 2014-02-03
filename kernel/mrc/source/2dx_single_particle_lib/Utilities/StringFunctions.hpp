#ifndef UTILITYFUNCTIONS_STRING_HPP_HEFZ7QX2
#define UTILITYFUNCTIONS_STRING_HPP_HEFZ7QX2

namespace SingleParticle2dx
{
	namespace Utilities
	{
		class StringFunctions;
	}
}


#include <vector>
#include <string>
#include <iostream>
#include <sstream>


namespace SingleParticle2dx
{
	namespace Utilities
	{
		class StringFunctions
		{
			
			public:
			
				static void splitString(std::vector<std::string>& vec_out, std::string string_in, std::string key);
						
				template<typename T>
				static std::string TtoString(T value)
				{
					std::stringstream ss;
					ss << value;
					return ss.str();
				}
				
		};
	}
}

#endif /* end of include guard: UTILITYFUNCTIONS_STRING_HPP_HEFZ7QX2 */
