#ifndef INTERFACETOSTRING_HPP_4LF8LH2X
#define INTERFACETOSTRING_HPP_4LF8LH2X


namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class InterfaceToString
		{
		public:
			
			virtual std::string getDataString() = 0;
			
			
			virtual std::string getDescString() = 0;
			
		};
		
	} /* DataStructures */
	
} /* SingleParticle2dx */

#endif /* end of include guard: INTERFACETOSTRING_HPP_4LF8LH2X */
