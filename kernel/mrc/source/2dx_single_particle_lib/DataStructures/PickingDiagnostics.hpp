#ifndef PICKINGDIAGNOSTICS_HPP_KYUFCFVH
#define PICKINGDIAGNOSTICS_HPP_KYUFCFVH

#include "../Typedefs.hpp"

namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class PickingDiagnostics
		{
		public:
			
			typedef SingleParticle2dx::size_type size_type;
			
			PickingDiagnostics ();
			virtual ~PickingDiagnostics ();
			
			void addGoodParticle();
			
			
			void addBadSdParticle();
			
			
			void addTooLowSdParticle();
			
			
			void addTooLowRelativeSdParticle();
			
			
			void addFailingParticle();
			
			
			size_type getNumberOfGoodParticles();
			
			
			void print(size_type level=1);

		private:
			
			std::vector<size_type> m_data;
			
		};
		
	} /* DataStructures */
	
} /* SingleParticle2dx */

#endif /* end of include guard: PICKINGDIAGNOSTICS_HPP_KYUFCFVH */
