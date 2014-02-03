#ifndef ABSTRACT3DMASKINGMETHOD_HPP_4Y6DSPUK
#define ABSTRACT3DMASKINGMETHOD_HPP_4Y6DSPUK

namespace SingleParticle2dx
{
	namespace Methods
	{
		class Abstract3dMaskingMethod;
	}
}


namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class Orientation;
		class Projection2d;
		class Reconstruction3d;
	} 
}

#include "../../Typedefs.hpp"

namespace SingleParticle2dx
{
	namespace Methods
	{
		class Abstract3dMaskingMethod
		{
			
		public:
			
			typedef SingleParticle2dx::real_array3d_type real_array3d_type;
			typedef SingleParticle2dx::size_type size_type;
			typedef SingleParticle2dx::value_type value_type;
						
			virtual void apply3dMask() = 0;

		protected:
			
			/** Context from which the method is called */
			SingleParticle2dx::DataStructures::Reconstruction3d* m_context;
		
		};
		
	} /* Methods */
	
} /* SingleParticle2dx */

#endif /* end of include guard: ABSTRACT3DMASKINGMETHOD_HPP_4Y6DSPUK */
