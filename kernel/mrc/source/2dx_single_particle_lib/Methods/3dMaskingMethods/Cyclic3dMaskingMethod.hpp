#ifndef CYCLIC3DMASKINGMETHOD_HPP_59G3DYSP
#define CYCLIC3DMASKINGMETHOD_HPP_59G3DYSP

namespace SingleParticle2dx
{
	namespace Methods
	{
		class Cyclic3dMaskingMethod;
	}
}


#include "Abstract3dMaskingMethod.hpp"
#include "../../DataStructures.hpp"
#include "../../Typedefs.hpp"
//#include "../../Methods.hpp"


namespace SingleParticle2dx
{
	namespace Methods
	{
		class Abstract3dMaskingMethod;
	}
}


namespace SingleParticle2dx
{
	namespace Methods
	{
		class Cyclic3dMaskingMethod : public SingleParticle2dx::Methods::Abstract3dMaskingMethod
		{
			
		public:
			
			Cyclic3dMaskingMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context);
			
			virtual ~Cyclic3dMaskingMethod ();
			
			virtual void apply3dMask();

		};
		
	} /* Methods */
	
} /* SingleParticle2dx */

#endif /* end of include guard: CYCLIC3DMASKINGMETHOD_HPP_59G3DYSP */
