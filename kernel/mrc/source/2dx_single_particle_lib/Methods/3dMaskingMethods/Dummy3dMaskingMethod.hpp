#ifndef DUMMY3DMASKINGMETHOD_HPP_SKN1RRHF
#define DUMMY3DMASKINGMETHOD_HPP_SKN1RRHF

namespace SingleParticle2dx
{
	namespace Methods
	{
		class Dummy3dMaskingMethod;
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
		class Dummy3dMaskingMethod : public SingleParticle2dx::Methods::Abstract3dMaskingMethod
		{

		public:

			Dummy3dMaskingMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context);
			
			virtual ~Dummy3dMaskingMethod ();
			
			virtual void apply3dMask();

		};
		
	} /* Methods */
	
} /* SingleParticle2dx */

#endif /* end of include guard: DUMMY3DMASKINGMETHOD_HPP_SKN1RRHF */
