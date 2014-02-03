#ifndef SPHERICAL3DMASKINGMETHOD_HPP_59G3DYSP
#define SPHERICAL3DMASKINGMETHOD_HPP_59G3DYSP

namespace SingleParticle2dx
{
	namespace Methods
	{
		class Sperical3dMaskingMethod;
	}
}


#include "Abstract3dMaskingMethod.hpp"
#include "../../DataStructures.hpp"
#include "../../Typedefs.hpp"


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
		class Spherical3dMaskingMethod : public SingleParticle2dx::Methods::Abstract3dMaskingMethod
		{
			
		public:
			
			Spherical3dMaskingMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context);
			
			virtual ~Spherical3dMaskingMethod ();
			
			virtual void apply3dMask();

		};
		
	} /* Methods */
	
} /* SingleParticle2dx */

#endif /* end of include guard: SPHERICAL3DMASKINGMETHOD_HPP_59G3DYSP */
