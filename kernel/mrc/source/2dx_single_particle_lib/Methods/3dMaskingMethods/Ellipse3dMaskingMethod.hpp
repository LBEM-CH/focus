#ifndef ELLIPSE3DMASKINGMETHOD_HPP_59G3DYSP
#define ELLIPSE3DMASKINGMETHOD_HPP_59G3DYSP

namespace SingleParticle2dx
{
	namespace Methods
	{
		class Ellipse3dMaskingMethod;
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
		class Ellipse3dMaskingMethod : public SingleParticle2dx::Methods::Abstract3dMaskingMethod
		{
			
		public:
			
			Ellipse3dMaskingMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context);
			
			virtual ~Ellipse3dMaskingMethod ();
			
			virtual void apply3dMask();

		};
		
	} /* Methods */
	
} /* SingleParticle2dx */

#endif /* end of include guard: ELLIPSEC3DMASKINGMETHOD_HPP_59G3DYSP */
