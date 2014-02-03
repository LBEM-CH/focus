#ifndef SD3DMASKINGMETHOD_HPP_ENFW3Q7L
#define SD3DMASKINGMETHOD_HPP_ENFW3Q7L


namespace SingleParticle2dx
{
	namespace Methods
	{
		class SD3dMaskingMethod;
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
		class SD3dMaskingMethod : public SingleParticle2dx::Methods::Abstract3dMaskingMethod
		{

		public:

			SD3dMaskingMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context);
			
			virtual ~SD3dMaskingMethod ();
			
			virtual void apply3dMask();

		};
		
	} /* Methods */
	
} /* SingleParticle2dx */



#endif /* end of include guard: LAYER3DMASKINGMETHOD_HPP_7D6ERATG */

