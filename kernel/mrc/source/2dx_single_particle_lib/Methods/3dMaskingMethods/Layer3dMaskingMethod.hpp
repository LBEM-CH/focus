#ifndef LAYER3DMASKINGMETHOD_HPP_7D6ERATG
#define LAYER3DMASKINGMETHOD_HPP_7D6ERATG


namespace SingleParticle2dx
{
	namespace Methods
	{
		class Layer3dMaskingMethod;
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
		class Layer3dMaskingMethod : public SingleParticle2dx::Methods::Abstract3dMaskingMethod
		{

		public:

			Layer3dMaskingMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context);
			
			virtual ~Layer3dMaskingMethod ();
			
			virtual void apply3dMask();

		};
		
	} /* Methods */
	
} /* SingleParticle2dx */



#endif /* end of include guard: LAYER3DMASKINGMETHOD_HPP_7D6ERATG */
