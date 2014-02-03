#ifndef MASKANDTHRESHOLD3DMETHOD_HPP_I5ZJN6W9
#define MASKANDTHRESHOLD3DMETHOD_HPP_I5ZJN6W9

namespace SingleParticle2dx
{
	namespace Methods
	{
		class MaskAndThreshold3dMethod;
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
		class MaskAndThreshold3dMethod : public SingleParticle2dx::Methods::Abstract3dMaskingMethod
		{
		
		public:
		
			MaskAndThreshold3dMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context);
			
			virtual ~MaskAndThreshold3dMethod ();
			
			virtual void apply3dMask();

		};
		
	} /* Methods */
	
} /* SingleParticle2dx */

#endif /* end of include guard: MASKANDTHRESHOLD3DMETHOD_HPP_I5ZJN6W9 */
