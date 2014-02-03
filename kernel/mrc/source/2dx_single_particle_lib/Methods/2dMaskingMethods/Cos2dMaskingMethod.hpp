#ifndef COS2DMASKINGMETHOD_HPP_FTWKLG8M
#define COS2DMASKINGMETHOD_HPP_FTWKLG8M

namespace SingleParticle2dx
{
	namespace Methods
	{
		class Cos2dMaskingMethod;
	}
}


#include "Abstract2dMaskingMethod.hpp"
#include "../../DataStructures.hpp"
#include "../../Typedefs.hpp"
//#include "../../Methods.hpp"


namespace SingleParticle2dx
{
	namespace Methods
	{
		class Abstract2dMaskingMethod;
	}
}


namespace SingleParticle2dx
{
	namespace Methods
	{
		class Cos2dMaskingMethod : public SingleParticle2dx::Methods::Abstract2dMaskingMethod
		{
		public:
			
			Cos2dMaskingMethod (SingleParticle2dx::DataStructures::Abstract2dData* context);
			
			virtual ~Cos2dMaskingMethod ();
			
			virtual void apply2dMask();

		};
		
	} /* Methods */
	
} /* SingleParticle2dx */

#endif /* end of include guard: COS2DMASKINGMETHOD_HPP_FTWKLG8M */
