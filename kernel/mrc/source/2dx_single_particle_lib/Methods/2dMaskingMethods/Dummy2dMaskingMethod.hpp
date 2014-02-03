#ifndef DUMMY2DMASKINGMETHOD_HPP_6TBB2L86
#define DUMMY2DMASKINGMETHOD_HPP_6TBB2L86


namespace SingleParticle2dx
{
	namespace Methods
	{
		class Dummy2dMaskingMethod;
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
		class Dummy2dMaskingMethod : public SingleParticle2dx::Methods::Abstract2dMaskingMethod
		{
		public:
			
			Dummy2dMaskingMethod (SingleParticle2dx::DataStructures::Abstract2dData* context);
			
			virtual ~Dummy2dMaskingMethod ();
			
			virtual void apply2dMask();

		};
		
	} /* Methods */
	
} /* SingleParticle2dx */

#endif /* end of include guard: DUMMY2DMASKINGMETHOD_HPP_6TBB2L86 */
