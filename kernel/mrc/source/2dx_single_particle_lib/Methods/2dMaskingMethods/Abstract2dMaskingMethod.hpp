#ifndef ABSTRACT2DMASKINGMETHODS_HPP_2R4L7X0H
#define ABSTRACT2DMASKINGMETHODS_HPP_2R4L7X0H


namespace SingleParticle2dx
{
	namespace Methods
	{
		class Abstract2dMaskingMethod;
	}
}


namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class Orientation;
		class Projection2d;
		class Reconstruction3d;
		class Abstract2dData;
	} 
}

#include <boost/serialization/base_object.hpp>


#include "../../Typedefs.hpp"


namespace SingleParticle2dx
{
	namespace Methods
	{
		class Abstract2dMaskingMethod
		{
		public:
			
			typedef SingleParticle2dx::real_array2d_type real_array_type;
			typedef SingleParticle2dx::size_type size_type;
			typedef SingleParticle2dx::value_type value_type;
						
			virtual void apply2dMask() = 0;

		protected:
			
			/** Context from which the method is called */
			SingleParticle2dx::DataStructures::Abstract2dData* m_context;
	
		};
		
	} /* Methods */
	
} /* SingleParticle2dx */

#endif /* end of include guard: ABSTRACT2DMASKINGMETHODS_HPP_2R4L7X0H */
