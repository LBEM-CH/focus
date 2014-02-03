#include "Cos2dMaskingMethod.hpp"

#include "../../Config.hpp"

SingleParticle2dx::Methods::Cos2dMaskingMethod::Cos2dMaskingMethod (SingleParticle2dx::DataStructures::Abstract2dData* context)
{
	m_context = context;
}
	
	
SingleParticle2dx::Methods::Cos2dMaskingMethod::~Cos2dMaskingMethod ()
{
	
}
	

void SingleParticle2dx::Methods::Cos2dMaskingMethod::apply2dMask()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();

	size_type n = m_context->getSizeX();
	real_array2d_type data( boost::extents[n][n] );
	
	m_context->getRealSpaceData(&data);
	SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&data);	

	value_type R = config->getParticleMaskingRadius();
	value_type dR = config->getParticleMaskingdR();
	value_type current_R;
	value_type weight;
	
	for(size_type i=0; i<n; i++)
	{
		for(size_type j=0; j<n; j++)
		{
			current_R = sqrt( (i-n/2)*(i-n/2) + (j-n/2)*(j-n/2));
				
			if( fabs(current_R) <= R)
			{
				weight = 1;
			}
			else if( fabs(current_R) <= (R+dR) )
			{
				weight = cos(((fabs(current_R)-R)/dR)*pi/2.0);
			}
			else
			{
				weight = 0;
			}
			
			data[i][j] *= weight;
		}
	}
	
	SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&data);
	m_context->setFourierSpaceData(&data);	
}
