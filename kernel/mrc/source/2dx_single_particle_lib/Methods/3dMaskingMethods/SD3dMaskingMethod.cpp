#include "SD3dMaskingMethod.hpp"

SingleParticle2dx::Methods::SD3dMaskingMethod::SD3dMaskingMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context)
{
	m_context = context;
}
	
	
SingleParticle2dx::Methods::SD3dMaskingMethod::~SD3dMaskingMethod ()
{}
	

void SingleParticle2dx::Methods::SD3dMaskingMethod::apply3dMask()
{
	size_type n = m_context->getSizeX();
	real_array3d_type data( boost::extents[n][n][n] );
	
	m_context->getRealSpaceData(data);
	SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&data);
	
	value_type H = 23;
	
	value_type current_H;
	
	for(size_type i=0; i<n; i++)
	{
		for(size_type j=0; j<n; j++)
		{
			for(size_type k=0; k<n; k++)
			{
				if(data[i][j][k] < value_type(0))
				{
					data[i][j][k] = 0;
				}
			}
		}
	}
	
	SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&data);
	m_context->setFourierSpaceData(data);
	
	return;
}