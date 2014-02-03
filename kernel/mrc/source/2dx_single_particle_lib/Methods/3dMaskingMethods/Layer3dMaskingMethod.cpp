#include "Layer3dMaskingMethod.hpp"

SingleParticle2dx::Methods::Layer3dMaskingMethod::Layer3dMaskingMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context)
{
	m_context = context;
}
	
	
SingleParticle2dx::Methods::Layer3dMaskingMethod::~Layer3dMaskingMethod ()
{}
	

void SingleParticle2dx::Methods::Layer3dMaskingMethod::apply3dMask()
{
	size_type n = m_context->getSizeX();
	real_array3d_type data( boost::extents[n][n][n] );
	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	m_context->getRealSpaceData(data);
	SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&data);
	
	value_type H = config->getReconstructionMaskingHeight() / 2;
	
	value_type current_H;
	
	for(size_type i=0; i<n; i++)
	{
		for(size_type j=0; j<n; j++)
		{
			for(size_type k=0; k<n; k++)
			{
				current_H = i-n/2;
				
				if ( !(fabs(current_H)<=H) ) 
				{
					data[i][j][k] = value_type(0);
				}
			}
		}
	}
	
	SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&data);
	m_context->setFourierSpaceData(data);
	
	return;
}