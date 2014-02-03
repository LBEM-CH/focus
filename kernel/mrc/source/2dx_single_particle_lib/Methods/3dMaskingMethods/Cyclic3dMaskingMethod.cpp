#include "Cyclic3dMaskingMethod.hpp"

SingleParticle2dx::Methods::Cyclic3dMaskingMethod::Cyclic3dMaskingMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context)
{
	m_context = context;
}
	
	
SingleParticle2dx::Methods::Cyclic3dMaskingMethod::~Cyclic3dMaskingMethod ()
{}
	

void SingleParticle2dx::Methods::Cyclic3dMaskingMethod::apply3dMask()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	size_type n = m_context->getSizeX();
	value_type pi = config->getPI();
	real_array3d_type data( boost::extents[n][n][n] );
	
	m_context->getRealSpaceData(data);
	SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&data);	

	value_type R = config->getReconstructionMaskingRadius();
	value_type H = config->getReconstructionMaskingHeight() / 2;

	value_type dR = config->getReconstructionMaskingdR();
	value_type dH = config->getReconstructionMaskingdH();	
	value_type w;

	value_type current_R, current_H;
	value_type weight;
	
	for(size_type i=0; i<n; i++)
	{
		for(size_type j=0; j<n; j++)
		{
			for(size_type k=0; k<n; k++)
			{
				current_R = sqrt( (k-n/2)*(k-n/2) + (j-n/2)*(j-n/2));
				current_H = i-n/2;
		
			//	if ( !((fabs(current_R)<=(R-dR)) && (fabs(current_H)<=(H-dH))) )	
			//	{
			//		data[i][j][k] = value_type(0);
			//	}
			
				weight = 0;
				
				if( fabs(current_H) <= H)
				{
					weight = 1;
				}
				else if ( fabs(current_H) <= (H+dH) )
				{
					weight = cos(((fabs(current_H)-H)/dH)*pi/2.0);
				}
				
				if(weight > 0)
				{
					if( fabs(current_R) <= R)
					{
						weight *= 1;
					}
					else if( fabs(current_R) <= (R+dR) )
					{
						weight *= cos(((fabs(current_R)-R)/dR)*pi/2.0);
					}
					else
					{
						weight = 0;
					}
				}
				
				data[i][j][k] *= weight;
				
			
			}
		}
	}
	
	SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&data);
	m_context->setFourierSpaceData(data);
	
	return;
}
