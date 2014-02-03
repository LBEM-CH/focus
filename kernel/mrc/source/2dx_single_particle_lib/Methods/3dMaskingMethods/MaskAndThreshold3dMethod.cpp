#include "MaskAndThreshold3dMethod.hpp"
#include "../../Utilities.hpp"
#include "../../Config.hpp"


SingleParticle2dx::Methods::MaskAndThreshold3dMethod::MaskAndThreshold3dMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context)
{
	m_context = context;
}
	
	
SingleParticle2dx::Methods::MaskAndThreshold3dMethod::~MaskAndThreshold3dMethod ()
{}
	

void SingleParticle2dx::Methods::MaskAndThreshold3dMethod::apply3dMask()
{
	std::cout << "apply mask, lowpass and threshold" << std::endl;
	
	m_context->applyLowPassFilter();
	SingleParticle2dx::Methods::Cyclic3dMaskingMethod mask_cyclic(m_context);
	mask_cyclic.apply3dMask();

	size_type n = m_context->getSizeX();
	real_array3d_type data( boost::extents[n][n][n] );
	real_array3d_type flag( boost::extents[n][n][n] );
	
	m_context->getRealSpaceData(data);
	SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&data);
	
	for(size_type i=0; i<m_context->getSizeX(); i++)
	{
		for(size_type j=0; j<m_context->getSizeY(); j++)
		{
			for(size_type k=0; k<m_context->getSizeZ(); k++)
			{
				if ( data[i][j][k] > value_type(1) )
				{
					flag[i][j][k] = value_type(1);
				}
				else
				{
					flag[i][j][k] = value_type(0);
				}
			}
		}
	}
			
	value_type rad;
	value_type edge;
	value_type pi = SingleParticle2dx::ConfigContainer::Instance()->getPI();
	for(size_type i=0; i<m_context->getSizeX(); i++)
	{
		for(size_type j=0; j<m_context->getSizeY(); j++)
		{
			for(size_type k=0; k<m_context->getSizeZ(); k++)
			{
				if(flag[i][j][k] > 0.99)
				{
					for(size_type ii=-5; ii<=5; ii++)
					{
						for(size_type jj=-5; jj<=5; jj++)
						{
							for(size_type kk=-5; kk<=5; kk++)
							{
								rad = sqrtf(static_cast<value_type>( ii*ii + jj*jj + kk*kk ) );
								edge = (1. + cos(rad/5 * pi )) / value_type(2);
								if ( (i+ii>=0) && (j+jj>=0) && (k+kk>=0) && (i+ii<n) && (j+jj<n) && (k+kk<n) )
								{
									if ( flag[i+ii][j+jj][k+kk] < edge )
									{
										flag[i+ii][j+jj][k+kk] = edge;
									}
								}
							}
						}
					}
				}
			}
		}
	}
	
	SingleParticle2dx::Utilities::MRCFileIO::writeToMrc(&flag, "flag.mrc");
		
	value_type mean_value = 0;
	size_type num = 0;
	for(size_type i=0; i<m_context->getSizeX(); i++)
	{
		for(size_type j=0; j<m_context->getSizeY(); j++)
		{
			for(size_type k=0; k<m_context->getSizeZ(); k++)
			{
				if( (flag[i][j][k] <= value_type(1)) && (flag[i][j][k]>1e-5) )
				{
					mean_value += data[i][j][k];
					num++;
				}
			}
		}
	}
	
	mean_value /= num;
	
	for(size_type i=0; i<m_context->getSizeX(); i++)
	{
		for(size_type j=0; j<m_context->getSizeY(); j++)
		{
			for(size_type k=0; k<m_context->getSizeZ(); k++)
			{
				data[i][j][k] = data[i][j][k]*flag[i][j][k] + (1-flag[i][j][k])*mean_value;
			}
		}
	}
	
	SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&data);
	m_context->setFourierSpaceData( data );
}
