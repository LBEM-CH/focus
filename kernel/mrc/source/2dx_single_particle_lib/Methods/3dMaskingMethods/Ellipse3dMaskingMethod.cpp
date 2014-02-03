#include "Ellipse3dMaskingMethod.hpp"

SingleParticle2dx::Methods::Ellipse3dMaskingMethod::Ellipse3dMaskingMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context)
{
	m_context = context;
}
	
	
SingleParticle2dx::Methods::Ellipse3dMaskingMethod::~Ellipse3dMaskingMethod ()
{}
	

void SingleParticle2dx::Methods::Ellipse3dMaskingMethod::apply3dMask()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	size_type n = m_context->getSizeX();
	value_type pi = config->getPI();
	real_array3d_type data( boost::extents[n][n][n] );
	
	m_context->getRealSpaceData(data);
	SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&data);	

	value_type H = config->getReconstructionMaskingHeight() / 2;
	value_type dH = config->getReconstructionMaskingdH();	
	
	value_type current_H;
	value_type weight;
	
	value_type a = config->getMaskEllipseA();
	value_type b = config->getMaskEllipseB();
	value_type phi = config->getMaskEllipsePhi();
	
	Eigen::Matrix2f rot_mat;
	rot_mat(0,0) = cos(phi);
	rot_mat(0,1) = -sin(phi);
	rot_mat(1,0) = sin(phi);
	rot_mat(1,1) = cos(phi);
	
	Eigen::Vector2f vec;
	Eigen::Vector2f vec_rot;
	value_type r_sigma = 1.4;
	size_type k_rot, j_rot;
	
	for(size_type i=0; i<n; i++)
	{
		for(size_type j=0; j<n; j++)
		{
			for(size_type k=0; k<n; k++)
			{
				current_H = i-n/2;
				
				vec(0) = k-n/2;
				vec(1) = j-n/2;
				vec_rot = rot_mat * vec;
				
				k_rot = vec_rot(0);
				j_rot = vec_rot(1);
				
				if( fabs(current_H) <= H)
				{
					weight = 1;
				}
				else if ( fabs(current_H) <= (H+dH) )
				{
					weight = cos(((fabs(current_H)-H)/dH)*pi/2.0);
				}
				else
				{
					weight = 0;
				}
				
				if(weight > 0)
				{
					value_type r = (k_rot/a)*(k_rot/a) + (j_rot/b)*(j_rot/b);
				
					if (  r < 1 )
					{
						weight = 1;
					}
					else if (r < r_sigma)
					{
						weight = (r_sigma - r)* (1./(r_sigma-1));
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
