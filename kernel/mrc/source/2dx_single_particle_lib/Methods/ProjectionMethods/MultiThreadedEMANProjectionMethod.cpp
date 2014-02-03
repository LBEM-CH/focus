/*
 *  Copyright (C) 2012 by C-Cina University of Basel
 *  www.c-cina.unibas.ch
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the
 *  Free Software Foundation, Inc.,
 *  59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

#include <omp.h>

#include "MultiThreadedEMANProjectionMethod.hpp"
#include "../../Utilities.hpp"


SingleParticle2dx::Methods::MultiThreadedEMANProjectionMethod::MultiThreadedEMANProjectionMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context)
{
	std::cout << "parallel eman_proj constructed" << std::endl;
	
	m_context = context;
	
	size_type nt = omp_get_num_procs();
	std::cout << "generating MultiThreadEMANProj for " << nt << " threads" << std::endl;
	size_type size = m_context->getSizeX() * m_context->getSizeY() * m_context->getSizeZ();
	
	for (size_type i=0; i<nt; i++)
	{
		m_proj.push_back(EMAN::Factory<EMAN::Projector>::get("fourier_gridding"));
		//m_float_data_3d.push_back((float*) malloc(size * sizeof(float)));
		m_3dmodel.push_back(new EMAN::EMData);
	}
	
	/*
	EMAN::Dict params;
	m_proj = EMAN::Factory<EMAN::Projector>::get("standard");
	
	size_type size = m_context->getSizeX() * m_context->getSizeY() * m_context->getSizeZ();
	m_float_data_3d = (float*) malloc(size * sizeof(float));
	m_3dmodel = new EMAN::EMData;
	*/
}


SingleParticle2dx::Methods::MultiThreadedEMANProjectionMethod::~MultiThreadedEMANProjectionMethod ()
{
	/*
	std::cout << "eman_proj freed" << std::endl;
	delete m_proj;
	delete m_3dmodel;
	free(m_float_data_3d);
	*/
}


void SingleParticle2dx::Methods::MultiThreadedEMANProjectionMethod::calculateProjection(SingleParticle2dx::DataStructures::Orientation& o, SingleParticle2dx::DataStructures::Projection2d& p)
{
	size_type thread = omp_get_thread_num();
	
	EMAN::Dict params;
	
	EMAN::Transform* t = new EMAN::Transform;
	EMAN::Dict rot;
		
	Eigen::Matrix3f rot_matrix = SingleParticle2dx::Utilities::UtilityFunctions::determineRotation( o );
	
	rot["type"] = "matrix";
	rot["m11"] = rot_matrix(0,0);
	rot["m12"] = rot_matrix(0,1);
	rot["m13"] = rot_matrix(0,2);
	rot["m21"] = rot_matrix(1,0);
	rot["m22"] = rot_matrix(1,1);
	rot["m23"] = rot_matrix(1,2);
	rot["m31"] = rot_matrix(2,0);
	rot["m32"] = rot_matrix(2,1);
	rot["m33"] = rot_matrix(2,2);
		
	t->set_rotation(rot);
	
	params["transform"] = t;
	params["kb_alpha"] = 0;
	params["kb_K"] = 0;
	params["angletype"] = "matrix";
	
	m_proj[thread]->set_params(params);
	
	EMAN::EMData* eman_result;
	eman_result = m_proj[thread]->project3d(m_3dmodel[thread]);
		
	size_type size2d = m_context->getSizeX() * m_context->getSizeY();
	real_array2d_type rdata( boost::extents[m_context->getSizeX()][m_context->getSizeY()] );
	std::copy(eman_result->get_data(), eman_result->get_data()+size2d, rdata.origin() );
	
	SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&rdata);
	p.setFourierSpaceData(&rdata);
	p.applyLowPassFilter();
	p.applyMask();
	p.setOrientation(o);
	
	delete eman_result;
	delete t;
}


void SingleParticle2dx::Methods::MultiThreadedEMANProjectionMethod::prepareForProjections(SingleParticle2dx::DataStructures::ParticleContainer& cont)
{
	size_type nt = omp_get_num_procs();
	size_type size = m_context->getSizeX() * m_context->getSizeY() * m_context->getSizeZ();
	
	real_array3d_type rdata( boost::extents[m_context->getSizeX()][m_context->getSizeY()][m_context->getSizeZ()] );
	m_context->getRealSpaceData(rdata);
	
	std::cout << "setting up MultiThreadEMANProj for " << nt << " threads" << std::endl;
	
	#pragma omp parallel for
	for (size_type i=0; i<nt; i++)
	{
		float* float_data_3d = (float*) malloc(m_context->getSizeX() * m_context->getSizeY() * m_context->getSizeZ() * sizeof(float));
		std::copy(rdata.origin(), rdata.origin()+rdata.num_elements(), float_data_3d );
		m_3dmodel[i]->set_data(float_data_3d, m_context->getSizeX(), m_context->getSizeY(), m_context->getSizeZ());
	}
}
