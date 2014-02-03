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

#include "EMANProjectionMethod.hpp"
#include "../../Utilities.hpp"


SingleParticle2dx::Methods::EMANProjectionMethod::EMANProjectionMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context)
{
	//std::cout << "eman_proj constructed" << std::endl;
	
	m_context = context;
	
	EMAN::Dict params;
	m_proj = EMAN::Factory<EMAN::Projector>::get("standard");
	
	m_3dmodel = new EMAN::EMData;
}


SingleParticle2dx::Methods::EMANProjectionMethod::~EMANProjectionMethod ()
{
	std::cout << "eman_proj freed" << std::endl;
	delete m_proj;
	delete m_3dmodel;
}


void SingleParticle2dx::Methods::EMANProjectionMethod::calculateProjection(SingleParticle2dx::DataStructures::Orientation& o, SingleParticle2dx::DataStructures::Projection2d& p)
{
	EMAN::Dict params;
	
	EMAN::Transform* t = new EMAN::Transform;
	EMAN::Dict rot;
		
	rot["type"] = "spider";
//	rot["phi"] = o.getTLTAXIS() + 90;
//	rot["theta"] = o.getTLTANG();
//	rot["psi"] = o.getTAXA();
	
	rot["phi"] = o.getTAXA();
	rot["theta"] = o.getTLTANG();
	rot["psi"] = o.getTLTAXIS() + 90;

	t->set_rotation(rot);
	
	params["mode"] = 1;
	params["transform"] = t;
	
	m_proj->set_params(params);
	
	EMAN::EMData* eman_result;// = new EMAN::EMData;
	eman_result = m_proj->project3d(m_3dmodel);
		
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


void SingleParticle2dx::Methods::EMANProjectionMethod::prepareForProjections(SingleParticle2dx::DataStructures::ParticleContainer& cont)
{
	//std::cout << "eman_proj prepared" << std::endl;
	
	real_array3d_type rdata( boost::extents[m_context->getSizeX()][m_context->getSizeY()][m_context->getSizeZ()] );
	m_context->getRealSpaceData(rdata);
		
	float* float_data_3d = (float*) malloc(m_context->getSizeX() * m_context->getSizeY() * m_context->getSizeZ() * sizeof(float));
	std::copy(rdata.origin(), rdata.origin()+rdata.num_elements(), float_data_3d );
	
	delete m_3dmodel;
	m_3dmodel = new EMAN::EMData;
	
	m_3dmodel->set_data(float_data_3d, m_context->getSizeX(), m_context->getSizeY(), m_context->getSizeZ());
	
	//std::cout << "eman_proj prepared (done)" << std::endl;
}
