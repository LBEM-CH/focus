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


#include "EMANBackprojectionMethod.hpp"

#include "../../Utilities.hpp"

#include <transform.h>

SingleParticle2dx::Methods::EMANBackprojectionMethod::EMANBackprojectionMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context)
{
	m_context = context;

	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
			
	m_params["size"] = m_context->getSizeX();
	m_params["zsample"] = m_context->getSizeX();
	m_params["sym"] = (config->getSymmetryString()).c_str();
	m_params["weight"] = 1.0;
	
	m_rec = EMAN::Factory<EMAN::Reconstructor>::get("back_projection", m_params);
	m_rec->setup();
	m_rec->print_params();
}


SingleParticle2dx::Methods::EMANBackprojectionMethod::~EMANBackprojectionMethod ()
{
	delete m_rec;
}


void SingleParticle2dx::Methods::EMANBackprojectionMethod::setupForBackProjection()
{
	m_rec->setup();
	m_rec->print_params();
}
			
			
void SingleParticle2dx::Methods::EMANBackprojectionMethod::finishReconstruction()
{
	size_type size = m_context->getSizeX() * m_context->getSizeY() * m_context->getSizeZ();
	EMAN::EMData* eman_data;
	eman_data = m_rec->finish();
	
	real_array3d_type rdata( boost::extents[m_context->getSizeX()][m_context->getSizeY()][m_context->getSizeZ()] );
	std::copy(eman_data->get_data(), eman_data->get_data()+size, rdata.origin() );
	
	SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&rdata);
	m_context->setFourierSpaceData(rdata);
	
	delete eman_data;
}
						
			
void SingleParticle2dx::Methods::EMANBackprojectionMethod::insertData(SingleParticle2dx::DataStructures::ParticleContainer &c)
{
	for (size_type i=0; i<c.getNumberOfParticles(); i++)
	{
		std::cout << i << "/" << c.getNumberOfParticles()-1 << std::endl;
		insertParticle(c(i));
	}
}


void SingleParticle2dx::Methods::EMANBackprojectionMethod::insertParticle(SingleParticle2dx::DataStructures::Particle& p)
{
	
	size_type size = m_context->getSizeX() * m_context->getSizeY();
	float* float_data_2d;
	float_data_2d = (float*) malloc(size * sizeof(float));
	real_array2d_type rdata( boost::extents[m_context->getSizeX()][m_context->getSizeY()] );
	
	SingleParticle2dx::DataStructures::Particle p2insert;
	p2insert = p;
	p2insert.applyLowPassFilter();
	p2insert.updateParticleShift();
	p2insert.applyWeight(p.getWeight());
	
	p2insert.getRealSpaceData(&rdata);
	std::copy(rdata.origin(), rdata.origin()+rdata.num_elements(), float_data_2d );
			
	EMAN::EMData eman_data;
	eman_data.set_data(float_data_2d, m_context->getSizeX(), m_context->getSizeY(), 1);
	
	EMAN::Transform t;
	EMAN::Dict rot;
	
	//std::cout << "\t" << p.getNewOrientation().getTLTAXIS() << ", " << p.getNewOrientation().getTLTANG() << ", " << p.getNewOrientation().getTAXA() << std::endl;
	
	rot["type"] = "spider";
	rot["phi"] = p.getNewOrientation().getTLTAXIS() + 90;
	rot["theta"] = p.getNewOrientation().getTLTANG();
	rot["psi"] = p.getNewOrientation().getTAXA();

	t.set_rotation(rot);
	
	if (m_rec->insert_slice(&eman_data, t, 1) != 0)
	{
		std::cerr << "slice inserting in eman2 failed" << std::endl;
		throw std::runtime_error("Bad operation");
	}
}

