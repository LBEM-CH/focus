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

#include "MultiThreadedEMANBackProjectionMethod.hpp"
#include "FFTEMANBackprojectionMethod.hpp"

#include "../../Utilities.hpp"

#include <transform.h>

SingleParticle2dx::Methods::MultiThreadedEMANBackprojectionMethod::MultiThreadedEMANBackprojectionMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context)
{
	m_context = context;
	
	size_type num_threads;
	#pragma omp parallel
	{
		num_threads = omp_get_num_threads();
	}
	
	std::cout << "stetting up backprojection for " << num_threads << " threads" << std::endl;
	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();	

	for (size_type i=0; i<num_threads; i++)
	{
		std::vector<int> size;
		size.push_back(m_context->getSizeX());
		size.push_back(m_context->getSizeY());
		size.push_back(m_context->getSizeZ());

		m_params.push_back(new EMAN::Dict);
		(*m_params[i])["size"] = size;
		(*m_params[i])["sym"] = (config->getSymmetryString()).c_str();
		(*m_params[i])["mode"] = "nearest_neighbor";
		(*m_params[i])["sqrtnorm"] = false;
		(*m_params[i])["verbose"] = false;
		(*m_params[i])["quiet"] = true;

		std::vector<int> array;
		array.push_back(0);
		array.push_back(0);
		array.push_back(0);
		array.push_back(m_context->getSizeX());
		array.push_back(m_context->getSizeX());
		array.push_back(m_context->getSizeX());

		m_rec.push_back(EMAN::Factory<EMAN::Reconstructor>::get("fourier", (*m_params[i])));
		m_rec[i]->setup();
		m_counter.push_back(value_type(0));
	}
}


SingleParticle2dx::Methods::MultiThreadedEMANBackprojectionMethod::~MultiThreadedEMANBackprojectionMethod ()
{}


void SingleParticle2dx::Methods::MultiThreadedEMANBackprojectionMethod::setupForBackProjection()
{
	size_type num_threads;
	#pragma omp parallel
	{
		num_threads = omp_get_num_threads();
	}
	
	#pragma omp parallel for
	for (size_type i=0; i<num_threads; i++)
	{
		m_rec[i]->setup();
	}
}
			
			
void SingleParticle2dx::Methods::MultiThreadedEMANBackprojectionMethod::finishReconstruction()
{
	size_type num_threads;
	#pragma omp parallel
	{
		num_threads = omp_get_num_threads();
	}
	
	std::cout << "assembly the volume" << std::endl;
	real_array3d_type rdata_total( boost::extents[m_context->getSizeX()][m_context->getSizeY()][m_context->getSizeZ()] );
	
	size_type check = 0;
	for(size_type i=0; i<num_threads; i++)
	{
		check += m_counter[i];
	}
	
	for (size_type i=0; i<num_threads; i++)
	{
		size_type size = m_context->getSizeX() * m_context->getSizeY() * m_context->getSizeZ();
		EMAN::EMData* eman_data;
		eman_data = m_rec[i]->finish();
		
		real_array3d_type rdata( boost::extents[m_context->getSizeX()][m_context->getSizeY()][m_context->getSizeZ()] );
		std::copy(eman_data->get_data(), eman_data->get_data()+size, rdata.origin() );
		SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&rdata);
		
		value_type w = static_cast<value_type>(m_counter[i]) / check;
		
		if(m_counter[i] == 0)
		{
			continue;
		}
		
		m_counter[i] = 0;
		std::cout << "weight: " << w << std::endl;
		
		size_type j,k;
		#pragma omp parallel for private(j,k)
		for(size_type i=0; i<m_context->getSizeX(); i++)
		{
			for(j=0; j<m_context->getSizeY(); j++)
			{
				for(k=0; k<m_context->getSizeZ(); k++)
				{
					rdata_total[i][j][k] += rdata[i][j][k] * w;
				}
			}
		}
	}
	
	SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&rdata_total);
	m_context->setFourierSpaceData(rdata_total);
}						
	
			
void SingleParticle2dx::Methods::MultiThreadedEMANBackprojectionMethod::insertData(SingleParticle2dx::DataStructures::ParticleContainer &c)
{
	size_type num_threads;
	#pragma omp parallel
	{
		num_threads = omp_get_num_threads();
	}
	
	for (size_type i=0; i<num_threads; i++)
	{
		{
			insertParticle(c(i));
		}
	}
	
	std::cout << "serial init done ready to run in parallel" << std::endl;

	//#pragma omp parallel for schedule(dynamic, 1)
	#pragma omp parallel for
	for (size_type i=num_threads; i<c.getNumberOfParticles(); i++)
	{
		insertParticle(c(i));
	}
}


void SingleParticle2dx::Methods::MultiThreadedEMANBackprojectionMethod::insertParticle(SingleParticle2dx::DataStructures::Particle& p)
{
	if ( !p.getUseForReconstruction())
	{
	//	std::cout << "too low cc value, not using this particle for reconstruction" << std::endl;
	//	
	//	#pragma omp critical (bad_particles_update)
	//	{
	//		m_bad_counter++;
	//	}
	//	
		return;
	}
	
	size_type thread = omp_get_thread_num();
	
	size_type size = m_context->getSizeX() * m_context->getSizeY();
	float* float_data_2d;
	float_data_2d = (float*) malloc(size * sizeof(float));
	real_array2d_type rdata( boost::extents[m_context->getSizeX()][m_context->getSizeY()] );
	
	SingleParticle2dx::DataStructures::Particle p2insert;
	p2insert = p;
	p2insert.updateParticleShift();
	
	p2insert.applyWeight(p.getWeight());
	
	p2insert.getRealSpaceData(&rdata);
	std::copy(rdata.origin(), rdata.origin()+rdata.num_elements(), float_data_2d );
			
	EMAN::EMData eman_data;
	eman_data.set_data(float_data_2d, m_context->getSizeX(), m_context->getSizeY(), 1);
	
	EMAN::Transform t;
	EMAN::Dict rot;
	
	rot["type"] = "spider";
	rot["phi"] = p.getNewOrientation().getTAXA();
	rot["theta"] = p.getNewOrientation().getTLTANG();
	rot["psi"] = p.getNewOrientation().getTLTAXIS() + 90;

	t.set_rotation(rot);
			
	if (m_rec[thread]->insert_slice(&eman_data, t, 1) != 0)
	{
		std::cerr << "slice inserting in eman2 failed" << std::endl;
		throw std::runtime_error("Bad operation");
	}
	
	m_counter[thread] += p.getWeight();
}

