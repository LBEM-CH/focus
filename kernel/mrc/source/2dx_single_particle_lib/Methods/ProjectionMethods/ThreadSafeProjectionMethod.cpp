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

#include "ThreadSafeProjectionMethod.hpp"
#include "../../Utilities.hpp"


SingleParticle2dx::Methods::ThreadSafeProjectionMethod::ThreadSafeProjectionMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context)
{
	std::cout << "parallelproj constructed" << std::endl;
	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	m_context = context;
	
	size_type nt;
	#pragma omp parallel
	{
		nt = omp_get_num_threads();
	}
	std::cout << "generating MultiThreadProj for " << nt << " threads" << std::endl;
	
	for (size_type i=0; i<nt; i++)
	{
		SingleParticle2dx::Methods::AbstractProjectionMethod* new_projector;
		
		switch (static_cast<int>(config->getProjectionMethod()))
		{
			case 0:
				new_projector = new SingleParticle2dx::Methods::RealSpaceProjectionMethod( m_context );
				break;
			case 1:
				new_projector = new SingleParticle2dx::Methods::FourierSpaceProjectionMethod( m_context );
				break;
			case 2:
				new_projector = new SingleParticle2dx::Methods::EMANProjectionMethod( m_context );
				break;
			case 3:
				new_projector = new SingleParticle2dx::Methods::EMANFourierGriddingProjector( m_context ); 
				break;
			#ifdef USE_CUDA
			case 4:
				new_projector = new SingleParticle2dx::Methods::CUDAProjectionMethod( m_context );
				break;
			#endif
			default:
				std::cerr << "unknown projection method in thread safe projector " << config->getProjectionMethod() << std::endl;
				new_projector = new SingleParticle2dx::Methods::EMANProjectionMethod( m_context );
				throw std::runtime_error("Bad operation");
		}
		
		m_projection_strategies.push_back(new_projector);
	}
}


SingleParticle2dx::Methods::ThreadSafeProjectionMethod::~ThreadSafeProjectionMethod ()
{
	for(size_type i=0; i<static_cast<size_type>(m_projection_strategies.size()); i++)
	{
		delete m_projection_strategies[i];
	}
}


void SingleParticle2dx::Methods::ThreadSafeProjectionMethod::calculateProjection(SingleParticle2dx::DataStructures::Orientation& o, SingleParticle2dx::DataStructures::Projection2d& p)
{
	size_type thread = omp_get_thread_num();
	
	m_projection_strategies[thread]->calculateProjection(o, p);	
	
}


void SingleParticle2dx::Methods::ThreadSafeProjectionMethod::prepareForProjections(SingleParticle2dx::DataStructures::ParticleContainer& cont)
{
	size_type nt;
	#pragma omp parallel
	{
		nt = omp_get_num_threads();
	}
	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	size_type n = config->getParticleSize();
	
	#pragma omp parallel for
	for (size_type i=0; i<nt; i++)
	{
		m_projection_strategies[i]->prepareForProjections(cont);
		SingleParticle2dx::DataStructures::Orientation o;
		SingleParticle2dx::DataStructures::Projection2d p(n, n);
		m_projection_strategies[i]->calculateProjection(o, p);
	}
}
