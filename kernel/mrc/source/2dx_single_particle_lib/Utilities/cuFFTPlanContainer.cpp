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

#ifdef USE_CUDA

#include <omp.h>

#include <boost/shared_ptr.hpp>

#include "cuFFTPlanContainer.hpp"
#include "../Utilities.hpp"
#include "../Config.hpp"

boost::scoped_ptr<SingleParticle2dx::Utilities::cuFFTPlanContainer> SingleParticle2dx::Utilities::cuFFTPlanContainer::m_instance( NULL );


SingleParticle2dx::Utilities::cuFFTPlanContainer::cuFFTPlanContainer()
{	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	size_type nx = config->getParticleSize();
	size_type ny = config->getParticleSize();
	size_type nz = config->getParticleSize();
	
	#pragma omp parallel for
	for(size_type i=0; i<omp_get_num_threads(); i++)
	{
		cudaStream_t stream;
		cudaStreamCreate(&stream);
		
		#pragma omp critical (add_cuda_stream)
		{
			m_streams.push_back(stream);
		}
	}
	
	for(size_type i=0; i<omp_get_num_threads(); i++)
	{
		cufftHandle plan;
		cufftPlan2d(&plan, nx, ny, CUFFT_C2C);
		cufftSetStream(plan, m_streams[i]);
		m_small_2d_plan.push_back(plan);
		
		cufftPlan2d(&plan, 4*nx, 4*ny, CUFFT_C2C);
		cufftSetStream(plan, m_streams[i]);
		m_large_2d_plan.push_back(plan);
		
		cufftPlan3d(&plan, nx, ny, nz, CUFFT_C2C);
		cufftSetStream(plan, m_streams[i]);
		m_3d_plan.push_back(plan);
		
		m_device_p.push_back(NULL);
		m_host_p.push_back(NULL);
	}
	
	for(size_type i=0; i<omp_get_num_threads(); i++)
	{
		cudaMalloc((void**) m_device_p[i], sizeof(cufftComplex)*nx*ny );
		m_host_p[i] = (cufftComplex*)malloc(nx*ny*sizeof(cufftComplex));
	}
	
	std::cout << "setup for cuFFT done" << std::endl;
	
	setupPowArrays(nx);
}


cufftComplex* SingleParticle2dx::Utilities::cuFFTPlanContainer::getHostArray()
{
	return m_host_p[omp_get_thread_num()];
}
	

cufftComplex* SingleParticle2dx::Utilities::cuFFTPlanContainer::getDeviceArray()
{
	return m_device_p[omp_get_thread_num()];	
}


cudaStream_t& SingleParticle2dx::Utilities::cuFFTPlanContainer::getStream()
{
	return m_streams[omp_get_thread_num()];
}


void SingleParticle2dx::Utilities::cuFFTPlanContainer::setupPowArrays (size_type n)
{
	m_powf_array_2d.reset( new real_array2d_type(boost::extents[n][n]) );

  size_type j,k;

	#pragma omp parallel for private (j)
	for (size_type i=0; i<n; i++)
	{
		for (j=0; j<n; j++)
		{
			(*m_powf_array_2d.get())[i][j] = powf(-1.0, i+j);
		}
	}
	
	if (n < 1025)
	{
		m_powf_array_2d_large.reset( new real_array2d_type(boost::extents[4*n][4*n]) );
		
		#pragma omp parallel for private(j)
		for (size_type i=0; i<(4*n); i++)
		{
			for (j=0; j<(4*n); j++)
			{
				(*m_powf_array_2d_large.get())[i][j] = powf(-1.0, i+j);
			}
		}
	
		m_powf_array_3d.reset( new real_array3d_type(boost::extents[n][n][n]) );
		
		#pragma omp parallel for private(j,k)
		for (size_type i=0; i<n; i++)
		{
			for (j=0; j<n; j++)
			{
				for (k=0; k<n; k++)
				{
					(*m_powf_array_3d.get())[i][j][k] = powf(-1.0, i+j+k);
				}
			}
		}
	}	
}


SingleParticle2dx::Utilities::cuFFTPlanContainer::real_array2d_type* SingleParticle2dx::Utilities::cuFFTPlanContainer::getSmallPowArray2D()
{
	return m_powf_array_2d.get();
}


SingleParticle2dx::Utilities::cuFFTPlanContainer::real_array2d_type* SingleParticle2dx::Utilities::cuFFTPlanContainer::getLargePowArray2D()
{
	return m_powf_array_2d_large.get();
}


SingleParticle2dx::Utilities::cuFFTPlanContainer::real_array3d_type* SingleParticle2dx::Utilities::cuFFTPlanContainer::getPowArray3D()
{
	return m_powf_array_3d.get();
}


SingleParticle2dx::Utilities::cuFFTPlanContainer::~cuFFTPlanContainer()
{
	SingleParticle2dx::Utilities::cuFFTPlanContainer::deletePlans();
}


void SingleParticle2dx::Utilities::cuFFTPlanContainer::deletePlans()
{
	
}


SingleParticle2dx::Utilities::cuFFTPlanContainer* SingleParticle2dx::Utilities::cuFFTPlanContainer::Instance() {
	
	#pragma omp critical (cu_fftw_construction)
	{
		if (m_instance == NULL)
		{
			m_instance.reset( new SingleParticle2dx::Utilities::cuFFTPlanContainer );
		}
	}
	return m_instance.get();
}


cufftHandle& SingleParticle2dx::Utilities::cuFFTPlanContainer::getPlan_2d()
{
	return m_small_2d_plan[omp_get_thread_num()];
//	return m_small_2d_plan[0];
}


cufftHandle& SingleParticle2dx::Utilities::cuFFTPlanContainer::getPlan_4_2d()
{
	return m_large_2d_plan[omp_get_thread_num()];
//	return m_large_2d_plan[0];
}


cufftHandle& SingleParticle2dx::Utilities::cuFFTPlanContainer::getPlan_3d()
{
	return m_3d_plan[omp_get_thread_num()];
}

#endif
