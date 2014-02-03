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

#include <boost/shared_ptr.hpp>

#include "FFTPlanContainer.hpp"
#include "FFTCalculator.hpp"

#include "../Config.hpp"


boost::scoped_ptr<SingleParticle2dx::Utilities::FFTPlanContainer> SingleParticle2dx::Utilities::FFTPlanContainer::m_instance( NULL );


SingleParticle2dx::Utilities::FFTPlanContainer::FFTPlanContainer()
{	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	size_type nx = config->getParticleSize();
	size_type ny = config->getParticleSize();
	size_type nz = config->getParticleSize();
	
	setupPowArrays(nx);
	
	fftwf_complex* in_2d = (fftwf_complex *) fftwf_malloc (sizeof(fftw_complex)*nx*ny);
	fftwf_complex* out_2d = (fftwf_complex *) fftwf_malloc (sizeof(fftw_complex)*nx*ny);
	
	fftwf_complex* in_2d_4 = (fftwf_complex *) fftwf_malloc (sizeof(fftw_complex)*nx*ny*16);
	fftwf_complex* out_2d_4 = (fftwf_complex *) fftwf_malloc (sizeof(fftw_complex)*nx*ny*16);
	
	if (nx < 1025)
	{
		fftwf_complex* in_3d = (fftwf_complex *) fftwf_malloc (sizeof(fftw_complex)*nx*ny*nz);
		fftwf_complex* out_3d = (fftwf_complex *) fftwf_malloc (sizeof(fftw_complex)*nx*ny*nz);
		
		m_p3d_fw = fftwf_plan_dft_3d (nx, ny, nz, in_3d, out_3d, FFTW_FORWARD, FFTW_MEASURE);
		m_p3d_bw = fftwf_plan_dft_3d (nx, ny, nz, in_3d, out_3d, FFTW_BACKWARD, FFTW_MEASURE);
		
		m_p2d_4_fw = fftwf_plan_dft_2d (4*nx, 4*ny, in_2d_4, out_2d_4, FFTW_FORWARD, FFTW_MEASURE);
		m_p2d_4_bw = fftwf_plan_dft_2d (4*nx, 4*ny, in_2d_4, out_2d_4, FFTW_BACKWARD, FFTW_MEASURE);
		
		m_p2d_fw = fftwf_plan_dft_2d (nx, ny, in_2d, out_2d, FFTW_FORWARD, FFTW_MEASURE);
		m_p2d_bw = fftwf_plan_dft_2d (nx, ny, in_2d, out_2d, FFTW_BACKWARD, FFTW_MEASURE);
		
		fftwf_free(in_3d);
		fftwf_free(out_3d);
	}
	else
	{
		m_p2d_fw = fftwf_plan_dft_2d (nx, ny, in_2d, out_2d, FFTW_FORWARD, FFTW_ESTIMATE);
		m_p2d_bw = fftwf_plan_dft_2d (nx, ny, in_2d, out_2d, FFTW_BACKWARD, FFTW_ESTIMATE);
	}
	
	fftwf_free(in_2d);
	fftwf_free(out_2d);
	fftwf_free(in_2d_4);
	fftwf_free(out_2d_4);
	
	SingleParticle2dx::Utilities::FFTPlanContainer::checkPlans();
}


void SingleParticle2dx::Utilities::FFTPlanContainer::setupPowArrays (size_type n)
{
	m_powf_array_2d.reset( new real_array2d_type(boost::extents[n][n]) );
	
	size_type j,k;
	value_type n_s = SingleParticle2dx::Utilities::FFTCalculator::getScalingFactor2d (n, n);

	#pragma omp parallel for private (j)
	for (size_type i=0; i<n; i++)
	{
		for (j=0; j<n; j++)
		{
			(*m_powf_array_2d.get())[i][j] = powf(-1.0, i+j) / n_s;
		}
	}
	
	
	if (n < 1025)
	{
		m_powf_array_2d_large.reset( new real_array2d_type(boost::extents[4*n][4*n]) );
		n_s = SingleParticle2dx::Utilities::FFTCalculator::getScalingFactor2d (4*n, 4*n);
		
		#pragma omp parallel for private(j)
		for (size_type i=0; i<(4*n); i++)
		{
			for (j=0; j<(4*n); j++)
			{
				(*m_powf_array_2d_large.get())[i][j] = powf(-1.0, i+j) / n_s;
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


SingleParticle2dx::Utilities::FFTPlanContainer::real_array2d_type* SingleParticle2dx::Utilities::FFTPlanContainer::getSmallPowArray2D()
{
	return m_powf_array_2d.get();
}


SingleParticle2dx::Utilities::FFTPlanContainer::real_array2d_type* SingleParticle2dx::Utilities::FFTPlanContainer::getLargePowArray2D()
{
	return m_powf_array_2d_large.get();
}


SingleParticle2dx::Utilities::FFTPlanContainer::real_array3d_type* SingleParticle2dx::Utilities::FFTPlanContainer::getPowArray3D()
{
	return m_powf_array_3d.get();
}


SingleParticle2dx::Utilities::FFTPlanContainer::~FFTPlanContainer()
{
	SingleParticle2dx::Utilities::FFTPlanContainer::deletePlans();
}


void SingleParticle2dx::Utilities::FFTPlanContainer::checkPlans()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	size_type nx = config->getParticleSize();
	
	if ( nx < 1025 )
	{
		if ( (m_p2d_fw==NULL) || (m_p2d_bw==NULL) || (m_p3d_fw==NULL) || (m_p3d_bw==NULL) )
		{
			SingleParticle2dx::Utilities::FFTPlanContainer::deletePlans();
			std::cerr << "invalid small plan" << std::endl;
			throw std::runtime_error("Bad operation");
		}
	}
	else
	{
		if ( (m_p2d_fw==NULL) || (m_p2d_bw==NULL) )
		{
			SingleParticle2dx::Utilities::FFTPlanContainer::deletePlans();
			std::cerr << "invalid big plan" << std::endl;
			throw std::runtime_error("Bad operation");
		}
	}
}


void SingleParticle2dx::Utilities::FFTPlanContainer::deletePlans()
{
	fftwf_destroy_plan (m_p2d_fw);
	fftwf_destroy_plan (m_p2d_bw);
	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	size_type nx = config->getParticleSize();
	
	if ( nx < 1025 )
	{
		fftwf_destroy_plan (m_p3d_fw);
		fftwf_destroy_plan (m_p3d_bw);
		
		fftwf_destroy_plan (m_p2d_4_fw);
		fftwf_destroy_plan (m_p2d_4_bw);
	}
}


SingleParticle2dx::Utilities::FFTPlanContainer* SingleParticle2dx::Utilities::FFTPlanContainer::Instance() {
	
	#pragma omp critical (fftw_construction)
	{
		if (m_instance == NULL)
		{
			m_instance.reset( new SingleParticle2dx::Utilities::FFTPlanContainer );
		}
	}
	return m_instance.get();
}


fftwf_plan& SingleParticle2dx::Utilities::FFTPlanContainer::getFW_2d()
{
	return m_p2d_fw;
}


fftwf_plan& SingleParticle2dx::Utilities::FFTPlanContainer::getBW_2d()
{
	return m_p2d_bw;
}


fftwf_plan& SingleParticle2dx::Utilities::FFTPlanContainer::getFW_4_2d()
{
	return m_p2d_4_fw;
}


fftwf_plan& SingleParticle2dx::Utilities::FFTPlanContainer::getBW_4_2d()
{
	return m_p2d_4_bw;
}


fftwf_plan& SingleParticle2dx::Utilities::FFTPlanContainer::getFW_3d()
{
	return m_p3d_fw;
}


fftwf_plan& SingleParticle2dx::Utilities::FFTPlanContainer::getBW_3d()
{
	return m_p3d_bw;
}
