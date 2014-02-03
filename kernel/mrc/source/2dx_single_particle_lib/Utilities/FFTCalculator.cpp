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

#include <boost/shared_ptr.hpp>

#include "FFTCalculator.hpp"
#include "FFTPlanContainer.hpp"

#include "../Config.hpp"


SingleParticle2dx::Utilities::FFTCalculator::value_type SingleParticle2dx::Utilities::FFTCalculator::getScalingFactor2d (size_type nx, size_type ny)
{
	return (sqrt(nx) * sqrt(ny));
}


SingleParticle2dx::Utilities::FFTCalculator::value_type SingleParticle2dx::Utilities::FFTCalculator::getScalingFactor3d (size_type nx, size_type ny, size_type nz)
{
	return (sqrt(nx) * sqrt(ny) * sqrt(nz));
}


void SingleParticle2dx::Utilities::FFTCalculator::performBackwardFFT (fft_array2d_type* fourier_data, real_array2d_type* real_data)
{
	size_type nx = fourier_data->shape()[0];
	size_type ny = fourier_data->shape()[1];
	size_type n_part = SingleParticle2dx::ConfigContainer::Instance()->getParticleSize();
	
	SingleParticle2dx::Utilities::FFTPlanContainer* plan_cont = SingleParticle2dx::Utilities::FFTPlanContainer::Instance();
		
	fftwf_complex* in = (fftwf_complex *) fftwf_malloc ( sizeof(fftwf_complex)*nx*ny);
	fftwf_complex* out = (fftwf_complex *) fftwf_malloc ( sizeof(fftwf_complex)*nx*ny);
		
	fftwf_complex* out_pointer = out;
	
	for (size_type i=0; i<nx; i++)
	{
		for (size_type j=0; j<ny; j++)
		{
			out_pointer[j+i*ny][0] = (*fourier_data)[i][j].real();
			out_pointer[j+i*ny][1] = (*fourier_data)[i][j].imag();
		}
	}
	
	fftwf_plan p2_bw;
	real_array2d_type* pow_array;
	
	if( nx == 4*n_part )
	{
		p2_bw = plan_cont->getBW_4_2d();
		pow_array = plan_cont->getLargePowArray2D();
	}
	else
	{
		p2_bw = plan_cont->getBW_2d();
		pow_array = plan_cont->getSmallPowArray2D();
	}
	
//	fftw3_mkl.verbose = 10;
	fftwf_execute_dft(p2_bw, out, in);
	
	fftwf_complex* in_pointer = in;
	value_type power;
	
	for (size_type i=0; i<nx; i++)
	{
		for (size_type j=0; j<ny; j++)
		{
			power = (*pow_array)[i][j];
			//((*real_data)[i][j]) = in.get()[j+i*ny][0] * powf(-1.0,i+j) / n;
			((*real_data)[i][j]) = in_pointer[j+i*ny][0] * power;
		}
	}
	
	fftwf_free(in);
	fftwf_free(out);
}


void SingleParticle2dx::Utilities::FFTCalculator::performForwardFFT(real_array2d_type* real_data, fft_array2d_type* fourier_data)
{
	size_type nx = fourier_data->shape()[0];
	size_type ny = fourier_data->shape()[1];
	
	size_type n_part = SingleParticle2dx::ConfigContainer::Instance()->getParticleSize();
	
	SingleParticle2dx::Utilities::FFTPlanContainer* plan_cont = SingleParticle2dx::Utilities::FFTPlanContainer::Instance();
	
	fftwf_plan p2_fw;
	real_array2d_type* pow_array;
	
	if( nx == 4*n_part )
	{
		p2_fw = plan_cont->getFW_4_2d();
		pow_array = plan_cont->getLargePowArray2D();
	}
	else
	{
		p2_fw = plan_cont->getFW_2d();
		pow_array = plan_cont->getSmallPowArray2D();
	}
	
	fftwf_complex* in = (fftwf_complex *) fftwf_malloc (sizeof(fftwf_complex)*nx*ny);
	fftwf_complex* out = (fftwf_complex *) fftwf_malloc (sizeof(fftwf_complex)*nx*ny);

	for (size_type i=0; i<nx; i++)
	{
		for (size_type j=0; j<ny; j++)
		{
			in[j+i*ny][0] = ((*real_data)[i][j]) * ((*pow_array)[i][j]);
			in[j+i*ny][1] = 0;
		}
	}
	
	fftwf_execute_dft(p2_fw, in, out);
	
	fft_type value2set(0,0);
	
	for (size_type i=0; i<nx; i++)
	{
		for (size_type j=0; j<ny; j++)
		{
			value2set.real(out[j+i*ny][0]);
			value2set.imag(out[j+i*nx][1]);
			(*fourier_data)[i][j] = value2set;
		}
	}
	
	fftwf_free(in);
	fftwf_free(out);
}


void SingleParticle2dx::Utilities::FFTCalculator::performBackwardFFT(fft_array3d_type* fourier_data, real_array3d_type* real_data)
{
	size_type nx = fourier_data->shape()[0];
	size_type ny = fourier_data->shape()[1];
	size_type nz = fourier_data->shape()[2];
	
	value_type n = getScalingFactor3d (nx, ny, nz);
	
	fftwf_complex* in = (fftwf_complex *) fftwf_malloc (sizeof(fftwf_complex)*nx*ny*nz);
	fftwf_complex* out = (fftwf_complex *) fftwf_malloc (sizeof(fftwf_complex)*nx*ny*nz);
	
	for (size_type i=0; i<nx; i++)
	{
		for (size_type j=0; j<ny; j++)
		{
			for (size_type k=0; k<nz; k++)
			{
				out[k+j*ny+i*nx*ny][0] = (*fourier_data)[i][j][k].real();
				out[k+j*ny+i*nx*ny][1] = (*fourier_data)[i][j][k].imag();
			}	
		}
	}
	
	
	SingleParticle2dx::Utilities::FFTPlanContainer* plan_cont = SingleParticle2dx::Utilities::FFTPlanContainer::Instance();
	fftwf_plan p3_bw = plan_cont->getBW_3d();
	real_array3d_type* power_array = plan_cont->getPowArray3D();
	
	fftwf_execute_dft(p3_bw, out, in);
	
	value_type power;
	for (size_type i=0; i<nx; i++)
	{
		for (size_type j=0; j<ny; j++)
		{
			for (size_type k=0; k<nz; k++)
			{
				power = (*power_array)[i][j][k];
				//((*real_data)[i][j][k]) = in.get()[k+j*ny+i*nx*ny][0] * powf(-1.0,i+j+k) / n;
				((*real_data)[i][j][k]) = in[k+j*ny+i*nx*ny][0] * power / n; 
			}
		}
	}
	
	fftwf_free(in);
	fftwf_free(out);
}


void SingleParticle2dx::Utilities::FFTCalculator::performForwardFFT(real_array3d_type* real_data, fft_array3d_type* fourier_data)
{
	size_type nx = fourier_data->shape()[0];
	size_type ny = fourier_data->shape()[1];
	size_type nz = fourier_data->shape()[2];
	
	value_type n = getScalingFactor3d (nx, ny, nz);
	
	SingleParticle2dx::Utilities::FFTPlanContainer* plan_cont = SingleParticle2dx::Utilities::FFTPlanContainer::Instance();
	
	fftwf_complex* in = (fftwf_complex *) fftwf_malloc (sizeof(fftwf_complex)*nx*ny*nz);
	fftwf_complex* out = (fftwf_complex *) fftwf_malloc (sizeof(fftwf_complex)*nx*ny*nz);
	
	real_array3d_type* power_array = plan_cont->getPowArray3D();

	value_type power;
	for (size_type i=0; i<nx; i++)
	{
		for (size_type j=0; j<ny; j++)
		{
			for (size_type k=0; k<nz; k++)
			{
				power = (*power_array)[i][j][k];
				//in.get()[k+j*ny+i*nx*ny][0] = ((*real_data)[i][j][k]) * powf(-1,i+j+k) / n;
				in[k+j*ny+i*nx*ny][0] = ((*real_data)[i][j][k]) * power / n;
				in[k+j*ny+i*nx*ny][1] = 0;
			}
		}
	}
	
	fftwf_plan p3_fw = plan_cont->getFW_3d();
	fftwf_execute_dft(p3_fw, in, out);
	
	fft_type value2set(0,0);
	for (size_type i=0; i<nx; i++)
	{
		for (size_type j=0; j<ny; j++)
		{
			for (size_type k=0; k<nz; k++)
			{
				value2set.real(out[k+j*ny+i*nx*ny][0]);
				value2set.imag(out[k+j*ny+i*nx*ny][1]);
				(*fourier_data)[i][j][k] = value2set;
			}
		}
	}
	
	fftwf_free(in);
	fftwf_free(out);
}
