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

#include <boost/shared_ptr.hpp>

#include "cuFFTCalculator.hpp"
#include "../Utilities.hpp"
#include "../Config.hpp"



SingleParticle2dx::Utilities::cuFFTCalculator::value_type SingleParticle2dx::Utilities::cuFFTCalculator::getScalingFactor2d (size_type nx, size_type ny)
{
	return (sqrt(nx) * sqrt(ny));
}


SingleParticle2dx::Utilities::cuFFTCalculator::value_type SingleParticle2dx::Utilities::cuFFTCalculator::getScalingFactor3d (size_type nx, size_type ny, size_type nz)
{
	return (sqrt(nx) * sqrt(ny) * sqrt(nz));
}


void SingleParticle2dx::Utilities::cuFFTCalculator::performBackwardFFT (fft_array2d_type* fourier_data, real_array2d_type* real_data)
{
	size_type nx = fourier_data->shape()[0];
	size_type ny = fourier_data->shape()[1];
	
	size_type n_part = SingleParticle2dx::ConfigContainer::Instance()->getParticleSize();
	
	SingleParticle2dx::Utilities::cuFFTPlanContainer* plan_cont = SingleParticle2dx::Utilities::cuFFTPlanContainer::Instance();
	
	value_type n = getScalingFactor2d (nx, ny);
	
	cufftComplex *data_h = plan_cont->getHostArray();
	cufftComplex *data_d = plan_cont->getDeviceArray();
	
	for (size_type i=0; i<nx; i++)
	{
		for (size_type j=0; j<ny; j++)
		{
			data_h[j+i*ny].x = (*fourier_data)[i][j].real();
			data_h[j+i*ny].y = (*fourier_data)[i][j].imag();
		}
	}
	
	cudaStream_t stream = plan_cont->getStream();
	
	cudaMemcpyAsync(data_d, data_h, nx*nx*sizeof(cufftComplex), cudaMemcpyHostToDevice, stream);
	
	cufftHandle p2_bw;
	real_array2d_type* pow_array;
	
	if( nx == 4*n_part )
	{
		p2_bw = plan_cont->getPlan_4_2d();
		pow_array = plan_cont->getLargePowArray2D();
	}
	else
	{
		p2_bw = plan_cont->getPlan_2d();
		pow_array = plan_cont->getSmallPowArray2D();
	}
	
	cufftExecC2C(p2_bw, data_d, data_d, CUFFT_INVERSE);
	cudaMemcpyAsync(data_d, data_h, nx*nx*sizeof(cufftComplex), cudaMemcpyDeviceToHost, stream);
	
	value_type power;
	
	for (size_type i=0; i<nx; i++)
	{
		for (size_type j=0; j<ny; j++)
		{
			power = (*pow_array)[i][j];
			((*real_data)[i][j]) = data_h[j+i*ny].x * power / n;
		}
	}
}


void SingleParticle2dx::Utilities::cuFFTCalculator::performForwardFFT(real_array2d_type* real_data, fft_array2d_type* fourier_data)
{
	size_type nx = fourier_data->shape()[0];
	size_type ny = fourier_data->shape()[1];
	
	size_type n_part = SingleParticle2dx::ConfigContainer::Instance()->getParticleSize();
	SingleParticle2dx::Utilities::cuFFTPlanContainer* plan_cont = SingleParticle2dx::Utilities::cuFFTPlanContainer::Instance();
	value_type n = getScalingFactor2d (nx, ny);
	
	cufftHandle p2_fw;
	real_array2d_type* pow_array;
	
	if( nx == 4*n_part )
	{
		p2_fw = plan_cont->getPlan_4_2d();
		pow_array = plan_cont->getLargePowArray2D();
	}
	else
	{
		p2_fw = plan_cont->getPlan_2d();
		pow_array = plan_cont->getSmallPowArray2D();
	}
	
	cufftComplex *data_h = plan_cont->getHostArray();
	cufftComplex *data_d = plan_cont->getDeviceArray();
	
	std::cout << "got array" << std::endl;

	value_type power;
	for (size_type i=0; i<nx; i++)
	{
		for (size_type j=0; j<ny; j++)
		{
			power = (*pow_array)[i][j];
			data_h[j+i*ny].x = ((*real_data)[i][j]) * power / n;
			data_h[j+i*ny].y = 0;
		}
	}
	
	std::cout << "filled array" << std::endl;
	
	cudaStream_t stream = plan_cont->getStream();
	std::cout << "got stream" << std::endl;
	
	cudaMemcpyAsync(data_d, data_h, nx*nx*sizeof(cufftComplex), cudaMemcpyHostToDevice, stream);
	std::cout << "H2D done" << std::endl;
	
	cufftExecC2C(p2_fw, data_d, data_d, CUFFT_FORWARD);
	std::cout << "cufft done" << std::endl;
	
	cudaMemcpyAsync(data_d, data_h, nx*nx*sizeof(cufftComplex), cudaMemcpyDeviceToHost, stream);
	std::cout << "D2H done" << std::endl;

	fft_type value2set(0,0);	
	for (size_type i=0; i<nx; i++)
	{
		for (size_type j=0; j<ny; j++)
		{
			value2set.real(data_h[j+i*ny].x);
			value2set.imag(data_h[j+i*nx].y);
			(*fourier_data)[i][j] = value2set;
		}
	}
	std::cout << "final copy done" << std::endl;
}


void SingleParticle2dx::Utilities::cuFFTCalculator::performBackwardFFT(fft_array3d_type* fourier_data, real_array3d_type* real_data)
{
	/*
	size_type nx = fourier_data->shape()[0];
	size_type ny = fourier_data->shape()[1];
	size_type nz = fourier_data->shape()[2];
	
	value_type n = getScalingFactor3d (nx, ny, nz);
	
	boost::shared_ptr<fftwf_complex> in((fftwf_complex *) fftwf_malloc (sizeof(fftwf_complex)*nx*ny*nz), SingleParticle2dx::Utilities::CustomizedDeleter());
	boost::shared_ptr<fftwf_complex> out((fftwf_complex *) fftwf_malloc (sizeof(fftwf_complex)*nx*ny*nz), SingleParticle2dx::Utilities::CustomizedDeleter());
	
	for (size_type i=0; i<nx; i++)
	{
		for (size_type j=0; j<ny; j++)
		{
			for (size_type k=0; k<nz; k++)
			{
				out.get()[k+j*ny+i*nx*ny][0] = (*fourier_data)[i][j][k].real();
				out.get()[k+j*ny+i*nx*ny][1] = (*fourier_data)[i][j][k].imag();
			}	
		}
	}
	
	
	SingleParticle2dx::Utilities::cuFFTPlanContainer* plan_cont = SingleParticle2dx::Utilities::cuFFTPlanContainer::Instance();
	fftwf_plan p3_bw = plan_cont->getBW_3d();
	real_array3d_type* power_array = plan_cont->getPowArray3D();
	
	fftwf_execute_dft(p3_bw, out.get(), in.get());
	
	value_type power;
	for (size_type i=0; i<nx; i++)
	{
		for (size_type j=0; j<ny; j++)
		{
			for (size_type k=0; k<nz; k++)
			{
				power = (*power_array)[i][j][k];
				//((*real_data)[i][j][k]) = in.get()[k+j*ny+i*nx*ny][0] * powf(-1.0,i+j+k) / n;
				((*real_data)[i][j][k]) = in.get()[k+j*ny+i*nx*ny][0] * power / n; 
			}
		}
	}
	*/
}


void SingleParticle2dx::Utilities::cuFFTCalculator::performForwardFFT(real_array3d_type* real_data, fft_array3d_type* fourier_data)
{
	/*
	size_type nx = fourier_data->shape()[0];
	size_type ny = fourier_data->shape()[1];
	size_type nz = fourier_data->shape()[2];
	
	value_type n = getScalingFactor3d (nx, ny, nz);
	
	SingleParticle2dx::Utilities::cuFFTPlanContainer* plan_cont = SingleParticle2dx::Utilities::cuFFTPlanContainer::Instance();
	
	boost::shared_ptr<fftwf_complex> in((fftwf_complex *) fftwf_malloc (sizeof(fftwf_complex)*nx*ny*nz), SingleParticle2dx::Utilities::CustomizedDeleter());
	boost::shared_ptr<fftwf_complex> out((fftwf_complex *) fftwf_malloc (sizeof(fftwf_complex)*nx*ny*nz), SingleParticle2dx::Utilities::CustomizedDeleter());
	
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
				in.get()[k+j*ny+i*nx*ny][0] = ((*real_data)[i][j][k]) * power / n;
				in.get()[k+j*ny+i*nx*ny][1] = 0;
			}
		}
	}
	
	fftwf_plan p3_fw = plan_cont->getFW_3d();
	fftwf_execute_dft(p3_fw, in.get(), out.get());
	
	fft_type value2set(0,0);
	for (size_type i=0; i<nx; i++)
	{
		for (size_type j=0; j<ny; j++)
		{
			for (size_type k=0; k<nz; k++)
			{
				value2set.real(out.get()[k+j*ny+i*nx*ny][0]);
				value2set.imag(out.get()[k+j*ny+i*nx*ny][1]);
				(*fourier_data)[i][j][k] = value2set;
			}
		}
	}
	*/
}

#endif
