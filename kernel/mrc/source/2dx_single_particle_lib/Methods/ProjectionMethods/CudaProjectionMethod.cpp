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


#include "CudaProjectionMethod.hpp"
#include "cuda_project.h"

//#include "../../Utilities.hpp"


SingleParticle2dx::Methods::CUDAProjectionMethod::CUDAProjectionMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context)
{
	m_context = context;
	m_size = context->getSizeX();
	m_real_data.reset( new real_array2d_type(boost::extents[context->getSizeX()][context->getSizeX()]) );
	m_matrix = new float[12];
	m_t = new EMAN::Transform;
	m_alloc_done = false;
	
	//std::cout << "::CUDA PROJECTION CREATED" << std::endl;
}


SingleParticle2dx::Methods::CUDAProjectionMethod::~CUDAProjectionMethod ()
{
	cudaDestroyTextureObject(m_texObj);
	cudaFreeArray(m_cuArray);
	cudaStreamDestroy(m_stream);
	delete[] m_matrix;
	delete m_t;
	
	free(res_data_h);
	cudaFree(res_data_d);
}


SingleParticle2dx::Methods::CUDAProjectionMethod::CUDAProjectionMethod (CUDAProjectionMethod const& rhs)
{
	m_proj = rhs.m_proj;
	m_3dmodel = rhs.m_3dmodel;
	m_float_data_3d = rhs.m_float_data_3d;
	m_real_data.reset( new real_array2d_type(boost::extents[m_context->getSizeX()][m_context->getSizeX()]) );
	m_matrix = rhs.m_matrix;
	m_size = rhs.m_size;
	m_t = rhs.m_t;
	m_cuArray = rhs.m_cuArray;
	m_texObj = rhs.m_texObj;
	res_data_h = rhs.res_data_h;
	res_data_d = rhs.res_data_d;
	m_stream = rhs.m_stream;
	m_alloc_done = rhs.m_alloc_done;
	
}


SingleParticle2dx::Methods::CUDAProjectionMethod& SingleParticle2dx::Methods::CUDAProjectionMethod::operator= (CUDAProjectionMethod rhs)
{
	swap (*this, rhs);
	return *this;
}

namespace SingleParticle2dx
{
	namespace Methods
	{
		void swap (CUDAProjectionMethod& o1, CUDAProjectionMethod& o2)
		{
			std::swap (o1.m_proj, o2.m_proj);
			std::swap (o1.m_3dmodel, o2.m_3dmodel);
			std::swap (o1.m_float_data_3d, o2.m_float_data_3d);
			o1.m_real_data.reset( new real_array2d_type(boost::extents[o1.m_context->getSizeX()][o1.m_context->getSizeX()]) );
			o2.m_real_data.reset( new real_array2d_type(boost::extents[o2.m_context->getSizeX()][o2.m_context->getSizeX()]) );
			std::swap (o1.m_matrix, o2.m_matrix);
			std::swap (o1.m_size, o2.m_size);
			std::swap (o1.m_t, o2.m_t);
			std::swap (o1.m_cuArray, o2.m_cuArray);
			std::swap (o1.m_texObj, o2.m_texObj);
			std::swap (o1.res_data_h, o2.res_data_h);
			std::swap (o1.res_data_d, o2.res_data_d);
			std::swap (o1.m_stream, o2.m_stream);
			std::swap (o1.m_alloc_done, o2.m_alloc_done);
			std::swap (o1.m_context, o2.m_context);
		}
		
	} /* DataStructures */
	
} /* SingleParticle2dx */


void SingleParticle2dx::Methods::CUDAProjectionMethod::calculateProjection(SingleParticle2dx::DataStructures::Orientation& o, SingleParticle2dx::DataStructures::Projection2d& p)
{
	EMAN::Dict rot;
	
	rot["type"] = "spider";
	rot["phi"] = o.getTAXA();
	rot["theta"] = o.getTLTANG();
	rot["psi"] = o.getTLTAXIS() + 90;

	m_t->set_rotation(rot);
	m_t->copy_matrix_into_array(m_matrix);
	
	project_on_gpu(m_matrix, res_data_d, m_size, m_size, m_texObj, m_stream);
	cudaMemcpyAsync(m_real_data.get()->origin(), res_data_d, m_size*m_size*sizeof(float), cudaMemcpyDeviceToHost, m_stream);
	
	SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(m_real_data.get());
	p.setFourierSpaceData(m_real_data.get());
	p.applyLowPassFilter();
	p.applyMask();
	p.setOrientation(o);
}


void SingleParticle2dx::Methods::CUDAProjectionMethod::prepareForProjections(SingleParticle2dx::DataStructures::ParticleContainer& cont)
{
	cudaSetDevice(getMyGPU());
	cudaStreamCreate(&m_stream);
	
	cudaChannelFormatDesc channelDesc = cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindFloat);
	cudaExtent VS = make_cudaExtent(m_size, m_size, m_size);
	
	if( m_alloc_done == false )
	{
		cudaMalloc3DArray(&m_cuArray, &channelDesc, VS);
	}
		
	SingleParticle2dx::real_array3d_type real_data( boost::extents[m_size][m_size][m_size] );
	m_context->getRealSpaceData(real_data);
	unsigned int size = m_size*m_size*m_size*sizeof(float);
	
	if( m_alloc_done == false )
	{
		res_data_h = (float*)malloc(m_size*m_size*sizeof(float));
		cudaMalloc((void**)&res_data_d, m_size*m_size*sizeof(float));
		m_alloc_done = true;
	}
	
	cudaMemcpy3DParms copyParams = {0};
	copyParams.srcPtr = make_cudaPitchedPtr((void*)real_data.origin(), VS.width*sizeof(float), VS.width, VS.height);
	copyParams.dstArray = m_cuArray;
	copyParams.extent = VS;
	copyParams.kind = cudaMemcpyHostToDevice;
	
//	cudaMemcpy3D(&copyParams);
	cudaMemcpy3DAsync(&copyParams, m_stream);
		
	struct cudaResourceDesc resDesc;
	memset(&resDesc, 0, sizeof(resDesc));
	resDesc.resType = cudaResourceTypeArray;
	resDesc.res.array.array = m_cuArray;
	
	struct cudaTextureDesc texDesc;
	memset(&texDesc, 0, sizeof(texDesc));
	texDesc.addressMode[0]   = cudaAddressModeClamp;
	texDesc.addressMode[1]   = cudaAddressModeClamp;
	texDesc.addressMode[2]   = cudaAddressModeClamp;
	texDesc.filterMode       = cudaFilterModeLinear;
	texDesc.readMode         = cudaReadModeElementType;
	texDesc.normalizedCoords = 0;

	if(m_alloc_done == true)
	{
		cudaDestroyTextureObject(m_texObj);
	}

	m_texObj = 0;
	cudaCreateTextureObject(&m_texObj, &resDesc, &texDesc, NULL);
}


SingleParticle2dx::Methods::CUDAProjectionMethod::size_type SingleParticle2dx::Methods::CUDAProjectionMethod::getMyGPU()
{
	size_type num_devs;
	cudaGetDeviceCount(&num_devs);
	
	size_type active_gpu;
	
	if(num_devs == 1)
	{
		active_gpu = 0;
	}
	else
	{
		size_type omp_num_threads = omp_get_num_threads();
		size_type current_thread = omp_get_thread_num();
		
		if(current_thread < (omp_num_threads/2))
		{
			active_gpu = 0;
		}
		else
		{
			active_gpu = 1;
		}
	}
	
	return active_gpu;
}

#endif
