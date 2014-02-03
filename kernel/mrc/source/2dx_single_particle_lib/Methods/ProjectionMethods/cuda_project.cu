#include "cuda_project.h"

#include <fstream>
#include <iostream>

#include <omp.h>

typedef unsigned int uint;

__global__ void proj_kernel_sp2dx(float* out, float size, float size_on_two, float3 mxx, float3 mxy, float3 mxz, cudaTextureObject_t tex)
{
	uint x = threadIdx.x;
	uint y = blockIdx.x;
	
	float fx = x-size_on_two;
	float fy = y-size_on_two;
	
	float tx, ty, tz;
	
	float sum = 0;
	
	for(float fz = -size_on_two; fz<size_on_two-.1; fz+=1.0)
	{
		tx = fx*mxx.x + fy*mxx.y + fz*mxx.z + size_on_two + 0.5;
		ty = fx*mxy.x + fy*mxy.y + fz*mxy.z + size_on_two + 0.5;
		tz = fx*mxz.x + fy*mxz.y + fz*mxz.z + size_on_two + 0.5;
		sum += tex3D<float>(tex, tx, ty, tz);
	}
	
	out[x+y*(int)size] = sum;
}


void project_on_gpu(const float* const matrix, float* data, const int nx, const int ny, cudaTextureObject_t texObj, cudaStream_t& stream)
{
	const dim3 blockSize(ny,1,1);
	const dim3 gridSize(nx,1,1);
	
	float3 mxx, mxy, mxz;
	
	mxx.x = matrix[0];
	mxx.y = matrix[4];
	mxx.z = matrix[8];
	
	mxy.x = matrix[1];
	mxy.y = matrix[5];
	mxy.z = matrix[9];
	
	mxz.x = matrix[2];
	mxz.y = matrix[6];
	mxz.z = matrix[10];
	
	proj_kernel_sp2dx<<<blockSize, gridSize, 0, stream>>>(data, static_cast<float>(nx), static_cast<float>(nx)/2, mxx, mxy, mxz, texObj);
	cudaThreadSynchronize();

	cudaError_t err = cudaGetLastError();		
	if (err != cudaSuccess)
	{
		printf("::CUDA-Error: %s\n", cudaGetErrorString(err));
	}
}

