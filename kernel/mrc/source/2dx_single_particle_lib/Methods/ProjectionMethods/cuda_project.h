#include <cuda.h>
#include <cuda_runtime_api.h>
#include <driver_functions.h>


void project_on_gpu(const float* const matrix, float* data, const int nx, const int ny, cudaTextureObject_t texObj, cudaStream_t& stream);

