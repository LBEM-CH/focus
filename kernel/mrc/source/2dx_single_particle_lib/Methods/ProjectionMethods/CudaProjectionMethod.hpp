#ifdef USE_CUDA

#ifndef CUDAPROJECTIONMETHOD_HPP_BP7HCX81
#define CUDAPROJECTIONMETHOD_HPP_BP7HCX81

namespace SingleParticle2dx
{
	namespace Methods
	{
		class CUDAProjectionMethod;
	} 
}

#include <cuda.h>
#include <cuda_runtime_api.h>
#include <driver_functions.h>


#include "AbstractProjectionMethod.hpp"

#include "../../DataStructures/Reconstruction3d.hpp"
#include "../../DataStructures/ParticleContainer.hpp"
#include "../../DataStructures/Projection2d.hpp"
#include "../../DataStructures/Orientation.hpp"

//#include "../../DataStructures.hpp"
//#include "../../Typedefs.hpp"
//#include "../../Methods.hpp"

#include <projector.h>

namespace SingleParticle2dx
{
	
	namespace Methods
	{
		
		/**
		 *  @brief     Projection class preforming real space projections
		 *  @details   
		 *  @author    Sebastian Scherer
		 *  @version   0.1
		 *  @date      2012
		 *  @copyright GNU Public License
		 */
		class CUDAProjectionMethod : public SingleParticle2dx::Methods::AbstractProjectionMethod
		{
		public:
				
			typedef SingleParticle2dx::real_array2d_type real_array2d_type;
			typedef SingleParticle2dx::real_array3d_type real_array3d_type;
			
			
								
			/**
			 *  @brief      Constructor
			 *  @details    Costum constructor
			 *  @param[in]  context Context of the method
			 */
			CUDAProjectionMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context);
			
			
			/**
			 *  @brief      Destructor
			 *  @details    All the allocated memory is freed afer calling the Destructor
			 */
			virtual ~CUDAProjectionMethod ();
			
			CUDAProjectionMethod (CUDAProjectionMethod const& rhs);
			CUDAProjectionMethod& operator= (CUDAProjectionMethod rhs);
			friend void swap (CUDAProjectionMethod& o1, CUDAProjectionMethod& o2);
			
			
			virtual void prepareForProjections(SingleParticle2dx::DataStructures::ParticleContainer& cont);

			
			/**
			 *  @brief      Calculates the projection in direction o
			 *  @details    
			 *  @param[in]  o Direction of the projection to calculate
			 *  @param[out] p Reference where to store the calculated projection
			 */
			virtual void calculateProjection(SingleParticle2dx::DataStructures::Orientation& o, SingleParticle2dx::DataStructures::Projection2d& p);
			
			static size_type getMyGPU();
			
		private:
		
			EMAN::Projector* m_proj;
			EMAN::EMData* m_3dmodel;
			float* m_float_data_3d;
			boost::scoped_ptr<real_array2d_type> m_real_data;
			float * m_matrix;
			size_type m_size;
			
			EMAN::Transform* m_t;
			
			cudaArray* m_cuArray;
			cudaTextureObject_t m_texObj;
			
			float* res_data_h;
			float* res_data_d;
			
			cudaStream_t m_stream;
			
			bool m_alloc_done;

		};
		
	} /* Algorithms */	
	
} /* SingleParticle2dx */

#endif /* end of include guard: CUDAPROJECTIONMETHOD_HPP_BP7HCX81 */

#endif
