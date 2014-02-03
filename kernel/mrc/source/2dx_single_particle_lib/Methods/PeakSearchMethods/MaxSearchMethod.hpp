#ifndef PEAKSEARCHALGORITHM_HPP_OV821WKY
#define PEAKSEARCHALGORITHM_HPP_OV821WKY

namespace SingleParticle2dx
{
	namespace Methods
	{
		class MaxSearchMethod;
	}
}

namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class ParticleShiftt;
	} 
}


#include "AbstractPeakSearchMethod.hpp"


namespace SingleParticle2dx
{
	
	namespace Methods
	{
		
		/**
		 *  @brief     Abstract max search method class performing peak-search
		 *  @details   
		 *  @author    Sebastian Scherer
		 *  @version   0.1
		 *  @date      2012
		 *  @copyright GNU Public License
		 */
		class MaxSearchMethod : public SingleParticle2dx::Methods::AbstractPeakSearchMethod
		{
			
		public:
			
			/**
			 *  @brief      Constructor
			 *  @details    Costum constructor
			 */
			MaxSearchMethod ();
			
			
			/**
			 *  @brief      Destructor
			 *  @details    All the allocated memory is freed afer calling the Destructor
			 */
			virtual ~MaxSearchMethod ();
		
			
			/**
			 *  @brief      Finds the highest CC-Peak and stores shift
			 *  @details    
			 *  @param[in]  cc_array Cross-Correlation profile
			 *  @param[out] shift Optimal shift
			 *  @post       Particle p inserted by means of back-projection
			 */
			virtual value_type findMaximalElementAndSetShift(real_array2d_type& cc_array, SingleParticle2dx::DataStructures::ParticleShift& shift);
			
		};
		
	} /* Algorithms */
	
} /* SingleParticle2dx */

#endif /* end of include guard: PEAKSEARCHALGORITHM_HPP_OV821WKY */
