#ifndef CUSTOMIZEDDELETER_HPP
#define CUSTOMIZEDDELETER_HPP

namespace SingleParticle2dx
{
	namespace Utilities
	{
		class CustomizedDeleter;
	}
}


#include <fstream>
#include <string>
#include <iostream>

#include <fftw3.h>


namespace SingleParticle2dx
{
	
	namespace Utilities
	{
		/**
		 *  @brief     Customized deleters for deleting special objects stored in boost::shared_ptr
		 *  @details   Used to call a costumized destructor when deleting a shared_ptr with a non
		 *             trivial content. To add new functions just realize a new implementation of 
		 *             the ()-operator with the dedicated type
		 *  @warning   It's your job to delete all the allocated resources here, nobody will call
		 *             a default constructor for you!!
		 *  @author    Sebastian Scherer
		 *  @version   0.1
		 *  @date      2012
		 *  @copyright GNU Public License
		 */
		class CustomizedDeleter
		{

		public:

			/**
			 *  @brief      Functor to delete files
			 *  @post       All memory freed
			 */
			void operator()(FILE* file)
			{
				if(file != 0)
				{
					fclose(file);
				}
				else
				{
					std::cerr << "Can't close a file" << std::endl;
					throw std::runtime_error("Bad operation");
				}
			}


			/**
			 *  @brief      Functor to delete fft arrays allocated by means of FFTW3
			 *  @post       All memory freed
			 */
			void operator()(fftwf_complex* fftarray)
			{
				fftwf_free( fftarray );
			}

			
			void operator()(std::ifstream* file)
			{
				if(file != 0)
				{
					file->close();
				}
			}

		};
		
	} /* Utilities */
	
} /* SingleParticle2dx */

#endif /* end of include guard: CUSTOMIZEDDELETER_HPP */
