#ifndef MRCFILEIO_HPP_E9Q9DSGA
#define MRCFILEIO_HPP_E9Q9DSGA

namespace SingleParticle2dx
{
	namespace Utilities
	{
		class MRCFileIO;
	}
}



#include <fstream>
#include <string>

#include <omp.h>

#include <boost/shared_ptr.hpp>
#include <boost/algorithm/minmax_element.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>

#include "../Utilities.hpp"
#include "../Typedefs.hpp"


namespace SingleParticle2dx
{
	namespace Utilities
	{
		
		/**
		 *  @brief     Class containing mrc file-operations
		 *  @details   The present class is templated on the passed data container
		 *  @author    Sebastian Scherer
		 *  @version   0.1
		 *  @date      2012
		 *  @note      Singleton Pattern
		 *  @copyright GNU Public License
		 */
		class MRCFileIO
		{
			
		public:
			
			/** Internal value type */
			typedef SingleParticle2dx::value_type value_type;
			
			
			/** Internal size type */
			typedef SingleParticle2dx::size_type size_type;
		
		
			/**
			 *  @brief      Store realspace data container to mrc file
			 *  @details    Generic function to store an entire real boost_mutli_array of any dimension
			 *  @note       value_type has to be float convertable
			 *  @param[in]  data Array to store
			 *  @param[out] filename Name of the file where to store the data container
			 *  @warning    Just tested for 2d and 3d, other dimensions don't make sense for our application
			 */
			template <typename T>
			static void writeToMrc(T* data, std::string filename)
			{
				boost::scoped_ptr<mrcHeader> head(new SingleParticle2dx::Utilities::mrcHeader);
				
				if(SingleParticle2dx::Utilities::DataContainerFunctions::checkData(data))
				{
					std::cout << "bad mrc to write, try to fix it" << std::endl;
					SingleParticle2dx::Utilities::DataContainerFunctions::fixData(data);
					if(SingleParticle2dx::Utilities::DataContainerFunctions::checkData(data))
					{
						std::cerr << "not able to fix bad mrc file" << std::endl;
						throw std::runtime_error("Bad operation");
					}
				}
					
				head.get()->nx = head.get()->mx = static_cast<SingleParticle2dx::Utilities::mrcHeader::mrc_int>(data->shape()[0]);
				head.get()->ny = head.get()->my = static_cast<SingleParticle2dx::Utilities::mrcHeader::mrc_int>(data->shape()[1]);

				if ( data->num_dimensions() == 3)
				{
					head.get()->nz = head.get()->mz = static_cast<SingleParticle2dx::Utilities::mrcHeader::mrc_int>(data->shape()[2]);
				}
				else 
				{
					head.get()->nz = head.get()->mz = static_cast<SingleParticle2dx::Utilities::mrcHeader::mrc_int>(1);
				}

				head.get()->nxStart = head.get()->nyStart = head.get()->nzStart = static_cast<SingleParticle2dx::Utilities::mrcHeader::mrc_int>(0);
				head.get()->a = static_cast<SingleParticle2dx::Utilities::mrcHeader::mrc_float>(data->shape()[0]);
				head.get()->b = static_cast<SingleParticle2dx::Utilities::mrcHeader::mrc_float>(data->shape()[1]);
				head.get()->c = static_cast<SingleParticle2dx::Utilities::mrcHeader::mrc_float>(data->shape()[2]);

				head.get()->alpha = static_cast<SingleParticle2dx::Utilities::mrcHeader::mrc_float>(90);
				head.get()->beta = static_cast<SingleParticle2dx::Utilities::mrcHeader::mrc_float>(90);
				head.get()->gamma = static_cast<SingleParticle2dx::Utilities::mrcHeader::mrc_float>(90);

				head.get()->mapc = static_cast<SingleParticle2dx::Utilities::mrcHeader::mrc_int>(1);
				head.get()->mapr = static_cast<SingleParticle2dx::Utilities::mrcHeader::mrc_int>(2);
				head.get()->maps = static_cast<SingleParticle2dx::Utilities::mrcHeader::mrc_int>(3);

				// get min and max of the data in just one for-loop by means of boost::minmax
				std::pair< value_type*, value_type* > minmax_value = boost::minmax_element(data->origin(), data->origin() + data->num_elements());
				value_type min_value = *(minmax_value.first);
				value_type max_value = *(minmax_value.second);
				value_type mean_value = SingleParticle2dx::Utilities::DataContainerFunctions::calculateMeanValue(data);
				value_type rmsd_value = SingleParticle2dx::Utilities::DataContainerFunctions::calculateRMSD(data);

				head.get()->amin = static_cast<SingleParticle2dx::Utilities::mrcHeader::mrc_float>(min_value);
				head.get()->amax = static_cast<SingleParticle2dx::Utilities::mrcHeader::mrc_float>(max_value);
				head.get()->amean = static_cast<SingleParticle2dx::Utilities::mrcHeader::mrc_float>(mean_value);
				head.get()->arms = static_cast<SingleParticle2dx::Utilities::mrcHeader::mrc_float>(rmsd_value);

				head.get()->ispg = static_cast<SingleParticle2dx::Utilities::mrcHeader::mrc_int>(0);
				head.get()->nsymbt = static_cast<SingleParticle2dx::Utilities::mrcHeader::mrc_int>(0);
				head.get()->mode = static_cast<SingleParticle2dx::Utilities::mrcHeader::mrc_int>(2);

				head.get()->xOrigin = head.get()->yOrigin = head.get()->zOrigin = static_cast<SingleParticle2dx::Utilities::mrcHeader::mrc_float>(0);

				boost::shared_ptr<FILE> file( fopen ( filename.c_str(), "wb" ), SingleParticle2dx::Utilities::CustomizedDeleter() );

				if ( (file.get() == NULL) || (static_cast<int>(fwrite(head.get(), s_mrc_header_size, 1, file.get())) != 1))
				{
					std::cerr << "IO went wrong 1" << std::endl;
					throw std::runtime_error("Bad operation");
				}
				
				#pragma omp critical (file_input_dia)
				{
					std::cout << "writing file: " << filename << " (" << head.get()->nx << "," << head.get()->ny << "," << head.get()->nz << ")" << std::endl;
				}
				
				if(SingleParticle2dx::Utilities::DataContainerFunctions::checkData(data))
				{
					std::cout << "bad mrc to write, try to fix it" << std::endl;
					SingleParticle2dx::Utilities::DataContainerFunctions::fixData(data);
					if(SingleParticle2dx::Utilities::DataContainerFunctions::checkData(data))
					{
						std::cerr << "not able to fix bad mrc file" << std::endl;
						throw std::runtime_error("Bad operation");
					}
				}

				boost::scoped_ptr<float> data2set(new SingleParticle2dx::Utilities::mrcHeader::mrc_float[head.get()->nx * head.get()->ny * head.get()->nz]);
				
				std::copy ( data->origin(), data->origin()+data->num_elements(), data2set.get() );
								
				if ( fseek( file.get() , s_mrc_header_size, std::ios::beg) != 0 )
				{
					std::cerr << "IO went wrong 2" << std::endl;
					throw std::runtime_error("Bad operation");
				}

				if ( static_cast<int>(fwrite( data2set.get(), static_cast<int>(head.get()->nx * head.get()->ny * head.get()->nz), sizeof(SingleParticle2dx::Utilities::mrcHeader::mrc_float), file.get())) != sizeof(SingleParticle2dx::Utilities::mrcHeader::mrc_float))
				{
					std::cerr << "IO went wrong 3" << std::endl;
					throw std::runtime_error("Bad operation");
				}
				
				#pragma omp critical (file_input_done)
				{
					std::cout << "\tdone" << std::endl;
				}

			}
			
			
			/**
			 *  @brief      Read mrc from file
			 *  @details    
			 *  @warning    Only MRC-mode 2 is supported
			 *  @note       If you dont't have mode 2 pease open the image in chimara and
			 *              store it as mrc. The resulting image will have mode 2.
			 *  @param[in]  filename Name of the file where to store the data container
			 *  @param[out] data Array to store
			 *  @warning    Just tested for 2d and 3d, other dimensions don't make sense for our application
			 */
			template <typename T>
			static void readFromMrc(std::string filename, T* data)
			{
				boost::scoped_ptr<mrcHeader> head(new SingleParticle2dx::Utilities::mrcHeader);
				boost::shared_ptr<FILE> file( fopen ( filename.c_str(), "r" ), SingleParticle2dx::Utilities::CustomizedDeleter() );
				
				if ( (file.get() == NULL) )
				{
					std::cerr << "file " << filename << " not there" << std::endl;
					throw std::runtime_error("Bad operation");
				}
				
				if ( (static_cast<int>(fread(head.get(), s_mrc_header_size, 1, file.get())) != 1) )
				{
					std::cerr << "can't read mrc header" << std::endl;
					throw std::runtime_error("Bad operation");
				}
				
				if (head.get()->mode != 2 && head.get()->mode != 1 && head.get()->mode != 0)
				{
					std::cerr << "unsupported MRC-mode (mode=" << head.get()->mode << ")\n";
					throw std::runtime_error("Bad operation");
					//head.get()->mode=2;
				}
								
				if ( fseek( file.get() , s_mrc_header_size, std::ios::beg) != 0 )
				{
					std::cerr << "mrc header can not be read correctly\n";
					throw std::runtime_error("Bad operation");
				}
				
				#pragma omp critical (file_input_dia)
				{
					std::cout << "reading file: " << filename << " (" << head.get()->nx << "," << head.get()->ny << "," << head.get()->nz << ")" << std::endl;
				}
				
				/*
				std::cout << "mapc: " << head.get()->mapc << std::endl;
				std::cout << "mapr: " << head.get()->mapr << std::endl;
				std::cout << "maps: " << head.get()->maps << std::endl;
				std::cout << "mode: " << head.get()->mode << std::endl;
				std::cout << "size_x: " << head.get()->nx << std::endl;
				std::cout << "size_y: " << head.get()->ny << std::endl;
				std::cout << "size_z: " << head.get()->nz << std::endl;
				std::cout << "xOrigin: " << head.get()->xOrigin << std::endl;
				std::cout << "yOrigin: " << head.get()->yOrigin << std::endl;
				std::cout << "zOrigin: " << head.get()->zOrigin << std::endl;
				std::cout << "nxStart: " << head.get()->nxStart << std::endl;
				std::cout << "nyStart: " << head.get()->nyStart << std::endl;
				std::cout << "nzStart: " << head.get()->nzStart << std::endl;
				*/
				
				struct
				{
					void resize(boost::multi_array<value_type, 2ul, std::allocator<value_type> >* data, size_type nx, size_type ny, size_type nz)
					{
						nz += 0; //dummy operation to supress warning
						data->resize( boost::extents[nx][ny]);
					}

					void resize(boost::multi_array<value_type, 3ul, std::allocator<value_type> >* data, size_type nx, size_type ny, size_type nz)
					{
						data->resize( boost::extents[nx][ny][nz]);
					}
				} resizer;
				
				resizer.resize(data, head.get()->nx, head.get()->ny, head.get()->nz);
								
				if (head.get()->mode == 1 )
				{
					boost::scoped_ptr<char> data2read(new char[2*head.get()->nx * head.get()->ny * head.get()->nz]);
					boost::scoped_ptr<float> data2write(new SingleParticle2dx::Utilities::mrcHeader::mrc_float[head.get()->nx * head.get()->ny * head.get()->nz]);

					if ( static_cast<int>(fread( data2read.get(), sizeof(char), static_cast<int>(2 * head.get()->nx * head.get()->ny * head.get()->nz)*sizeof(char) , file.get())) != static_cast<int>(2 * head.get()->nx * head.get()->ny * head.get()->nz))
					{
						std::cerr << "can not read mrc data field\n";
						throw std::runtime_error("Bad operation");
					}

					value_type tmp;
					for (size_type k=0; k<head.get()->nz; k++)
					{
						for (size_type j=0; j<head.get()->ny; j++)
						{
							for (size_type i=0; i<head.get()->nx; i++)
							{
								tmp = (float)((unsigned short*)data2read.get())[k*head.get()->nx*head.get()->ny + j*head.get()->nx + i];
								(data2write.get())[k*head.get()->nx*head.get()->ny + i*head.get()->nx + j] = tmp;
							}
						}
					}
					std::copy ( data2write.get(), data2write.get()+data->num_elements(), data->origin() );
				}
				else if (head.get()->mode == 0)
				{
					boost::scoped_ptr<char> data2read(new char[head.get()->nx * head.get()->ny * head.get()->nz]);
					boost::scoped_ptr<float> data2write(new SingleParticle2dx::Utilities::mrcHeader::mrc_float[head.get()->nx * head.get()->ny * head.get()->nz]);

					if ( static_cast<int>(fread( data2read.get(), sizeof(char), static_cast<int>(head.get()->nx * head.get()->ny * head.get()->nz)*sizeof(char) , file.get())) != static_cast<int>(head.get()->nx * head.get()->ny * head.get()->nz))
					{
						std::cerr << "can not read mrc data field\n";
						throw std::runtime_error("Bad operation");
					}

					value_type tmp;
					for (size_type k=0; k<head.get()->nz; k++)
					{
						for (size_type j=0; j<head.get()->ny; j++)
						{
							for (size_type i=0; i<head.get()->nx; i++)
							{
								tmp = (float)((unsigned char*)data2read.get())[k*head.get()->nx*head.get()->ny + j*head.get()->nx + i];
								(data2write.get())[k*head.get()->nx*head.get()->ny + i*head.get()->nx + j] = tmp;
							}
						}
					}
					//std::cout << "before copy" << std::endl;
					std::copy ( data2write.get(), data2write.get()+data->num_elements(), data->origin() );
					//std::cout << "after copy" << std::endl;
				}
				else if (head.get()->mode == 2)
				{
	//				boost::scoped_ptr<float> data2read(new SingleParticle2dx::Utilities::mrcHeader::mrc_float[head.get()->nx * head.get()->ny * head.get()->nz]);
	//				boost::scoped_ptr<float> data2write(new SingleParticle2dx::Utilities::mrcHeader::mrc_float[head.get()->nx * head.get()->ny * head.get()->nz]);
	//				
	//				if ( static_cast<int>(fread( data2read.get(), sizeof(float), static_cast<int>(head.get()->nx * head.get()->ny * head.get()->nz)*sizeof(float) , file.get())) != static_cast<int>(head.get()->nx * head.get()->ny * head.get()->nz))
	//				{
	//					std::cerr << "can not read mrc data field\n";
	//					throw std::runtime_error("Bad operation");
	//				}
	//				//std::copy ( data2read.get(), data2read.get()+data->num_elements(), data->origin() );
	//				
	//				value_type tmp;
	//				for (size_type k=0; k<head.get()->nz; k++)
	//				{
	//					for (size_type j=0; j<head.get()->ny; j++)
	//					{
	//						for (size_type i=0; i<head.get()->nx; i++)
	//						{
	//							tmp = (data2read.get())[k*head.get()->nx*head.get()->ny + j*head.get()->nx + i];
	//							(data2write.get())[k*head.get()->nx*head.get()->ny + j*head.get()->nx + i] = tmp;
	//						}
	//					}
	//				}
	//				std::copy ( data2write.get(), data2write.get()+data->num_elements(), data->origin() );
	
					boost::scoped_ptr<char> data2read(new char[4*head.get()->nx * head.get()->ny * head.get()->nz]);
					boost::scoped_ptr<float> data2write(new SingleParticle2dx::Utilities::mrcHeader::mrc_float[head.get()->nx * head.get()->ny * head.get()->nz]);

					if ( static_cast<int>(fread( data2read.get(), sizeof(char), static_cast<int>(4 * head.get()->nx * head.get()->ny * head.get()->nz)*sizeof(char) , file.get())) != static_cast<int>(4 * head.get()->nx * head.get()->ny * head.get()->nz))
					{
						std::cerr << "can not read mrc data field\n";
						throw std::runtime_error("Bad operation");
					}

					value_type tmp;
					
					if (head.get()->nz==1)
					{
						size_type k=0;
						for (size_type j=0; j<head.get()->ny; j++)
						{
							for (size_type i=0; i<head.get()->nx; i++)
							{
								tmp = (float)((float*)data2read.get())[k*head.get()->nx*head.get()->ny + j*head.get()->nx + i];
		//						std::cout << data2read.get()[k*head.get()->nx*head.get()->ny + j*head.get()->nx + i] << " " << tmp << std::endl;
								(data2write.get())[k*head.get()->nx*head.get()->ny + i*head.get()->nx + j] = tmp;
							}
						}
					}
					else
					{
						for (size_type k=0; k<head.get()->nz; k++)
						{
							for (size_type j=0; j<head.get()->ny; j++)
							{
								for (size_type i=0; i<head.get()->nx; i++)
								{
									tmp = (float)((float*)data2read.get())[k*head.get()->nx*head.get()->ny + j*head.get()->nx + i];
			//						std::cout << data2read.get()[k*head.get()->nx*head.get()->ny + j*head.get()->nx + i] << " " << tmp << std::endl;
									(data2write.get())[k*head.get()->nx*head.get()->ny + j*head.get()->nx + i] = tmp;
								}
							}
						}
					}
					
				//	std::cout << "before copy" << std::endl;
					std::copy ( data2write.get(), data2write.get()+data->num_elements(), data->origin() );
				//	std::cout << "after copy" << std::endl;
					
				}
				
				//std::cout << (*data)[10][10] << std::endl;
			//	SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(data);
				//std::cout << (*data)[10][10] << std::endl;
				
				if(SingleParticle2dx::Utilities::DataContainerFunctions::checkData(data))
				{
					std::cout << "bad mrc read, try to fix it" << std::endl;
					SingleParticle2dx::Utilities::DataContainerFunctions::fixData(data);
					if(SingleParticle2dx::Utilities::DataContainerFunctions::checkData(data))
					{
						std::cerr << "not able to fix bad mrc file" << std::endl;
						throw std::runtime_error("Bad operation");
					}
				}
				
				//fclose(file.get());
			}
			
		private:
		
			/** mrc header size */
			static const size_type s_mrc_header_size = 1024;
		};
		
	} /* Utilities */
	
} /* SingleParticle2dx */

#endif /* end of include guard: MRCFILEIO_HPP_E9Q9DSGA */
