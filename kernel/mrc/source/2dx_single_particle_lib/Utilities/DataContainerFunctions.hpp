#ifndef DATACONTAINERFUNCTIONS_HPP_LY7U6N3I
#define DATACONTAINERFUNCTIONS_HPP_LY7U6N3I

namespace SingleParticle2dx
{
	namespace Utilities
	{
		class DataContainerFunctions;
	}
}

#include <iostream>
#include <fstream>

#include <time.h>
#include <boost/random/normal_distribution.hpp>
#include <boost/random/mersenne_twister.hpp>
#include <boost/random/variate_generator.hpp>
#include <boost/random/uniform_real.hpp>

#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>

#include <boost/accumulators/accumulators.hpp>
#include <boost/accumulators/statistics/stats.hpp>
#include <boost/accumulators/statistics/mean.hpp>
#include <boost/accumulators/statistics/variance.hpp>
#include <boost/accumulators/statistics/min.hpp>
#include <boost/accumulators/statistics/max.hpp>

#include <boost/algorithm/minmax_element.hpp>

#include "CTFParameters.hpp"
#include "FFTCalculator.hpp"

#include "../DataStructures/CTFInformation.hpp"


#include "../Config.hpp"

#ifdef USE_CILK
	#include <cilk/cilk.h>
#endif


namespace SingleParticle2dx
{
	
	namespace Utilities
	{
		
		/**
		 *  @brief     Templated generic functions to operate on a data container
		 *  @details   
		 *  @author    Sebastian Scherer
		 *  @version   0.1
		 *  @date      2012
		 *  @copyright GNU Public License
		 */
		class DataContainerFunctions
		{

		public:
			
			/** Size type */
			typedef SingleParticle2dx::size_type size_type;

			
			/** Value type */
			typedef SingleParticle2dx::value_type value_type;


			/** Complex number type */
			typedef SingleParticle2dx::fft_type fft_type;
			
			typedef SingleParticle2dx::fft_array2d_type fft_array2d_type;
			typedef SingleParticle2dx::real_array2d_type real_array2d_type;

			
			/**
			 *  @brief      Reset fft array
			 *  @details    Generic function to set an entire boost_mutli_array to 0+0i for any dimension
			 *  @param[in]  data Array to reset
			 *  @post       data set to zero
			 */
			template <typename T>
			static void resetData(T* data)
			{
				typename T::element zero = static_cast<typename T::element>(0);
				std::fill (data->origin(), data->origin() + data->num_elements(), zero);
			}

			
			template <typename T>
			static void invertData(T* data)
			{
				#ifdef USE_CILK
					(data->origin())[0:data->num_elements()] = -(data->origin())[0:data->num_elements()];
				#else
					using namespace boost::lambda;
					std::for_each(data->origin(), data->origin() + data->num_elements(), _1=-_1 );
				#endif
				
				normalizeRealSpaceData(data);
			}
			
			
			template <typename T> 
			static bool checkData(boost::multi_array<T, 2ul, std::allocator<T> >* data)
			{
				value_type n = data->shape()[0];
				
				for(size_type i=0; i < static_cast<size_type>(data->shape()[0]); i++)
				{
					for(size_type j=0; j < static_cast<size_type>(data->shape()[1]); j++)
					{
						if ( !std::isfinite((*data)[i][j]) )
						{
							std::cout << "check data failed: " << (*data)[i][j] << "@ " << i << "," << j << std::endl;
							return true;
						}
					}
				}
				return false;
			}
			
			
			template <typename T> 
			static bool checkData(boost::multi_array<T, 3ul, std::allocator<T> >* data)
			{
				value_type n = data->shape()[0];
				
				for(size_type i=0; i < static_cast<size_type>(data->shape()[0]); i++)
				{
					for(size_type j=0; j < static_cast<size_type>(data->shape()[1]); j++)
					{
						for(size_type k=0; k < static_cast<size_type>(data->shape()[2]); k++)
						{
							if ( !std::isfinite((*data)[i][j][k]) )
							{
								std::cerr << (*data)[i][j][k] << std::endl;
								return true;
							}
						}
					}
				}
				return false;
			}
			
			
			
			template <typename T> 
			static void fixData(boost::multi_array<T, 2ul, std::allocator<T> >* data)
			{
				value_type n = data->shape()[0];
				
				for(size_type i=0; i < static_cast<size_type>(data->shape()[0]); i++)
				{
					for(size_type j=0; j < static_cast<size_type>(data->shape()[1]); j++)
					{
						if ( !std::isfinite((*data)[i][j]) )
						{
						//	std::cout << i << ", " << j << " " << (*data)[i][j] << std::endl;
							(*data)[i][j] = 0;
						}
					}
				}
			}
			
			
			template <typename T> 
			static void fixData(boost::multi_array<T, 3ul, std::allocator<T> >* data)
			{
				value_type n = data->shape()[0];
				
				for(size_type i=0; i < static_cast<size_type>(data->shape()[0]); i++)
				{
					for(size_type j=0; j < static_cast<size_type>(data->shape()[1]); j++)
					{
						for(size_type k=0; k < static_cast<size_type>(data->shape()[2]); k++)
						{
							if ( !std::isfinite((*data)[i][j][k]) )
							{
								(*data)[i][j][k] = 0;
							}
						}
					}
				}
			}
			
			template <typename T>
			static void normalizeFourierSpaceData(T* data)
			{
				using namespace boost::lambda;
				value_type power = sqrt(std::accumulate(data->origin(), data->origin() + data->num_elements(), value_type(0), bind(&abs,_1)*bind(&abs,_1)));
				std::for_each(data->origin(), data->origin() + data->num_elements(), _1 /= power);
			}
			
			
			template <typename T> 
			static void applyBinning(boost::multi_array<T, 2ul, std::allocator<T> >* data)
			{
				value_type n = data->shape()[0];
				
				boost::multi_array<T, 2ul, std::allocator<T> > data_new( (boost::extents[n][n]) );
				resetData(&data_new);
				
				for(size_type i=0; i < static_cast<size_type>(data->shape()[0]); i++)
				{
					for(size_type j=0; j < static_cast<size_type>(data->shape()[1]); j++)
					{
						if ( (i>=n/4) && (j>=n/4) && (i<0.75*n) && (j<0.75*n))
						{
							int ii = i/2 + n/4;
							int jj = j/2 + n/4;
							data_new[ii][jj] += (*data)[i][j];
						} 
					}
				}
				
				std::copy(data_new.origin(), data_new.origin() + data_new.num_elements(), data->origin());
			}
			
			
			template <typename T> 
			static void applyBinning(boost::multi_array<T, 3ul, std::allocator<T> >* data)
			{
				value_type nx = data->shape()[0];
				value_type ny = data->shape()[1];
				value_type nz = data->shape()[2];
				
				boost::multi_array<T, 3ul, std::allocator<T> > data_new( (boost::extents[nx/2][ny/2][nz/2]) );
				resetData(&data_new);
				
				for(size_type i=0; i < static_cast<size_type>(data->shape()[0]); i++)
				{
					for(size_type j=0; j < static_cast<size_type>(data->shape()[1]); j++)
					{
						for(size_type k=0; k < static_cast<size_type>(data->shape()[2]); k++)
						{
							data_new[i/2][j/2][k/2] += (*data)[i][j][k];
						} 
					}
				}
				
				boost::multi_array<T, 3ul, std::allocator<T> > data_new2( (boost::extents[nx][ny][nz]) );
				resetData(&data_new2);
				
				for(size_type i=0; i < static_cast<size_type>(data_new.shape()[0]); i++)
				{
					for(size_type j=0; j < static_cast<size_type>(data_new.shape()[1]); j++)
					{
						for(size_type k=0; k < static_cast<size_type>(data_new.shape()[2]); k++)
						{
							data_new2[nx/2+i][ny/2+j][nz/2+k] = data_new[i][j][k];
							//data_new2[i][j][nz/2+1] = data_new[i][j][k];
						}
					}
				}
				
				std::copy(data_new2.origin(), data_new2.origin() + data_new2.num_elements(), data->origin());
			}
			
					
			template <typename T> 
			static void applyLowPassFilter(boost::multi_array<T, 2ul, std::allocator<T> >* data, size_type r_max, value_type relax_param)
			{
				value_type n = data->shape()[0];
				//n *= 0.46;
				value_type r, w, dr;

				for(size_type i=0; i < static_cast<size_type>(data->shape()[0]); i++)
				{
					for(size_type j=0; j < static_cast<size_type>(data->shape()[1]); j++)
					{
						r = sqrt((i-n/2)*(i-n/2) + (j-n/2)*(j-n/2));
						dr = r - r_max;
						if ( r < r_max )
						{
							w = 1;
						}
						else
						{
							w = exp( - fabs(dr) * relax_param);
						}
						(*data)[i][j] *= w;
					}
				}
			}
			
			
			template <typename T>
			static void applyLowPassFilter(boost::multi_array<T, 3ul, std::allocator<T> >* data, size_type r_max, value_type relax_param)
			{
				size_type n = data->shape()[0];
				//n = 65;

				value_type r, w, dr;

				for(size_type i=0; i<n; i++)
				{
					for(size_type j=0; j<n; j++)
					{
						for(size_type k=0; k<n; k++)
						{
							r = sqrt((i-n/2)*(i-n/2) + (j-n/2)*(j-n/2) + (k-n/2)*(k-n/2));
							dr = r - r_max;
							
							if ( r < r_max )
							{
								w = 1;
							}
							else
							{
								w = exp( - fabs(dr) * relax_param);
							}
						
							(*data)[i][j][k] *= w;
						}
					}
				}
			}
			
			
			template <typename T> 
			static void equalizeHistogram(boost::multi_array<T, 2ul, std::allocator<T> >* data)
			{
				fixData(data);
				
				value_type nx = data->shape()[0];
				value_type ny = data->shape()[1];
				boost::multi_array<int, 2ul, std::allocator<int> > data2( (boost::extents[nx][ny]) );
				
				value_type* data_as_array = new value_type[static_cast<int>(nx*ny)];
				std::copy(data->origin(), data->origin() + data->num_elements(), data_as_array);
				std::pair<value_type*,value_type*> result = boost::minmax_element(data_as_array, data_as_array + data->num_elements());
				
				std::vector<size_type> count (256, size_type(0));
				std::vector<size_type> cdf (256, size_type(0));
				std::vector<size_type> cdf_nz (256, size_type(0));
				
				for (size_type i=0; i<nx; i++)
				{
					for (size_type j=0; j<ny; j++)
					{
						value_type aux = (*data)[i][j];
						data2[i][j] = floor(  (aux- *result.first)  / (*result.second - *result.first) * 255);
					
						if ( (data2[i][j]<0) || (data2[i][j]>255) || !std::isfinite(data2[i][j]) )
						{
							std::cout << "error in data2[][] " << data2[i][j] << " " << *result.first << " " << *result.second << " " << aux << " " << calculateSD(data) << std::endl;
						}
						
						count[data2[i][j]]++;
					}
				}
				
				for (size_type i=0; i<256; i++)
				{
					size_type sum = 0;
					for(size_type j=0; j<=i; j++)
					{
						sum += count[j];
					}
					cdf[i] = sum;
					cdf_nz[i] = sum;
				}
			
				delete[] data_as_array;
				
				for (size_type i=0; i<256; i++)
				{
					if (cdf_nz[i] == 0)
					{
						cdf_nz[i] = nx*ny;
					}
				}
				
				size_type min_cdf = *(std::min_element(cdf_nz.begin(), cdf_nz.end()));
				
				std::vector<size_type> h (256, size_type(0));
				for (size_type i=0; i<256; i++)
				{
					h[i] = floor(  value_type(cdf[i]-min_cdf) / value_type(nx*ny-min_cdf) * 255 );
				}
				
				for (size_type i=0; i<nx; i++)
				{
					for (size_type j=0; j<ny; j++)
					{
						(*data)[i][j] = h[data2[i][j]];
					}
				}
				
				normalizeRealSpaceData(data);
			}
			
			
			template <typename T> 
			static void applyHighPassFilter(boost::multi_array<T, 2ul, std::allocator<T> >* data, size_type r_max, value_type relax_param)
			{
				SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
				
				value_type r, dr, w;
				value_type n = data->shape()[0];

				for(size_type i=0; i < static_cast<size_type>(data->shape()[0]); i++)
				{
					for(size_type j=0; j < static_cast<size_type>(data->shape()[1]); j++)
					{
						r = sqrt((i-n/2)*(i-n/2) + (j-n/2)*(j-n/2));
						if ( r < r_max )
						{
							dr = r - r_max;
							w = exp(-dr * relax_param );
						}
						else
						{
							w = 1;
						}
						
						(*data)[i][j] *= w; 
					}
				}
			}			
			
			
			template <typename T>
			static void fillRandomFourierSpace(boost::multi_array<T, 3ul, std::allocator<T> >* data, value_type max_tilt_deg, value_type damping)
			{
				SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
				size_type n = data->shape()[0];
				value_type max_tilt_rad = max_tilt_deg * config->getPI() / 180.0;
				
				std::ofstream myfile;
				myfile.open("hist_file_cone.txt");
				
				boost::accumulators::accumulator_set<value_type, boost::accumulators::stats<boost::accumulators::tag::mean, boost::accumulators::tag::variance(boost::accumulators::lazy)> > acc_real;
				boost::accumulators::accumulator_set<value_type, boost::accumulators::stats<boost::accumulators::tag::mean, boost::accumulators::tag::variance(boost::accumulators::lazy)> > acc_imag;

				for(size_type i=0; i<n; i++)
				{
					for(size_type j=0; j<n; j++)
					{
						for(size_type k=0; k<n; k++)
						{
							float r_loc = sqrt((i-n/2)*(i-n/2) + (j-n/2)*(j-n/2));
							if ( (r_loc>=(tan(max_tilt_rad)*fabs(k-n/2))) && ( fabs((*data)[k][j][i].real()) > 0.01) && ( fabs((*data)[k][j][i].imag()) > 0.01) )
							{
								acc_real((*data)[k][j][i].real());
								acc_imag((*data)[k][j][i].imag());
								//fprintf(file, "%s\t%s\t%s\t%s\t%s\n", "nb", "weight", "sim", "qual", "cons");
								myfile << (*data)[k][j][i].real() << "\t" << (*data)[k][j][i].imag() << std::endl;
							}
						}
					}
				}

				myfile.close();

				boost::mt19937 baseGen(time(0));

//				boost::uniform_real<> uniDblUnit_real(boost::accumulators::extract_result<boost::accumulators::tag::min>(acc_real),boost::accumulators::extract_result<boost::accumulators::tag::max>(acc_real));
//				boost::uniform_real<> uniDblUnit_imag(boost::accumulators::extract_result<boost::accumulators::tag::min>(acc_imag),boost::accumulators::extract_result<boost::accumulators::tag::max>(acc_imag));
				
				typedef boost::normal_distribution<value_type> DIST;
				
				DIST uniDblUnit_real(boost::accumulators::extract_result<boost::accumulators::tag::mean>(acc_real), sqrtf(boost::accumulators::extract_result<boost::accumulators::tag::variance>(acc_real)));  
				DIST uniDblUnit_imag(boost::accumulators::extract_result<boost::accumulators::tag::mean>(acc_imag), sqrtf(boost::accumulators::extract_result<boost::accumulators::tag::variance>(acc_imag)));  
				
				std::cout << "mean_real " << boost::accumulators::extract_result<boost::accumulators::tag::mean>(acc_real) << std::endl;
				std::cout << "mean_imag " << boost::accumulators::extract_result<boost::accumulators::tag::mean>(acc_imag) << std::endl;
				
				std::cout << "sigma_real " << sqrtf(boost::accumulators::extract_result<boost::accumulators::tag::variance>(acc_real)) << std::endl;
				std::cout << "sigma_imag " << sqrtf(boost::accumulators::extract_result<boost::accumulators::tag::variance>(acc_imag)) << std::endl;
				
				boost::variate_generator<boost::mt19937&, DIST > generator_real(baseGen, uniDblUnit_real);
				boost::variate_generator<boost::mt19937&, DIST > generator_imag(baseGen, uniDblUnit_imag);
				
				std::ofstream myfile2;
				myfile2.open("hist_file_cone_2.txt");
				
				for(size_type i=0; i<n; i++)
				{
					for(size_type j=0; j<n; j++)
					{
						for(size_type k=0; k<n; k++)
						{
							float r_loc = sqrt((i-n/2)*(i-n/2) + (j-n/2)*(j-n/2));
							if ( r_loc < (tan(max_tilt_rad)*fabs(k-n/2)) )
							{
								(*data)[k][j][i].real(damping * generator_real());
								(*data)[k][j][i].imag(damping * generator_imag());
								myfile2 << damping * generator_real() << "\t" << damping * generator_imag() << std::endl;
							}
							else if ( (fabs((*data)[k][j][i].real()) < 0.01) || (fabs((*data)[k][j][i].imag()) < 0.01) )
							{
								(*data)[k][j][i].real(damping * generator_real());
								(*data)[k][j][i].imag(damping * generator_imag());
							}
						}
					}
				}
				
				myfile2.close();
				
			}
			
		
			/**
			 *  @brief      Normalize realspace data container
			 *  @details    Generic function to normalize an entire boost_mutli_array of any dimension
			 *  @param[in]  data Array to normalize
			 *  @post       data set normalized to zero mean and standard deviation one
			 *  @note       Throws an exception if the standard deviation is too small
			 */
		/*
    		template <typename T>
    		static void normalizeRealSpaceData(T* data)
    		{				
    			using namespace boost::lambda;
    			value_type mean_value;
    			value_type sd;
    			value_type n = data->num_elements();
    		
				boost::accumulators::accumulator_set<value_type, boost::accumulators::stats<boost::accumulators::tag::mean, boost::accumulators::tag::variance(boost::accumulators::lazy)> > acc;
				
				for (typename T::iterator it = data->begin() ; it != data->end(); ++it)
				{
					acc(*((*it).origin()));
				}
				
				mean_value = boost::accumulators::extract_result<boost::accumulators::tag::mean>(acc);
				sd = sqrt(fabs(boost::accumulators::extract_result<boost::accumulators::tag::variance>(acc)));
				
				#ifdef USE_CILK
					(data->origin())[0:data->num_elements()] = ((data->origin())[0:data->num_elements()] - mean_value)/sd  ;
				#else
					std::for_each(data->origin(), data->origin() + data->num_elements(), _1=(_1-mean_value)/sd);
				#endif	
    		}
	*/
  
  
			template <typename T> 
			static void normalizeRealSpaceData(boost::multi_array<T, 2ul, std::allocator<T> >* data)
			{		
				value_type n = data->shape()[0] * data->shape()[1];
				value_type tmp;
				value_type e2 = 0;
				value_type e = 0;
				
		//		std::cout << "test_out: " << (*data)[10][10] << std::endl;
				
		//		for(size_type i=0; i<(static_cast<size_type>(data->shape()[0]) * static_cast<size_type>(data->shape()[1])); i++)
			//	{
					
			//	} 
				
				for(size_type i=0; i < static_cast<size_type>(data->shape()[0]); i++)
				{
					for(size_type j=0; j < static_cast<size_type>(data->shape()[1]); j++)
					{
						tmp = (*data)[i][j];
						e2 += tmp*tmp;
						e += tmp;
					}
				}
				
				e2 /= n;
				e /= n;
				value_type sd = sqrt(e2-e*e);
				
		//		std::cout << "mean:" << e << std::endl;
		//		std::cout << "mean2:" << e2 << std::endl;
		//		std::cout << "test2:" << e2-e*e << std::endl;
		//		std::cout << "sd:" << sd << std::endl;
				
				for(size_type i=0; i < static_cast<size_type>(data->shape()[0]); i++)
				{
					for(size_type j=0; j < static_cast<size_type>(data->shape()[1]); j++)
					{
						(*data)[i][j] = ((*data)[i][j] - e) / sd;
					}
				}
				
			}
			
			
			template <typename T> 
			static void normalizeRealSpaceData(boost::multi_array<T, 3ul, std::allocator<T> >* data)
			{
				value_type n = data->shape()[0] * data->shape()[1] * data->shape()[2];
				value_type tmp;
				value_type e2 = 0;
				value_type e = 0;
				
				for(size_type i=0; i < static_cast<size_type>(data->shape()[0]); i++)
				{
					for(size_type j=0; j < static_cast<size_type>(data->shape()[1]); j++)
					{
						for(size_type k=0; k < static_cast<size_type>(data->shape()[2]); k++)
						{
							tmp = (*data)[i][j][k];
							e2 += tmp*tmp/n;
							e += tmp/n;
						}
					}
				}
				
				value_type sd = sqrt(e2-e*e);
				
				for(size_type i=0; i < static_cast<size_type>(data->shape()[0]); i++)
				{
					for(size_type j=0; j < static_cast<size_type>(data->shape()[1]); j++)
					{
						for(size_type k=0; k < static_cast<size_type>(data->shape()[2]); k++)
						{
							(*data)[i][j][k] = ((*data)[i][j][k] - e) / sd;
						}
					}
				}
			}
			
		
	/*		
			template <typename T> 
			static void normalizeRealSpaceData(boost::multi_array<T, 2ul, std::allocator<T> >* data)
			{
				value_type n = data->shape()[0];
				value_type r;
				value_type r_max = SingleParticle2dx::ConfigContainer::Instance()->getParticleMaskingRadius();
				value_type e = 0;
				value_type e2 = 0;
				size_type ne = 0;
				
				for(size_type i=0; i < static_cast<size_type>(data->shape()[0]); i++)
				{
					for(size_type j=0; j < static_cast<size_type>(data->shape()[1]); j++)
					{
						r = sqrt((i-n/2)*(i-n/2) + (j-n/2)*(j-n/2));
						if ( r > r_max )
						{
							e += (*data)[i][j];
							e2 += (*data)[i][j] * (*data)[i][j];
							ne++;
						}
					}
				}
				
				e /= ne;
				e2 /= ne;
				value_type var = e2 - e*e;
				
				if (fabs(var) < 1e-5)
				{
					std::cerr << "sd too small" << std::endl;
					throw std::runtime_error("Bad operation");
				}
				
				for(size_type i=0; i < static_cast<size_type>(data->shape()[0]); i++)
				{
					for(size_type j=0; j < static_cast<size_type>(data->shape()[1]); j++)
					{
						(*data)[i][j] = ((*data)[i][j] - e) / sqrt(var);
					}
				}
			}
			
			
			template <typename T> 
			static void normalizeRealSpaceData(boost::multi_array<T, 3ul, std::allocator<T> >* data)
			{
				using namespace boost::lambda;
				value_type mean_value = value_type(0);
				value_type mean_value2 = value_type(0);
				size_type n = data->num_elements();

				// calc mean value and squared mean value by means of a bosst lambda function
				std::for_each(data->origin(), data->origin() + data->num_elements(), (mean_value+=_1/n) && (mean_value2+=(_1*_1)/n) );
				value_type sd = sqrt( mean_value2 - mean_value*mean_value );

				if (fabs(sd) < 1e-10)
				{
					sd = 1.0;
				}
				
				// normalization loop based on a boost lambda function
				std::for_each(data->origin(), data->origin() + data->num_elements(), _1=(_1-mean_value)/sd);
			}
		*/	
			
			template <typename T>
			static void devideBySqrtAv(T* data)
			{
				using namespace boost::lambda;
				value_type max = value_type(0);
				size_type n = data->num_elements();
				
				// calc mean value and squared mean value by means of a bosst lambda function
				std::for_each(data->origin(), data->origin() + data->num_elements(), max+=(_1/(n*n*n)) );
				max = sqrt(max);

				if (fabs(max) < 1e-4)
				{
					std::cout << "max=" << max << " too small\n";
					throw std::runtime_error("Bad operation");
				}
				
				// normalization loop based on a boost lambda function
				std::for_each(data->origin(), data->origin() + data->num_elements(), _1=(_1/max));	
			}
			
			
			template <typename T>
			static void addGaussianNoise(T* data, value_type sd = value_type(1))
			{
				boost::variate_generator<boost::mt19937, boost::normal_distribution<> > generator(boost::mt19937(time(0)), boost::normal_distribution<>());
				typename T::element *p;
				
				for ( p = data->origin(); p != data->origin() + data->num_elements(); p++)
				{
					 (*p) = (*p) + (sd*generator());
				}
			}


			template <typename T>
			static void applyMirrorX(boost::multi_array<T, 2ul, std::allocator<T> >* data)
			{
				size_type n = static_cast<size_type>(data->shape()[0]);
				value_type tmp;	
				for(size_type i=0; i < static_cast<size_type>(data->shape()[0]/2); i++)
				{
					for(size_type j=0; j < static_cast<size_type>(data->shape()[1]); j++)
					{
						tmp = (*data)[j][i];
						(*data)[j][i] = (*data)[j][n-1-i];
						(*data)[j][n-1-i] = tmp;
					}
				}
			}
			
			
			template <typename T>
			static void applyMirrorY(boost::multi_array<T, 2ul, std::allocator<T> >* data)
			{
				size_type n = static_cast<size_type>(data->shape()[0]);
				value_type tmp;	
				for(size_type i=0; i < static_cast<size_type>(data->shape()[0]/2); i++)
				{
					for(size_type j=0; j < static_cast<size_type>(data->shape()[1]); j++)
					{
						tmp = (*data)[i][j];
						(*data)[i][j] = (*data)[n-i-1][j];
						(*data)[n-i-1][j] = tmp;
					}
				}
			}
			
			
			template <typename T>
			static void applyMirrorXY(boost::multi_array<T, 2ul, std::allocator<T> >* data)
			{
				size_type n = static_cast<size_type>(data->shape()[0]);
				value_type tmp;	
				for(size_type i=0; i < static_cast<size_type>(data->shape()[0]/2); i++)
				{
					for(size_type j=0; j < static_cast<size_type>(data->shape()[1]); j++)
					{
						tmp = (*data)[i][j];
						(*data)[i][j] = (*data)[j][i];
						(*data)[j][i] = tmp;
					}
				}
			}


			/**
			 *  @brief      Calculate the mean value of a data container
			 *  @details    Generic function to calculate the mean value of an entire boost_mutli_array of any dimension
			 *  @note       value_type of the boost_multiarray has to support the following operators: "+", "/" and "="
			 *  @param[in]  data Array to calculate the mean value
			 *  @return     Mean value of data
			 */
			template <typename T>
			static value_type calculateMeanValue(T* data)
			{
				using namespace boost::lambda;
				size_type n = data->num_elements();
				value_type mean_value = value_type(0);
				std::for_each (data->origin(), data->origin() + data->num_elements(), mean_value+=_1/n);
				return mean_value;
			}


	//   	template <typename T>
	//   	static value_type calculateSD(T* data)
	//   	{
	//   		using namespace boost::lambda;
	//   		value_type mean_value = value_type(0);
	//   		value_type mean_value2 = value_type(0);
	//   		size_type n = data->num_elements();
	//   		
	//   		// calc mean value and squared mean value by means of a bosst lambda function
	//   		std::for_each(data->origin(), data->origin() + data->num_elements(), (mean_value+=_1/n) && (mean_value2+=(_1*_1)/n) );
	//   		value_type sd = sqrt( mean_value2 - mean_value*mean_value );
	//   		return sd;	
	//   	}
			
			
			template <typename T> 
			static value_type calculateSD(boost::multi_array<T, 2ul, std::allocator<T> >* data)
			{
				value_type n = data->shape()[0] * data->shape()[1];
				value_type tmp = 0;
				value_type e2 = 0;
				value_type e = 0;
				
	//			std::cout << "test_sd: " << (*data)[10][10] << std::endl;
				
				for(size_type i=0; i < static_cast<size_type>(data->shape()[0]); i++)
				{
					for(size_type j=0; j < static_cast<size_type>(data->shape()[1]); j++)
					{
						tmp = (*data)[i][j];
						e2 += tmp*tmp;
						e += tmp;
					}
				}
				
				e2 /= n;
				e /= n;
				
				value_type aux = e2-e*e;
				value_type sd;
				
				if(aux >= 0)
				{
					sd = sqrt(aux);
				}
				else
				{
					//std::cout << "resetting sd" << std::endl;
					sd = 0;
				}
				
				return sd;
			}
			
			
			template <typename T> 
			static value_type calculateSD(boost::multi_array<T, 3ul, std::allocator<T> >* data)
			{
				value_type n = data->shape()[0] * data->shape()[1] * data->shape()[2];
				value_type tmp;
				value_type e2 = 0;
				value_type e = 0;
				
				for(size_type i=0; i < static_cast<size_type>(data->shape()[0]); i++)
				{
					for(size_type j=0; j < static_cast<size_type>(data->shape()[1]); j++)
					{
						for(size_type k=0; k < static_cast<size_type>(data->shape()[2]); k++)
						{
							tmp = (*data)[i][j][k];
							e2 += tmp*tmp/n;
							e += tmp/n;
						}
					}
				}
				
				value_type sd = sqrt(e2-e*e);
				
				return sd;
				
			}


			/**
			 *  @brief      Calculate the normalized root mean square deviation of a data container
			 *  @details    Generic function to calculate the mean value of an entire boost_mutli_array of any dimension
			 *  @note       value_type of the boost_multiarray has to support the following operators: "+", "/", "*" and "="
			 *  @param[in]  data Array to calculate the root mean square deviation
			 *  @return     Normalized root mean square deviation
			 */
			template <typename T>
			static value_type calculateRMSD(T* data)
			{
				using namespace boost::lambda;
				value_type mean_value = SingleParticle2dx::Utilities::DataContainerFunctions::calculateMeanValue(data);
				value_type n = static_cast<value_type>(data->num_elements());
				value_type rmsd = value_type(0);
				std::for_each (data->origin(), data->origin() + data->num_elements(), rmsd += bind(&calcSquaredDiff, _1, mean_value, n) );
				return sqrt(rmsd);
			}
			
			
			template <typename T> 
			static void addRealData(boost::multi_array<T, 2ul, std::allocator<T> >* data_in, value_type w, boost::multi_array<T, 2ul, std::allocator<T> >* data_out)
			{
				for(size_type i=0; i < static_cast<size_type>(data_in->shape()[0]); i++)
				{
					for(size_type j=0; j < static_cast<size_type>(data_in->shape()[1]); j++)
					{
						(*data_out)[i][j] += w * (*data_in)[i][j];
					}
				}
			}
			
			
			template <typename T> 
			static void addData(boost::multi_array<T, 3ul, std::allocator<T> >* data_in, value_type w, boost::multi_array<T, 3ul, std::allocator<T> >* data_out)
			{
				size_type n = static_cast<size_type>(data_in->shape()[0]);
				for(size_type i=0; i<n; i++)
				{
					for(size_type j=0; j<n; j++)
					{
						for(size_type k=0; k<n; k++)
						{
							(*data_out)[i][j][k] += w * (*data_in)[i][j][k];
						}
					}
				}
			}
			
			
			template <typename T> 
			static void applyWeight(boost::multi_array<T, 2ul, std::allocator<T> >* data, value_type w)
			{
				T tmp;
				for(size_type i=0; i < static_cast<size_type>(data->shape()[0]); i++)
				{
					for(size_type j=0; j < static_cast<size_type>(data->shape()[1]); j++)
					{
						tmp = (*data)[i][j];
						(*data)[i][j] = w * tmp;
					}
				}
			}
			
			
			template <typename T>
			static void correctCTF(boost::multi_array<T, 2ul, std::allocator<T> >* data, SingleParticle2dx::Utilities::CTFParameters& ctf_params, SingleParticle2dx::DataStructures::CTFParticleInformation& ctf_part, bool skip_apply=false)
			{
				size_type n = data->shape()[0];

				value_type pi = SingleParticle2dx::ConfigContainer::Instance()->getPI();

				size_type posy = (ctf_params.m_posx - ctf_params.m_size_x/2);
				size_type posx = -(ctf_params.m_posy - ctf_params.m_size_y/2);

				value_type x = posx * ctf_params.m_dstep * 10000 / ctf_params.m_mag;
				value_type y = posy * ctf_params.m_dstep * 10000 / ctf_params.m_mag;

				value_type p = sqrt(x*x + y*y) * sin(-atan2(y,x) + (ctf_params.m_tltaxis*pi/180) );

				value_type difmid1 = ctf_params.m_difmid1 + p*tan( ctf_params.m_tltang*pi/180 );
				value_type difmid2 = ctf_params.m_difmid2 + p*tan( ctf_params.m_tltang*pi/180 );
		
				ctf_part.setDefocus(difmid1/10000.0);
				ctf_part.setCs(ctf_params.m_cs);
				ctf_part.setVoltage(ctf_params.m_kv);
				ctf_part.setApix(1);
				ctf_part.setAmpCon(0.1);
				ctf_part.setBfactor(0);
				ctf_part.setAst(0);
				ctf_part.setAngAst(0);
				
				if(skip_apply)
				{
					return;
				}
				
				fft_array2d_type fftdata((boost::extents[n][n]));
				SingleParticle2dx::Utilities::FFTCalculator::performForwardFFT(data, &fftdata);
				
				real_array2d_type ctf( boost::extents[n][n] );

				value_type cs1 = ctf_params.m_cs * 10000000;
				value_type kv1 = ctf_params.m_kv * 1000;
				value_type wl  = 12.3 / sqrt(kv1 + kv1*kv1/1000000);

				value_type stepr = ctf_params.m_dstep * 10000 / ctf_params.m_mag;
				value_type thetatr = wl / (stepr * n);

				value_type ampcon = 0.07;
				value_type phacon = sqrt( 1 - ampcon*ampcon );

				value_type rad, angle, angspt, c1, c2, angdif, c_cos, df, chi;
				size_type j;
				
				//#pragma omp parallel for private(rad, angle, angspt, c1, c2, angdif, c_cos, df, chi, j)
				for (size_type i=0; i<n; i++)
				{
					for(j=0; j<n; j++)
					{
						rad = sqrt( (i-n/2)*(i-n/2) + (j-n/2)*(j-n/2) );
						angle = rad * thetatr;
						angspt = atan2( j-n/2, i-n/2 );
						c1 = 2 * pi * angle * angle / ( 2.0 * wl );
						c2 = -c1 * cs1 * angle * angle / 2.0;
						angdif = angspt - ctf_params.m_angast * pi / 180;
						c_cos = cos(2*angdif);

						if ( i==n/2 && j==n/2)
						{
							ctf[i][j] = ampcon;
						}
						else
						{
							df = 0.5 * ( difmid1 + difmid2 + c_cos * (difmid1-difmid2) );
							chi = c1 * df + c2;
							ctf[i][j] = sin(chi)*phacon + cos(chi)*ampcon;
						}
					}
				}

				//#pragma omp parallel for private(j)
				for (size_type i=0; i<n; i++)
				{
					for(j=0; j<n; j++)
					{
						if (ctf[i][j] < 0)
						{
							fftdata[i][j] *= -1.0; 
						}
					}
				}
				
				SingleParticle2dx::Utilities::FFTCalculator::performBackwardFFT(&fftdata, data);
			}
			
		protected:
			
			/**
			 *  @brief      Calculates normalize square distance of the value
			 *  @details    Used for calculateRMSD
			 *  @param[in]  i1 First input value
			 *  @param[in]  i2 Second input value
			 *  @param[in]  n Normalization factor
			 *  @return     (i1-i2)^2 / n
			 */
			static value_type calcSquaredDiff(value_type i1, value_type i2, size_type n )
			{
				return (((i1-i2)*(i1-i2))/n);
			}

		};
		
	} /* Utilities */
	
} /* SingleParticle2dx */

#endif /* end of include guard: DATACONTAINERFUNCTIONS_HPP_LY7U6N3I */
