#ifndef ABSTRACT2DDATA_HPP_P8VIGAQZ
#define ABSTRACT2DDATA_HPP_P8VIGAQZ


namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class Abstract2dData;
	}
}

namespace SingleParticle2dx
{
	namespace Methods
	{
		class Dummy2dMaskingMethod;
		class Cos2dMaskingMethod;
		class Abstract2dMaskingMethod;
	}
}


#include <boost/scoped_ptr.hpp>
#include <boost/serialization/scoped_ptr.hpp>

#include "../Typedefs.hpp"
#include "../Utilities.hpp"
#include "../Methods.hpp"


namespace SingleParticle2dx
{
	namespace DataStructures
	{
		
		/**
		 *  @brief     Base class for 2d object
		 *  @details   This class implements some basic 2d functionality
		 *  @author    Sebastian Scherer
		 *  @version   0.2
		 *  @date      2012
		 *  @copyright GNU Public License
		 */
		class Abstract2dData
		{

		public:

			/** Type storing the size of the particle image */
			typedef SingleParticle2dx::size_type size_type;


			/** Value type of the stored image information in Fourier space */
			typedef SingleParticle2dx::value_type value_type;


			/** Type of the Fourier transformed value representing real and imaginary part */
			typedef SingleParticle2dx::fft_type fft_type;


			/** Array type of the whole Fourier transformed image */
			typedef SingleParticle2dx::fft_array2d_type fft_array_type;
			
			
			/** Array type of the whole reak space image */
			typedef SingleParticle2dx::real_array2d_type real_array_type;
			
			
			/** 
			 *  @brief      Get real space data
			 *  @details    Perform BW-FFT and stores the result at the passed loation
			 *  @param[out] rhs pointer to the place where to copy the realspace representation
			 */
			void getRealSpaceData (real_array_type* rhs)
			{
			//	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
			//	if(config->getDoCudaFFT())
			//	{
			//		SingleParticle2dx::Utilities::cuFFTCalculator::performBackwardFFT(m_fourier_data.get(), rhs);
			//	}
			//	else
			//	{
					SingleParticle2dx::Utilities::FFTCalculator::performBackwardFFT(m_fourier_data.get(), rhs);
			//	}
			}
			
			
			/** 
			 *  @brief      Set the Fourier space data to 0+0i
			 *  @details    
			 */
			void resetData()
			{
				SingleParticle2dx::Utilities::DataContainerFunctions::resetData(m_fourier_data.get());
			}
			
			
			/** 
			 *  @brief      Set Fourier space data
			 *  @details    Performs a FW-FFT and stores the result internally
			 *  @param[in]  Real space representation to be stored
			 */
			void setFourierSpaceData (real_array_type* rhs)
			{	
			//	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
			//	if(config->getDoCudaFFT())
			//	{
			//		SingleParticle2dx::Utilities::cuFFTCalculator::performForwardFFT(rhs, m_fourier_data.get());
			//	}
			//	else
			//	{
					SingleParticle2dx::Utilities::FFTCalculator::performForwardFFT(rhs, m_fourier_data.get());
			//	}
				
			}
			
			
			void applyWeight ( value_type w )
			{
				if (w > 0.00001)
				{
					SingleParticle2dx::Utilities::DataContainerFunctions::applyWeight(m_fourier_data.get(), w);
				}
			}
			
			
			/** 
			 *  @brief      Write interally stored data to file
			 *  @details    Performs a BW-FFT and stores the result to a .mrc-file
			 *              Result is normalized. Don't forget to pass the .mrc-ending
			 *  @param[in]  filename Where to store the real space data
			 */
			void writeToFile (std::string filename)
			{
				std::cout << "Storing 2d data to file" << std::endl;
				real_array_type rdata( boost::extents[getSizeX()][getSizeY()] );
				getRealSpaceData(&rdata);
				
				if(SingleParticle2dx::Utilities::DataContainerFunctions::checkData(&rdata))
				{
					std::cerr << "bad 2d structure to store" << std::endl;
					throw std::runtime_error("Bad operation");
				}
				
				SingleParticle2dx::Utilities::MRCFileIO::writeToMrc(&rdata, filename);
			}
			
			
			/**
			 *  @brief      Returns the size in x-direction
			 *  @details    Trivial get-Operation
			 *  @return     Size in first direction	
			 */
			size_type getSizeX() const
			{
				return m_fourier_data.get()->shape()[0];
			}


			/**
			 *  @brief      Returns the size in y-direction
			 *  @details    Trivial get-Operation
			 *  @return     Size in second direction	
			 */
			size_type getSizeY() const
			{
				return m_fourier_data.get()->shape()[1];
			}

			
			/**
			 *  @brief      Applies the 2d mask
			 *  @details    Strategy stored in m_2dmasking_strategy (strategy pattern)
			 */
			void applyMask()
			{
				m_2dmasking_strategy.get()->apply2dMask();
			}


			/**
			 *  @brief      Data normalixed in realspace
			 *  @details    mean=0 and sd=1 after calling this function
			 */
			void normalizeRealSpace ()
			{
				real_array_type rdata( boost::extents[getSizeX()][getSizeY()] );
				Abstract2dData::getRealSpaceData(&rdata);
				SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&rdata);
				Abstract2dData::setFourierSpaceData(&rdata);
			}
			
			
			void invertContrast ()
			{
				real_array_type rdata( boost::extents[getSizeX()][getSizeY()] );
				Abstract2dData::getRealSpaceData(&rdata);
				SingleParticle2dx::Utilities::DataContainerFunctions::invertData(&rdata);
				Abstract2dData::setFourierSpaceData(&rdata);
			}
			
			
			void addGaussianNoise(value_type sd)
			{
				real_array_type rdata( boost::extents[getSizeX()][getSizeY()] );
				Abstract2dData::getRealSpaceData(&rdata);
				SingleParticle2dx::Utilities::DataContainerFunctions::addGaussianNoise(&rdata, sd);
				SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&rdata);
				Abstract2dData::setFourierSpaceData(&rdata);
			}
			
			
			void equalizeHistogram ()
			{
				real_array_type rdata( boost::extents[getSizeX()][getSizeY()] );
				Abstract2dData::getRealSpaceData(&rdata);
				SingleParticle2dx::Utilities::DataContainerFunctions::equalizeHistogram(&rdata);
				Abstract2dData::setFourierSpaceData(&rdata);
			}
			
			
			/**
			 *  @brief      Apply a 2d lowpass filter
			 *  @details    mean=0 and sd=1 after calling this function
			 */
			void applyLowPassFilter()
			{
				//std::cout << "apply LP on 2D-data r=" << m_lowpass_radius << " , sigma=" << m_lowpass_relax_param << std::endl;
				SingleParticle2dx::Utilities::DataContainerFunctions::applyLowPassFilter(m_fourier_data.get(), m_lowpass_radius, m_lowpass_relax_param);
			}
			
			
			/**
			 *  @brief      Apply a 2d lowpass filter
			 *  @details    mean=0 and sd=1 after calling this function
			 */
			void applyHighPassFilter()
			{
				//std::cout << "apply HP on 2D-data r=" << m_highpass_radius << " , sigma=" << m_highpass_relax_param << std::endl;
				SingleParticle2dx::Utilities::DataContainerFunctions::applyHighPassFilter(m_fourier_data.get(), m_highpass_radius, m_highpass_relax_param);
			}
			
			
			/**
			 *  @brief      random access operator to the fourier compontents
			 *  @details
			 *  @param[in]  i index in x direction
			 *  @param[in]  j index in y direction
			 *  @return     Fourier data at (i,j)
			 */
			fft_type& operator() (const size_type i, const size_type j)
			{
				return (*m_fourier_data.get())[i][j];
			}

			
			void setLPFilterParams(value_type radius, value_type relax_param)
			{
				m_lowpass_radius = radius;
				m_lowpass_relax_param = relax_param;
			}
			
			
			void setMidddleTarget()
			{
				size_type n = getSizeX();
				real_array_type rdata( boost::extents[getSizeX()][getSizeY()] );
				
				normalizeRealSpace();
				getRealSpaceData(&rdata);
				
				for(size_type i=0; i<n; i++)
				{
					for(size_type j=0; j<n; j++)
					{
						if( (i==n/2) || (j==n/2) || ((i-1)==n/2) || ((j-1)==n/2) )
						{
							rdata[i][j] += 2.5;
						}
					}
				}
				normalizeRealSpace();
				setFourierSpaceData(&rdata);
			}
			
			virtual void setupFromConfig() = 0;
						
		protected:
			
			/**
			 *  @brief      Protected contructor
			 *  @details    Make sure, that nobody create an object of this class
			 *              As we don't have pure virtual functions this is required
			 */
			Abstract2dData () {}
						
			/** @brief Fourier transform of the underlying image */
			boost::scoped_ptr<fft_array_type> m_fourier_data;
			
			/** @brief Strategy used for 2d masking */
			boost::scoped_ptr<SingleParticle2dx::Methods::Abstract2dMaskingMethod> m_2dmasking_strategy;
			
			/** low-pass filter radius */
			size_type m_lowpass_radius;
			
			/** low-pass filter relaxation parameter */
			value_type m_lowpass_relax_param;
			
			/** high-pass filter radius */
			size_type m_highpass_radius;
			
			/** high-pass filter damping parameter */
			value_type m_highpass_relax_param;
			
		private:
			
			friend class boost::serialization::access;
			   
			template<class Archive>
			void serialize(Archive & ar, const unsigned int version)
			{
				ar & m_fourier_data;
				ar & m_lowpass_radius;
				ar & m_lowpass_relax_param;
			}
			
		};
	}
}

#endif /* end of include guard: ABSTRACT2DDATA_HPP_P8VIGAQZ */
