#ifndef ABSTRACT3DDATA_HPP_QE98Z6SZ
#define ABSTRACT3DDATA_HPP_QE98Z6SZ

namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class Abstract3dData;
	}
}


#include <boost/scoped_ptr.hpp>

#include "../Typedefs.hpp"
#include "../Utilities.hpp"
#include "../Methods.hpp"

namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class Abstract3dData
		{
			
		public:
			
			/** Type storing a size */
			typedef SingleParticle2dx::size_type size_type;
			
			
			/** Type storing a value */
			typedef SingleParticle2dx::value_type value_type;
			
			
			/** Type storing a complex number */
			typedef SingleParticle2dx::fft_type fft_type;

			
			/** Type storing a 2d array of complex numbers */
			typedef SingleParticle2dx::fft_array2d_type fft_array2d_type;
			
			
			/** Type storing a 2d array of real numbers */
			typedef SingleParticle2dx::real_array2d_type real_array2d_type;

			
			/** Type storing a 3d array of complex numbers */
			typedef SingleParticle2dx::fft_array3d_type fft_array3d_type;

			
			/** Type storing a 3d array of real numbers */
			typedef SingleParticle2dx::real_array3d_type real_array3d_type;

			
			/** Type storing a boost::multi_array index*/
			typedef fft_array3d_type::index_range range;
			
			
			/**
			 *  @brief      Apply 3d mask to the volume
			 *  @details    
			 */
			void applyMask()
			{
				m_3dmasking_strategy.get()->apply3dMask();
			}
			
			
			/**
			 *  @brief      Stores the 3d volume to a 3d mrc file
			 *  @details
			 *  @param[in]  filename Name of the file where to store the volume 
			 */
			void writeToFile (std::string filename)
			{
				real_array3d_type rdata( boost::extents[getSizeX()][getSizeY()][getSizeZ()] );
				getRealSpaceData(rdata);
				SingleParticle2dx::Utilities::MRCFileIO::writeToMrc( &rdata, filename);
			}
			
			
			void readFromFile (std::string filename)
			{
				real_array3d_type rdata( boost::extents[getSizeX()][getSizeY()][getSizeZ()] );
				SingleParticle2dx::Utilities::MRCFileIO::readFromMrc(filename, &rdata);
				setFourierSpaceData(rdata);
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
				return m_fourier_data.get()->shape()[0];
			}


			/**
			 *  @brief      Returns the size in z-direction
			 *  @details    Trivial get-Operation
			 *  @return     Size in third direction	
			 */
			size_type getSizeZ() const
			{
				return m_fourier_data.get()->shape()[0];
			}
			
			
			/**
			 *  @brief      Calculates the real space data
			 *  @details    Backtransforms the fourier space data to real space and stores the result in
			 *              the passed array
			 *  @param[out] rhs reference to the array where to put the realspace data		
			 */
			void getRealSpaceData (real_array3d_type& rhs)
			{
				SingleParticle2dx::Utilities::FFTCalculator::performBackwardFFT(m_fourier_data.get(), &rhs);
			}
			
			
			/**
			 *  @brief      Returns the size in z-direction
			 *  @details    Trivial get-Operation
			 *  @return     Size in third direction	
			 */
			void setFourierSpaceData (real_array3d_type& data)
			{
				SingleParticle2dx::Utilities::FFTCalculator::performForwardFFT (&data, m_fourier_data.get());
			}
			
			
			/**
			 *  @brief      Resets whole Fourier data array to zero
			 *  @details    Fast implementation by means of std::fill
			 *  @post       Fourier data array filled with fft_type(0,0)
			 */
			void resetData()
			{
				SingleParticle2dx::Utilities::DataContainerFunctions::resetData(m_fourier_data.get());
			}
			
			
			/**
			 *  @brief      Applyt lowpass filter to the volume
			 *  @details
			 */
			void applyLowPassFilter()
			{
				std::cout << "::apply LP on 3D-data r=" << m_lowpass_radius << " , sigma=" << m_lowpass_relax_param << std::endl;
				SingleParticle2dx::Utilities::DataContainerFunctions::applyLowPassFilter( m_fourier_data.get(), m_lowpass_radius, m_lowpass_relax_param);
			}
			
			
			void setLPFilterParams(value_type radius, value_type relax_param)
			{
				m_lowpass_radius = radius;
				m_lowpass_relax_param = relax_param;
			}
			
			
			void scale(value_type factor)
			{
				real_array3d_type rdata( boost::extents[this->getSizeX()][this->getSizeY()][this->getSizeZ()] );
				this->getRealSpaceData(rdata);
		
				float* float_data_3d = (float*) malloc(this->getSizeX() * this->getSizeY() * this->getSizeZ() * sizeof(float));
				std::copy(rdata.origin(), rdata.origin()+rdata.num_elements(), float_data_3d );
	
				EMAN::EMData* m_3dmodel = new EMAN::EMData;
	
				m_3dmodel->set_data(float_data_3d, this->getSizeX(), this->getSizeY(), this->getSizeZ());
				m_3dmodel->scale(factor);
				
				std::copy(m_3dmodel->get_data(), m_3dmodel->get_data()+(this->getSizeX() * this->getSizeY() * this->getSizeZ()), rdata.origin() );
	
				SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&rdata);
				this->setFourierSpaceData(rdata);
			}
			
			
			/**
			 *  @brief      random access operator to the fourier compontents
			 *  @details
			 *  @param[in]  i index in x direction
			 *  @param[in]  j index in y direction
			 *  @param[in]  k index in z direction
			 *  @return     Fourier data at (i,j,k)
			 */
			fft_type& operator() (const size_type i, const size_type j, const size_type k)
			{
				return (*m_fourier_data.get())[i][j][k];
			}

		protected:
			
			/**
			 *  @brief      Protected contructor
			 *  @details    Make sure, that nobody create an object of this class
			 *              As we don't have pure virtual functions this is required
			 */
			Abstract3dData () {};
			
			
			/** 3d data of the reconstruction */
			boost::scoped_ptr<fft_array3d_type> m_fourier_data;
			
			
			/** 3d masking strategy */
			boost::scoped_ptr<SingleParticle2dx::Methods::Abstract3dMaskingMethod> m_3dmasking_strategy;
			
			/** low pass filter radius */
			size_type m_lowpass_radius;
			
			/** low pass filter damping parameter */
			value_type m_lowpass_relax_param;
		
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
	} /* DataStructures */
	
} /* SingleParticle2dx */


#endif /* end of include guard: ABSTRACT3DDATA_HPP_QE98Z6SZ */
