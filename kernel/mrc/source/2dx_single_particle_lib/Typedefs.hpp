#ifndef SINGLE_PARTICLE_2DX_TYPEDEFS_HPP
#define SINGLE_PARTICLE_2DX_TYPEDEFS_HPP

#include <string>
#include <complex>

#include "boost/multi_array.hpp"
#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>


namespace SingleParticle2dx
{
	/** Type used to store any kind of size information  */
	typedef int size_type;
	
	
	/** Type used to store data such as realspace and Fourier space data */
	typedef float value_type;
	
	
	/** Type to used to store complex numbers */
	typedef std::complex<value_type> fft_type;
	
	
	/** Type of a real 2d array */
	typedef boost::multi_array<value_type,2> real_array2d_type;
	
	
	/** Type of a imaginary 2d array */
	typedef boost::multi_array<fft_type,2> fft_array2d_type;
	
	
	/** Type of a real 3d array */
	typedef boost::multi_array<value_type,3> real_array3d_type;
	
	
	/** Type of a imaginary 3d array */
	typedef boost::multi_array<fft_type,3> fft_array3d_type;
	
	/** Type of mrc int */
	typedef int mrc_int;
	
	/** Type of mrc float */
	typedef float mrc_float;
	
	/** Type of mrc char */
	typedef char mrc_char;
	
	typedef boost::archive::binary_iarchive archive_in_type;
	typedef boost::archive::binary_oarchive archive_out_type;
	
} /* SingleParticle2dx */

#endif /* end of include guard: SINGLE_PARTICLE_2DX_TYPEDEFS_HPP */
