#ifndef FFT_TEST_HPP
#define FFT_TEST_HPP

#include <omp.h>

#include "cppunit/TestFailure.h"
#include "cppunit/TestAssert.h"
#include <cppunit/extensions/HelperMacros.h>

#include "../2dxSingleParticle.hpp"
#include "../Typedefs.hpp"

/**
 *  @brief     Testing of the FFT routines
 *  @author    Sebastian Scherer
 *  @version   0.1
 *  @date      2012
 *  @copyright GNU Public License
 */
class FFTTest: public CppUnit::TestFixture  {

	CPPUNIT_TEST_SUITE( FFTTest );
		CPPUNIT_TEST( test2d );
		CPPUNIT_TEST( test3d );
	CPPUNIT_TEST_SUITE_END();
	
public:
	typedef SingleParticle2dx::value_type value_type;
	
private:
	int SIZE;
	
	boost::multi_array<value_type,2> Rin_2d_1;
	boost::multi_array<value_type,2> Rin_2d_2;
	boost::multi_array<value_type,2> Rout_2d_1;
	boost::multi_array<value_type,2> Rout_2d_2;
	
	boost::multi_array<std::complex<value_type>,2> F_2d_1;
	boost::multi_array<std::complex<value_type>,2> F_2d_2;
	
	boost::multi_array<value_type,3> Rin_3d_1;
	boost::multi_array<value_type,3> Rin_3d_2;
	boost::multi_array<value_type,3> Rout_3d_1;
	boost::multi_array<value_type,3> Rout_3d_2;
	
	boost::multi_array<std::complex<value_type>,3> F_3d_1;
	boost::multi_array<std::complex<value_type>,3> F_3d_2;
	
	boost::multi_array<value_type,3> LRin_3d_1;
	boost::multi_array<value_type,3> LRin_3d_2;
	boost::multi_array<value_type,3> LRout_3d_1;
	boost::multi_array<value_type,3> LRout_3d_2;
	
	boost::multi_array<std::complex<value_type>,3> LF_3d_1;
	boost::multi_array<std::complex<value_type>,3> LF_3d_2;
	
	
public:
	
	/**
	 *  @brief      Set up test case
	 */
	void setUp()
	{
		SIZE = SingleParticle2dx::ConfigContainer::Instance()->getParticleSize();
		
		Rin_2d_1.resize( boost::extents[SIZE][SIZE] );
		Rin_2d_2.resize( boost::extents[SIZE][SIZE] );
		Rout_2d_1.resize( boost::extents[SIZE][SIZE] );
		Rout_2d_2.resize( boost::extents[SIZE][SIZE] );
		Rin_2d_1 = boost::multi_array<value_type,2>(boost::extents[SIZE][SIZE]);
		std::fill (Rin_2d_1.origin(), Rin_2d_1.origin() + Rin_2d_1.num_elements(), 0.0);
		Rin_2d_2 = Rin_2d_1;
		Rout_2d_1 = Rin_2d_1;
		Rout_2d_2 = Rin_2d_1;
		
		F_2d_1.resize( boost::extents[SIZE][SIZE] );
		F_2d_2.resize( boost::extents[SIZE][SIZE] );
		F_2d_1 = boost::multi_array<std::complex<value_type>,2>(boost::extents[SIZE][SIZE]);
		std::fill (F_2d_1.origin(), F_2d_1.origin() + F_2d_1.num_elements(), std::complex<value_type>(0,0));
		F_2d_2 = F_2d_1;
		
		
		Rin_3d_1.resize( boost::extents[SIZE][SIZE][SIZE] );
		Rin_3d_2.resize( boost::extents[SIZE][SIZE][SIZE] );
		Rout_3d_1.resize( boost::extents[SIZE][SIZE][SIZE] );
		Rout_3d_2.resize( boost::extents[SIZE][SIZE][SIZE] );
		Rin_3d_1 = boost::multi_array<value_type,3>(boost::extents[SIZE][SIZE][SIZE]);
		std::fill (Rin_3d_1.origin(), Rin_3d_1.origin() + Rin_3d_1.num_elements(), 0.0);
		Rin_3d_2 = Rin_3d_1;
		Rout_3d_1 = Rin_3d_1;
		Rout_3d_2 = Rin_3d_1;
		
		F_3d_1.resize( boost::extents[SIZE][SIZE][SIZE] );
		F_3d_2.resize( boost::extents[SIZE][SIZE][SIZE] );
		F_3d_1 = boost::multi_array<std::complex<value_type>,3>(boost::extents[SIZE][SIZE][SIZE]);
		std::fill (F_3d_1.origin(), F_3d_1.origin() + F_3d_1.num_elements(), std::complex<value_type>(0,0));
		F_3d_2 = F_3d_1;
		
		
		LRin_3d_1.resize( boost::extents[3*SIZE][3*SIZE][3*SIZE] );
		LRin_3d_2.resize( boost::extents[3*SIZE][3*SIZE][3*SIZE] );
		LRout_3d_1.resize( boost::extents[3*SIZE][3*SIZE][3*SIZE] );
		LRout_3d_2.resize( boost::extents[3*SIZE][3*SIZE][3*SIZE] );
		LRin_3d_1 = boost::multi_array<value_type,3>(boost::extents[3*SIZE][3*SIZE][3*SIZE]);
		std::fill (LRin_3d_1.origin(), LRin_3d_1.origin() + LRin_3d_1.num_elements(), 0.0);
		LRin_3d_2 = LRin_3d_1;
		LRout_3d_1 = LRin_3d_1;
		LRout_3d_2 = LRin_3d_1;
		
		LF_3d_1.resize( boost::extents[3*SIZE][3*SIZE][3*SIZE] );
		LF_3d_2.resize( boost::extents[3*SIZE][3*SIZE][3*SIZE] );
		LF_3d_1 = boost::multi_array<std::complex<value_type>,3>(boost::extents[3*SIZE][3*SIZE][3*SIZE]);
		std::fill (LF_3d_1.origin(), LF_3d_1.origin() + LF_3d_1.num_elements(), std::complex<value_type>(0,0));
		LF_3d_2 = LF_3d_1;
	}
	
	
	/**
	 *  @brief      Tear down test case
	 */
	void tearDown()
	{}
	
	
	/**
	 *  @brief      Test 2d fft
	 */
	void test2d()
	{
		for (int i=0; i<SIZE; i++)
		{
			for (int j=0; j<SIZE; j++)
			{
				Rin_2d_1[i][j] = i+j;
			}
		}
		
		SingleParticle2dx::Utilities::FFTCalculator::performForwardFFT(&Rin_2d_1, &F_2d_1);
		SingleParticle2dx::Utilities::FFTCalculator::performBackwardFFT(&F_2d_1, &Rin_2d_2);
		
		for (int i=0; i<SIZE; i++)
		{
			for (int j=0; j<SIZE; j++)
			{
				CPPUNIT_ASSERT( (fabs(Rin_2d_1[i][j] - Rin_2d_2[i][j]) < 1e-4) );
			}
		}
		
	}
	

	/**
	 *  @brief      Test 3d fft
	 */
	void test3d()
	{
		for (int i=0; i<SIZE; i++)
		{
			for (int j=0; j<SIZE; j++)
			{
				for (int k=0; k<SIZE; k++)
				{
					Rin_3d_1[i][j][k] = i+j+k;
				}
			}
		}
		
		SingleParticle2dx::Utilities::FFTCalculator::performForwardFFT(&Rin_3d_1, &F_3d_1);
		SingleParticle2dx::Utilities::FFTCalculator::performBackwardFFT(&F_3d_1, &Rin_3d_2);
		
		for (int i=0; i<SIZE; i++)
		{
			for (int j=0; j<SIZE; j++)
			{
				for (int k=0; k<SIZE; k++)
				{
					CPPUNIT_ASSERT( (fabs(Rin_3d_1[i][j][k] - Rin_3d_2[i][j][k]) < 1e-3) );
				}
			}
		}
	}
	
};

#endif /* end of include guard: FFT_TEST_HPP */
