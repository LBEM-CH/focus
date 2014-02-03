#ifndef UTILITY_TEST_HPP
#define UTILITY_TEST_HPP


#include "cppunit/TestFailure.h"
#include "cppunit/TestAssert.h"
#include <cppunit/extensions/HelperMacros.h>

#include "../2dxSingleParticle.hpp"


class UtilityTest: public CppUnit::TestFixture  {

	CPPUNIT_TEST_SUITE( UtilityTest );
		CPPUNIT_TEST( testMeanValue );
		CPPUNIT_TEST( testResetFourierComp );
		CPPUNIT_TEST( testNormalize );
		CPPUNIT_TEST( testRMSD );
	CPPUNIT_TEST_SUITE_END();
	
private:
	int size;
	boost::multi_array<double,2> test_array_2d;
	boost::multi_array<double,3> test_array_3d;
	
	boost::multi_array<double,2> norm_array_2d;
	boost::multi_array<double,3> norm_array_3d;
	
	boost::multi_array<std::complex<double>,2> fft_array_2d;
	boost::multi_array<std::complex<double>,3> fft_array_3d;
	
	
	
public:
	void setUp()
	{
		size = 6;
		test_array_2d.resize( boost::extents[size][size] );
		test_array_3d.resize( boost::extents[size][size][size] );
		
		std::fill(test_array_2d.origin(), test_array_2d.origin() + test_array_2d.num_elements(), 2.);
		std::fill(test_array_3d.origin(), test_array_3d.origin() + test_array_3d.num_elements(), 3.);
		
		fft_array_2d.resize( boost::extents[size][size] );
		std::fill(fft_array_2d.origin(), fft_array_2d.origin() + fft_array_2d.num_elements(), std::complex<double>(1,2));
		
		fft_array_3d.resize( boost::extents[size][size][size] );
		std::fill(fft_array_3d.origin(), fft_array_3d.origin() + fft_array_3d.num_elements(), std::complex<double>(4,5));
		
		norm_array_2d.resize( boost::extents[size][size] );
		norm_array_3d.resize( boost::extents[size][size][size] );
		
		for (int i=0; i<size; i++)
		{
			for(int j=0; j<size; j++)
			{
				norm_array_2d[i][j] = i+j;
			}
		}
		
		for (int i=0; i<size; i++)
		{
			for(int j=0; j<size; j++)
			{
				for (int k=0; k<size; k++)
				{
					norm_array_3d[i][j][k] = i+j+k;
				}
			}
		}
	}
	
	void tearDown()
	{}
	
	void testResetFourierComp()
	{
		for (int i=0; i<size; i++)
		{
			for(int j=0; j<size; j++)
			{
				CPPUNIT_ASSERT(fft_array_2d[i][j] == std::complex<double>(1,2));
			}
		}
		
		SingleParticle2dx::Utilities::DataContainerFunctions::resetFourierData(&fft_array_2d);
		
		for (int i=0; i<size; i++)
		{
			for(int j=0; j<size; j++)
			{
				CPPUNIT_ASSERT(fft_array_2d[i][j] == std::complex<double>(0,0));
			}
		}
		
		for (int i=0; i<size; i++)
		{
			for(int j=0; j<size; j++)
			{
				for (int k=0; k<size; k++)
				{
					CPPUNIT_ASSERT(fft_array_3d[i][j][k] == std::complex<double>(4,5));
				}
			}
		}
		
		SingleParticle2dx::Utilities::DataContainerFunctions::resetFourierData(&fft_array_3d);
		
		for (int i=0; i<size; i++)
		{
			for(int j=0; j<size; j++)
			{
				for (int k=0; k<size; k++)
				{
					CPPUNIT_ASSERT(fft_array_3d[i][j][k] == std::complex<double>(0,0));
				}
			}
		}
	}
	
	void testMeanValue()
	{
		double test_2 = SingleParticle2dx::Utilities::DataContainerFunctions::calculateMeanValue(&test_array_2d);
		CPPUNIT_ASSERT( (test_2-2) < 1e-4 );
		
		double test_3 = SingleParticle2dx::Utilities::DataContainerFunctions::calculateMeanValue(&test_array_3d);
		CPPUNIT_ASSERT( (test_3-3) < 1e-4 );
	}
	
	void testNormalize()
	{
		CPPUNIT_ASSERT( SingleParticle2dx::Utilities::DataContainerFunctions::calculateMeanValue(&norm_array_2d) > 1e-5 );
		SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&norm_array_2d);
		
		double tmp;
		double mean = 0;
		double mean2 = 0;
		int n_2d = size*size;
		
		for (int i=0; i<size; i++)
		{
			for(int j=0; j<size; j++)
			{
				tmp = norm_array_2d[i][j];
				mean += tmp / n_2d;
				mean2 += (tmp*tmp) / n_2d;
			}
		}
		
		double sd = sqrt(mean2 - mean*mean);
		CPPUNIT_ASSERT( mean < 1e-4 );
		CPPUNIT_ASSERT( fabs(sd - 1) < 1e-4 );
		
		//TEST 3D
		
		CPPUNIT_ASSERT( SingleParticle2dx::Utilities::DataContainerFunctions::calculateMeanValue(&norm_array_3d) > 1e-5 );
		SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&norm_array_3d);
		
		mean = 0;
		mean2 = 0;
		int n_3d = size*size*size;
		
		for (int i=0; i<size; i++)
		{
			for(int j=0; j<size; j++)
			{
				for (int k=0; k<size; k++)
				{
					tmp = norm_array_3d[i][j][k];
					mean += tmp / n_3d;
					mean2 += (tmp*tmp) / n_3d;
				}
				
			}
		}
		
		sd = sqrt(mean2 - mean*mean);
		CPPUNIT_ASSERT( mean < 1e-4 );
		CPPUNIT_ASSERT( fabs(sd - 1) < 1e-4 );
		
	}
	
	void testRMSD()
	{
		int rmsd_size = 10;
		boost::multi_array<double,2> rmsd_2d ( boost::extents[rmsd_size][rmsd_size] );
		boost::multi_array<double,3> rmsd_3d ( boost::extents[rmsd_size][rmsd_size][rmsd_size] );
		
		for (int i=0; i<rmsd_size; i++)
		{
			for (int j=0; j<rmsd_size; j++)
			{
				rmsd_2d[i][j] = i+j;
				for (int k=0; k<rmsd_size; k++)
				{
					rmsd_3d[i][j][k] = i+j+k;
				}
			}
		}
		
		double rmsd_value_2d = SingleParticle2dx::Utilities::DataContainerFunctions::calculateRMSD(&rmsd_2d);
		double rmsd_value_3d = SingleParticle2dx::Utilities::DataContainerFunctions::calculateRMSD(&rmsd_3d);
		
		double mean_2d = SingleParticle2dx::Utilities::DataContainerFunctions::calculateMeanValue(&rmsd_2d);
		double mean_3d = SingleParticle2dx::Utilities::DataContainerFunctions::calculateMeanValue(&rmsd_3d);
		
		double rmsd_value_2d_hand = 0;
		double rmsd_value_3d_hand = 0;
		
		for (int i=0; i<rmsd_size; i++)
		{
			for (int j=0; j<rmsd_size; j++)
			{
				rmsd_value_2d_hand += (rmsd_2d[i][j]-mean_2d)*(rmsd_2d[i][j]-mean_2d);
				for (int k=0; k<rmsd_size; k++)
				{
					rmsd_value_3d_hand += (rmsd_3d[i][j][k]-mean_3d)*(rmsd_3d[i][j][k]-mean_3d);
				}
			}
		}
		
		rmsd_value_2d_hand /= (rmsd_size*rmsd_size);
		rmsd_value_3d_hand /= (rmsd_size*rmsd_size*rmsd_size);
		
		rmsd_value_2d_hand = sqrt(rmsd_value_2d_hand);
		rmsd_value_3d_hand = sqrt(rmsd_value_3d_hand);
		
		CPPUNIT_ASSERT( (rmsd_value_2d_hand-rmsd_value_2d) < 1e-4 );
		CPPUNIT_ASSERT( (rmsd_value_3d_hand-rmsd_value_3d) < 1e-4 );	
	}
	
};

#endif /* end of include guard: UTILITY_TEST_HPP */
