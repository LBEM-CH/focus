#ifndef PROJECTION2D_TEST_HPP
#define PROJECTION2D_TEST_HPP

#include <omp.h>

#include "cppunit/TestFailure.h"
#include "cppunit/TestAssert.h"
#include <cppunit/extensions/HelperMacros.h>

#include "../2dxSingleParticle.hpp"
#include "../Typedefs.hpp"


/**
 *  @brief     Testing of the projection class
 *  @author    Sebastian Scherer
 *  @version   0.1
 *  @date      2012
 *  @copyright GNU Public License
 */
class Projection2dTest: public CppUnit::TestFixture  {

	CPPUNIT_TEST_SUITE( Projection2dTest );
		CPPUNIT_TEST( testConstructor );
		CPPUNIT_TEST( testAssignValue );
		CPPUNIT_TEST( testCopy );
		CPPUNIT_TEST( testOrientation );
		CPPUNIT_TEST( testFFT );
	CPPUNIT_TEST_SUITE_END();
	
public:
	typedef SingleParticle2dx::value_type value_type;
	
private:
	int test_size;
	int SIZE;
	SingleParticle2dx::DataStructures::Projection2d p1, p2, p3;
	SingleParticle2dx::DataStructures::Projection2d p_small, p_small2;
	boost::multi_array<value_type,2> test_in;
	boost::multi_array<value_type,2> test_out;
	
	boost::multi_array<value_type,2> test_in2;
	boost::multi_array<value_type,2> test_out2;
	
	
public:
	
	/**
	 *  @brief      Set up test case
	 */
	void setUp()
	{
		test_size = SingleParticle2dx::ConfigContainer::Instance()->getParticleSize();
		SIZE = SingleParticle2dx::ConfigContainer::Instance()->getParticleSize();
		
		p1 = SingleParticle2dx::DataStructures::Projection2d();
		p2 = SingleParticle2dx::DataStructures::Projection2d(test_size+100, test_size+200);
		
		SingleParticle2dx::DataStructures::Orientation o(1,2,3);
		p3 = SingleParticle2dx::DataStructures::Projection2d(test_size, test_size, o);
		
		p_small = SingleParticle2dx::DataStructures::Projection2d(SIZE, SIZE, o);
		p_small2 = SingleParticle2dx::DataStructures::Projection2d(SIZE, SIZE, o);
		
		test_in.resize( boost::extents[SIZE][SIZE] );
		test_out.resize( boost::extents[SIZE][SIZE] );
		test_in = boost::multi_array<value_type,2>(boost::extents[SIZE][SIZE]);
		test_out = test_in;
		std::fill (test_in.origin(), test_in.origin() + test_in.num_elements(), 0.0);
		std::fill (test_out.origin(), test_out.origin() + test_out.num_elements(), 0.0);
		
		test_in2.resize( boost::extents[SIZE][SIZE] );
		test_out2.resize( boost::extents[SIZE][SIZE] );
		test_in2 = boost::multi_array<value_type,2>(boost::extents[SIZE][SIZE]);
		test_out2 = test_in;
		std::fill (test_in2.origin(), test_in2.origin() + test_in2.num_elements(), 0.0);
		std::fill (test_out2.origin(), test_out2.origin() + test_out2.num_elements(), 0.0);
	}
	
	
	/**
	 *  @brief      Tear down
	 */
	void tearDown()
	{}
	
	
	/**
	 *  @brief      Test constructor
	 */
	void testConstructor()
	{
		CPPUNIT_ASSERT( (p1.getSizeX() == 1) );
		CPPUNIT_ASSERT( (p1.getSizeY() == 1) );
		CPPUNIT_ASSERT( (p1.getOrientation().getTLTAXIS() == 0 ) );
		CPPUNIT_ASSERT( (p1.getOrientation().getTLTANG() == 0 ) );
		CPPUNIT_ASSERT( (p1.getOrientation().getTAXA() == 0 ) );
		
		CPPUNIT_ASSERT( (p2.getSizeX() == (test_size+100)) );
		CPPUNIT_ASSERT( (p2.getSizeY() == (test_size+200)) );
		CPPUNIT_ASSERT( (p2.getOrientation().getTLTAXIS() == 0 ) );
		CPPUNIT_ASSERT( (p2.getOrientation().getTLTANG() == 0 ) );
		CPPUNIT_ASSERT( (p2.getOrientation().getTAXA() == 0 ) );
		
		CPPUNIT_ASSERT( (p3.getSizeX() == test_size) );
		CPPUNIT_ASSERT( (p3.getSizeY() == test_size) );
		CPPUNIT_ASSERT( abs(p3.getOrientation().getTLTAXIS()-1) < 1e-8 );
		CPPUNIT_ASSERT( abs(p3.getOrientation().getTLTANG()-2) < 1e-8 );
		CPPUNIT_ASSERT( abs(p3.getOrientation().getTAXA()-3) < 1e-8 );	
	}
	
	
	/**
	 *  @brief      Test assignment
	 */
	void testAssignValue()
	{
		p2(100,100) = std::complex<value_type>(1,2);
		
		CPPUNIT_ASSERT(p2(100,100).real() == 1);
		CPPUNIT_ASSERT(p2(100,100).imag() == 2);
		
		p2(100,100).real() += 10;
		p2(100,100).imag() -= 1;
		
		CPPUNIT_ASSERT( (p2(100,100) == std::complex<value_type>(11,1)) );
	}
	
	
	/**
	 *  @brief      Test copy constructor
	 */
	void testCopy()
	{	
		p2(100,100) = std::complex<value_type>(100,200);
		CPPUNIT_ASSERT( (p2(100,100) == std::complex<value_type>(100,200)) );
		
		p3 = p2;
		SingleParticle2dx::DataStructures::Projection2d p4(p2);
	
		CPPUNIT_ASSERT( (p2(100,100) == std::complex<value_type>(100,200)) );
		CPPUNIT_ASSERT( (p3(100,100) == std::complex<value_type>(100,200)) );
		CPPUNIT_ASSERT( (p4(100,100) == std::complex<value_type>(100,200)) );
		
		p3(100,100).real() += 10;
		p3(100,100).imag() -= 10;
		
		p4(100,100).real() += 20;
		p4(100,100).imag() -= 20;
		
		CPPUNIT_ASSERT( (p2(100,100) == std::complex<value_type>(100,200)) );
		CPPUNIT_ASSERT( (p3(100,100) == std::complex<value_type>(110,190)) );
		CPPUNIT_ASSERT( (p4(100,100) == std::complex<value_type>(120,180)) );
		
		
		for (int i=0; i<p2.getSizeX(); i++)
		{
			for (int j=0; j<p2.getSizeY(); j++)
			{
				if (!(i==100 && j==100))
				{
					CPPUNIT_ASSERT( (p2(i,j) == std::complex<value_type>(0,0)) );
					CPPUNIT_ASSERT( (p3(i,j) == std::complex<value_type>(0,0)) );
					CPPUNIT_ASSERT( (p4(i,j) == std::complex<value_type>(0,0)) );
				}
			}
		}
	}
	
	
	/**
	 *  @brief      Test orientation copy
	 */
	void testOrientation()
	{
		CPPUNIT_ASSERT( (p2.getOrientation().getTLTAXIS() == 0) );
		CPPUNIT_ASSERT( (p2.getOrientation().getTLTANG() == 0) );
		CPPUNIT_ASSERT( (p2.getOrientation().getTAXA() == 0) );
		
		SingleParticle2dx::DataStructures::Orientation o(10,20,30);
		
		SingleParticle2dx::DataStructures::Projection2d p5(100,100,o);
		CPPUNIT_ASSERT( abs(p5.getOrientation().getTLTAXIS()-10) < 1e-8 );
		CPPUNIT_ASSERT( abs(p5.getOrientation().getTLTANG()-20) < 1e-8 );
		CPPUNIT_ASSERT( abs(p5.getOrientation().getTAXA()-30) < 1e-8 );
		
		SingleParticle2dx::DataStructures::Projection2d p6(p5);
		CPPUNIT_ASSERT( abs(p6.getOrientation().getTLTAXIS()-10) < 1e-8 );
		CPPUNIT_ASSERT( abs(p6.getOrientation().getTLTANG()-20) < 1e-8 );
		CPPUNIT_ASSERT( abs(p6.getOrientation().getTAXA()-30) < 1e-8 );
		
		p3 = p6;
		CPPUNIT_ASSERT( abs(p3.getOrientation().getTLTAXIS()-10) < 1e-8 );
		CPPUNIT_ASSERT( abs(p3.getOrientation().getTLTANG()-20) < 1e-8 );
		CPPUNIT_ASSERT( abs(p3.getOrientation().getTAXA()-30) < 1e-8 );
		
		o.setTLTAXIS(100);
		p3.setOrientation(o);
		CPPUNIT_ASSERT( abs(p3.getOrientation().getTLTAXIS()-100) < 1e-8 );
		CPPUNIT_ASSERT( abs(p3.getOrientation().getTLTANG()-20) < 1e-8 );
		CPPUNIT_ASSERT( abs(p3.getOrientation().getTAXA()-30) < 1e-8 );
		CPPUNIT_ASSERT( abs(p5.getOrientation().getTLTAXIS()-10) < 1e-8 );
		CPPUNIT_ASSERT( abs(p5.getOrientation().getTLTANG()-20) < 1e-8 );
		CPPUNIT_ASSERT( abs(p5.getOrientation().getTAXA()-30) < 1e-8 );
		CPPUNIT_ASSERT( abs(p6.getOrientation().getTLTAXIS()-10) < 1e-8 );
		CPPUNIT_ASSERT( abs(p6.getOrientation().getTLTANG()-20) < 1e-8 );
		CPPUNIT_ASSERT( abs(p6.getOrientation().getTAXA()-30) < 1e-8 );
	
		}
	
	
	/**
	 *  @brief      Test fft
	 */
	void testFFT()
	{
		for (int i=0; i<SIZE; i++)
		{
			for (int j=0; j<SIZE; j++)
			{
				test_in[i][j] = i+j;
				test_in2[i][j] = fabs((float) rand() / (RAND_MAX));
			}
		}
		
		#pragma omp parallel for
		for (int i=0; i<2; i++)
		{
			if (i==0)
			{
				p_small.setFourierSpaceData(test_in);
				p_small.getRealSpaceData(test_out);
			}
			if (i==1)
			{
				p_small2.setFourierSpaceData(test_in2);
				p_small2.getRealSpaceData(test_out2);
			}
		}
	
		for (int i=0; i<SIZE; i++)
		{
			for (int j=0; j<SIZE; j++)
			{
				//std::cout << test_in[i][j] << "\t" << test_out[i][j] << "\t" << p_small(i,j) << std::endl;
				CPPUNIT_ASSERT( (  fabs(test_in[i][j] - test_out[i][j] ) < 1e-4 ) );
				CPPUNIT_ASSERT( (  fabs(test_in2[i][j] - test_out2[i][j] ) < 1e-4 ) );
			}
		}
	}

};

#endif /* end of include guard: PROJECTION2D_TEST_HPP */
