#ifndef PARTICLE_TEST_HPP
#define PARTICLE_TEST_HPP

#include "cppunit/TestFailure.h"
#include "cppunit/TestAssert.h"
#include <cppunit/extensions/HelperMacros.h>

#include "../2dxSingleParticle.hpp"
#include "../Typedefs.hpp"



/**
 *  @brief     Testing of the particle class
 *  @author    Sebastian Scherer
 *  @version   0.1
 *  @date      2012
 *  @copyright GNU Public License
 */
class ParticleTest: public CppUnit::TestFixture  {

	CPPUNIT_TEST_SUITE( ParticleTest );
		CPPUNIT_TEST( testConstructor );
		CPPUNIT_TEST( testAssignValue );
		CPPUNIT_TEST( testCopy );
		CPPUNIT_TEST( testInformation );
		CPPUNIT_TEST( testOrientation );
		CPPUNIT_TEST( testShift );
	CPPUNIT_TEST_SUITE_END();

public:
	typedef SingleParticle2dx::value_type value_type;
	
private:
	SingleParticle2dx::DataStructures::Particle p_1, p_2, p_3;
	int test_size;
	
	
public:
	
	/**
	 *  @brief      Set up test case
	 */
	void setUp()
	{
		test_size = 512;
		
		p_1 = SingleParticle2dx::DataStructures::Particle();
		
		SingleParticle2dx::DataStructures::Orientation o;
		SingleParticle2dx::DataStructures::GlobalParticleInformation i;
		
		p_2 = SingleParticle2dx::DataStructures::Particle(test_size, test_size, o, i);
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
		CPPUNIT_ASSERT(p_1.getSizeX() == 1);
		CPPUNIT_ASSERT(p_1.getSizeY() == 1);
		CPPUNIT_ASSERT( (p_1(0,0) == std::complex<value_type>(0,0)) );
		
		CPPUNIT_ASSERT(p_2.getSizeX() == test_size);
		CPPUNIT_ASSERT(p_2.getSizeY() == test_size);
		
		for (int i=0; i<p_2.getSizeX(); i++)
		{
			for (int j=0; j<p_2.getSizeY(); j++)
			{
				CPPUNIT_ASSERT( (p_2(i,j) == std::complex<value_type>(0,0)) );
			}
		}
	}
	
	
	/**
	 *  @brief      Test assignment 
	 */
	void testAssignValue()
	{
		p_2(100,100) = std::complex<value_type>(1,2);
		
		CPPUNIT_ASSERT(p_2(100,100).real() == 1);
		CPPUNIT_ASSERT(p_2(100,100).imag() == 2);
		
		p_2(100,100).real() += 10;
		p_2(100,100).imag() -= 1;
		
		CPPUNIT_ASSERT( (p_2(100,100) == std::complex<value_type>(11,1)) );
	}
	
	
	/**
	 *  @brief      Test copy constructor
	 */
	void testCopy()
	{	
		p_2(100,100) = std::complex<value_type>(100,200);
		CPPUNIT_ASSERT( (p_2(100,100) == std::complex<value_type>(100,200)) );
		
		p_3 = p_2;
		SingleParticle2dx::DataStructures::Particle p_4(p_2);
	
		CPPUNIT_ASSERT( (p_2(100,100) == std::complex<value_type>(100,200)) );
		CPPUNIT_ASSERT( (p_3(100,100) == std::complex<value_type>(100,200)) );
		CPPUNIT_ASSERT( (p_4(100,100) == std::complex<value_type>(100,200)) );
		
		p_3(100,100).real() += 10;
		p_3(100,100).imag() -= 10;
		
		p_4(100,100).real() += 20;
		p_4(100,100).imag() -= 20;
		
		CPPUNIT_ASSERT( (p_2(100,100) == std::complex<value_type>(100,200)) );
		CPPUNIT_ASSERT( (p_3(100,100) == std::complex<value_type>(110,190)) );
		CPPUNIT_ASSERT( (p_4(100,100) == std::complex<value_type>(120,180)) );
		
		
		for (int i=0; i<p_2.getSizeX(); i++)
		{
			for (int j=0; j<p_2.getSizeY(); j++)
			{
				if (!(i==100 && j==100))
				{
					CPPUNIT_ASSERT( (p_2(i,j) == std::complex<value_type>(0,0)) );
					CPPUNIT_ASSERT( (p_3(i,j) == std::complex<value_type>(0,0)) );
					CPPUNIT_ASSERT( (p_4(i,j) == std::complex<value_type>(0,0)) );
				}
			}
		}
	}
	
	
	/**
	 *  @brief      Test info copy
	 */
	void testInformation()
	{
		CPPUNIT_ASSERT( (p_2.getGlobalParticleInformation().getImageNumber() == 0) );
		CPPUNIT_ASSERT( (p_2.getGlobalParticleInformation().getPositionX() == 0) );
		CPPUNIT_ASSERT( (p_2.getGlobalParticleInformation().getPositionY() == 0) );
		
		SingleParticle2dx::DataStructures::GlobalParticleInformation i(1,100,200);
		SingleParticle2dx::DataStructures::Orientation o;
		
		SingleParticle2dx::DataStructures::Particle p_5(100,100,o,i);
		CPPUNIT_ASSERT( (p_5.getGlobalParticleInformation().getImageNumber() == 1) );
		CPPUNIT_ASSERT( (p_5.getGlobalParticleInformation().getPositionX() == 100) );
		CPPUNIT_ASSERT( (p_5.getGlobalParticleInformation().getPositionY() == 200) );
		
		SingleParticle2dx::DataStructures::Particle p_6(p_5);
		CPPUNIT_ASSERT( (p_6.getGlobalParticleInformation().getImageNumber() == 1) );
		CPPUNIT_ASSERT( (p_6.getGlobalParticleInformation().getPositionX() == 100) );
		CPPUNIT_ASSERT( (p_6.getGlobalParticleInformation().getPositionY() == 200) );
		
		p_3 = p_5;
		CPPUNIT_ASSERT( (p_3.getGlobalParticleInformation().getImageNumber() == 1) );
		CPPUNIT_ASSERT( (p_3.getGlobalParticleInformation().getPositionX() == 100) );
		CPPUNIT_ASSERT( (p_3.getGlobalParticleInformation().getPositionY() == 200) );
		
		p_6.getGlobalParticleInformation().setImageNumber(2);
		p_6.getGlobalParticleInformation().setPositionX(300);
		p_6.getGlobalParticleInformation().setPositionY(400);
		
		CPPUNIT_ASSERT( (p_3.getGlobalParticleInformation().getImageNumber() == 1) );
		CPPUNIT_ASSERT( (p_3.getGlobalParticleInformation().getPositionX() == 100) );
		CPPUNIT_ASSERT( (p_3.getGlobalParticleInformation().getPositionY() == 200) );
		CPPUNIT_ASSERT( (p_5.getGlobalParticleInformation().getImageNumber() == 1) );
		CPPUNIT_ASSERT( (p_5.getGlobalParticleInformation().getPositionX() == 100) );
		CPPUNIT_ASSERT( (p_5.getGlobalParticleInformation().getPositionY() == 200) );
		CPPUNIT_ASSERT( (p_6.getGlobalParticleInformation().getImageNumber() == 2) );
		CPPUNIT_ASSERT( (p_6.getGlobalParticleInformation().getPositionX() == 300) );
		CPPUNIT_ASSERT( (p_6.getGlobalParticleInformation().getPositionY() == 400) );
	}
	
	
	/**
	 *  @brief      Test orientation copy
	 */
	void testOrientation()
	{
		CPPUNIT_ASSERT( (p_2.getInitialOrientation().getTLTAXIS() == 0) );
		CPPUNIT_ASSERT( (p_2.getInitialOrientation().getTLTANG() == 0) );
		CPPUNIT_ASSERT( (p_2.getInitialOrientation().getTAXA() == 0) );
		CPPUNIT_ASSERT( (p_2.getNewOrientation().getTLTAXIS() == 0) );
		CPPUNIT_ASSERT( (p_2.getNewOrientation().getTLTANG() == 0) );
		CPPUNIT_ASSERT( (p_2.getNewOrientation().getTAXA() == 0) );
		CPPUNIT_ASSERT( (p_2.getOldOrientation().getTLTAXIS() == 0) );
		CPPUNIT_ASSERT( (p_2.getOldOrientation().getTLTANG() == 0) );
		CPPUNIT_ASSERT( (p_2.getOldOrientation().getTAXA() == 0) );
		
		SingleParticle2dx::DataStructures::GlobalParticleInformation i;
		SingleParticle2dx::DataStructures::Orientation o(1,2,3);
		
		SingleParticle2dx::DataStructures::Particle p_5(100,100,o,i);
		CPPUNIT_ASSERT( abs(p_5.getInitialOrientation().getTLTAXIS()-1) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_5.getInitialOrientation().getTLTANG()-2) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_5.getInitialOrientation().getTAXA()-3) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_5.getNewOrientation().getTLTAXIS()-1) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_5.getNewOrientation().getTLTANG()-2) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_5.getNewOrientation().getTAXA()-3) < 1e-8 );
		CPPUNIT_ASSERT( (p_5.getOldOrientation().getTLTAXIS() == 0) );
		CPPUNIT_ASSERT( (p_5.getOldOrientation().getTLTANG() == 0) );
		CPPUNIT_ASSERT( (p_5.getOldOrientation().getTAXA() == 0) );
		
		SingleParticle2dx::DataStructures::Particle p_6(p_5);
		CPPUNIT_ASSERT( abs(p_6.getInitialOrientation().getTLTAXIS()-1) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_6.getInitialOrientation().getTLTANG()-2) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_6.getInitialOrientation().getTAXA()-3) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_6.getNewOrientation().getTLTAXIS()-1) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_6.getNewOrientation().getTLTANG()-2) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_6.getNewOrientation().getTAXA()-3) < 1e-8 );
		CPPUNIT_ASSERT( (p_6.getOldOrientation().getTLTAXIS() == 0) );
		CPPUNIT_ASSERT( (p_6.getOldOrientation().getTLTANG() == 0) );
		CPPUNIT_ASSERT( (p_6.getOldOrientation().getTAXA() == 0) );
		
		p_3 = p_6;
		CPPUNIT_ASSERT( abs(p_3.getInitialOrientation().getTLTAXIS()-1) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_3.getInitialOrientation().getTLTANG()-2) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_3.getInitialOrientation().getTAXA()-3) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_3.getNewOrientation().getTLTAXIS()-1) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_3.getNewOrientation().getTLTANG()-2) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_3.getNewOrientation().getTAXA()-3) < 1e-8 );
		CPPUNIT_ASSERT( (p_3.getOldOrientation().getTLTAXIS() == 0) );
		CPPUNIT_ASSERT( (p_3.getOldOrientation().getTLTANG() == 0) );
		CPPUNIT_ASSERT( (p_3.getOldOrientation().getTAXA() == 0) );
		
		SingleParticle2dx::DataStructures::Orientation o2(10,20,30);
		p_3.updateOrientation(o2);
		CPPUNIT_ASSERT( abs(p_3.getInitialOrientation().getTLTAXIS()-1) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_3.getInitialOrientation().getTLTANG()-2) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_3.getInitialOrientation().getTAXA()-3) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_3.getNewOrientation().getTLTAXIS()-10) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_3.getNewOrientation().getTLTANG()-20) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_3.getNewOrientation().getTAXA()-30) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_3.getOldOrientation().getTLTAXIS()-1) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_3.getOldOrientation().getTLTANG()-2) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_3.getOldOrientation().getTAXA()-3) < 1e-8 );
		
		o2.setTLTAXIS(100);
		p_3.updateOrientation(o2);
		CPPUNIT_ASSERT( abs(p_3.getInitialOrientation().getTLTAXIS()-1) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_3.getInitialOrientation().getTLTANG()-2) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_3.getInitialOrientation().getTAXA()-3) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_3.getNewOrientation().getTLTAXIS()-100) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_3.getNewOrientation().getTLTANG()-20) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_3.getNewOrientation().getTAXA()-30) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_3.getOldOrientation().getTLTAXIS()-10) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_3.getOldOrientation().getTLTANG()-20) < 1e-8 );
		CPPUNIT_ASSERT( abs(p_3.getOldOrientation().getTAXA()-30) < 1e-8 );
	}
	
	
	/**
	 *  @brief      Test shift copy
	 */
	void testShift()
	{
		SingleParticle2dx::DataStructures::ParticleShift s;
		s.setShiftX(1);
		s.setShiftY(2);
		
		SingleParticle2dx::DataStructures::Particle p_shift_1;
		p_shift_1.setParticleShift(s);
		
		CPPUNIT_ASSERT( p_shift_1.getParticleShift().getShiftX() == 1 );
		CPPUNIT_ASSERT( p_shift_1.getParticleShift().getShiftY() == 2 );
		
		SingleParticle2dx::DataStructures::Particle p_shift_2(p_shift_1);
		CPPUNIT_ASSERT( p_shift_2.getParticleShift().getShiftX() == 1 );
		CPPUNIT_ASSERT( p_shift_2.getParticleShift().getShiftY() == 2 );
		
		s.setShiftY(20);
		p_shift_2.setParticleShift(s);
		
		CPPUNIT_ASSERT( p_shift_1.getParticleShift().getShiftX() == 1 );
		CPPUNIT_ASSERT( p_shift_1.getParticleShift().getShiftY() == 2 );
		CPPUNIT_ASSERT( p_shift_2.getParticleShift().getShiftX() == 1 );
		CPPUNIT_ASSERT( p_shift_2.getParticleShift().getShiftY() == 20 );
		
	}

};

#endif /* end of include guard: PARTICLE_TEST_HPP */
