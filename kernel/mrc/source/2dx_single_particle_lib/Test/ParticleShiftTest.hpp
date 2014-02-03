#ifndef PARTICLE_SHIFT_TEST_HPP
#define PARTICLE_SHIFT_TEST_HPP

#include "cppunit/TestFailure.h"
#include "cppunit/TestAssert.h"
#include <cppunit/extensions/HelperMacros.h>

#include "../2dxSingleParticle.hpp"


/**
 *  @brief     Testing of the particle shift
 *  @author    Sebastian Scherer
 *  @version   0.1
 *  @date      2012
 *  @copyright GNU Public License
 */
class ParticleShiftTest: public CppUnit::TestFixture  {

CPPUNIT_TEST_SUITE( ParticleShiftTest );
	CPPUNIT_TEST( testConstructor );
	CPPUNIT_TEST( testAssignment );
	CPPUNIT_TEST( testDeepCopy );
	CPPUNIT_TEST( testCopyConstructor );
CPPUNIT_TEST_SUITE_END();
	
	
private:
	SingleParticle2dx::DataStructures::ParticleShift m_o1, m_o2;
	
public:
	
	/**
	 *  @brief      Set up test case
	 */
	void setUp()
	{
		m_o1 = SingleParticle2dx::DataStructures::ParticleShift();
		m_o2 = SingleParticle2dx::DataStructures::ParticleShift(1, 2);
	}
	
	
	/**
	 *  @brief      Tear down
	 */
	void tearDown()
	{}
	
	
	/**
	 *  @brief      Test copy constructor
	 */
	void testConstructor()
	{
		CPPUNIT_ASSERT( m_o1.getShiftX() == 0 );
		CPPUNIT_ASSERT( m_o1.getShiftY() == 0 );

		CPPUNIT_ASSERT( m_o2.getShiftX() == 1 );
		CPPUNIT_ASSERT( m_o2.getShiftY() == 2 );
		
		std::pair<int, int> p(10,11);
		SingleParticle2dx::DataStructures::ParticleShift aux(p);
		
		CPPUNIT_ASSERT( aux.getShiftX() == 10 );
		CPPUNIT_ASSERT( aux.getShiftY() == 11 );
		
		p.first += 100;
		p.second += 100;
		
		CPPUNIT_ASSERT( p.first == 110 );
		CPPUNIT_ASSERT( p.second == 111 );
		CPPUNIT_ASSERT( aux.getShiftX() == 10 );
		CPPUNIT_ASSERT( aux.getShiftY() == 11 );
		
		aux.setShiftX(0);
		aux.setShiftY(1);
		CPPUNIT_ASSERT( p.first == 110 );
		CPPUNIT_ASSERT( p.second == 111 );
		CPPUNIT_ASSERT( aux.getShiftX() == 0 );
		CPPUNIT_ASSERT( aux.getShiftY() == 1 );
		
	}


	/**
	 *  @brief      Test assignment operator
	 */
	void testAssignment()
	{
		m_o1 = m_o2;

		CPPUNIT_ASSERT( m_o1.getShiftX() == 1 );
		CPPUNIT_ASSERT( m_o1.getShiftY() == 2 );
	}


	/**
	 *  @brief      Test deep copy
	 */
	void testDeepCopy()
	{
		m_o1.setShiftX(3);
		m_o1.setShiftY(4);
		
		CPPUNIT_ASSERT( m_o1.getShiftX() == 3 );
		CPPUNIT_ASSERT( m_o2.getShiftX() == 1 );

		CPPUNIT_ASSERT( m_o1.getShiftY() == 4 );
		CPPUNIT_ASSERT( m_o2.getShiftY() == 2 );
	}


	/**
	 *  @brief      Test copy constructor
	 */
	void testCopyConstructor()
	{
		SingleParticle2dx::DataStructures::ParticleShift aux(m_o2);
		
		CPPUNIT_ASSERT( aux.getShiftX() == 1 );
		CPPUNIT_ASSERT( aux.getShiftY() == 2 );

		aux.setShiftX(100);
		CPPUNIT_ASSERT( aux.getShiftX() == 100);
		CPPUNIT_ASSERT( m_o2.getShiftX() == 1 );
	}
};

#endif /* end of include guard: PARTICLE_SHIFT_TEST_HPP */



