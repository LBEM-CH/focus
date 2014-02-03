#ifndef GLOBAL_PARTICLE_INFORMATION_TEST_HPP
#define GLOBAL_PARTICLE_INFORMATION_TEST_HPP

#include "cppunit/TestFailure.h"
#include "cppunit/TestAssert.h"
#include <cppunit/extensions/HelperMacros.h>

#include "../2dxSingleParticle.hpp"


/**
 *  @brief     Testing of the global particle information
 *  @author    Sebastian Scherer
 *  @version   0.1
 *  @date      2012
 *  @copyright GNU Public License
 */
class GlobalParticleInformationTest : public CppUnit::TestFixture  {

CPPUNIT_TEST_SUITE( GlobalParticleInformationTest );
	CPPUNIT_TEST( testConstructor );
	CPPUNIT_TEST( testAssignment );
	CPPUNIT_TEST( testDeepCopy );
	CPPUNIT_TEST( testCopyConstructor );
CPPUNIT_TEST_SUITE_END();
	
	
private:
	SingleParticle2dx::DataStructures::GlobalParticleInformation m_o1, m_o2;
	
public:
	
	/**
	 *  @brief      Set up test case
	 */
	void setUp()
	{
		m_o1 = SingleParticle2dx::DataStructures::GlobalParticleInformation();
		m_o2 = SingleParticle2dx::DataStructures::GlobalParticleInformation(1, 10, 11);
	}
	
	
	/**
	 *  @brief      Tear down test case
	 */
	void tearDown()
	{}
	
	
	/**
	 *  @brief      Test the constructor
	 */
	void testConstructor()
	{
		CPPUNIT_ASSERT( m_o1.getImageNumber() == 0 );
		CPPUNIT_ASSERT( m_o1.getPositionX() == 0 );
		CPPUNIT_ASSERT( m_o1.getPositionY() == 0 );

		CPPUNIT_ASSERT( m_o2.getImageNumber() == 1 );
		CPPUNIT_ASSERT( m_o2.getPositionX() == 10 );
		CPPUNIT_ASSERT( m_o2.getPositionY() == 11 );
	}

	
	/**
	 *  @brief      Test the assignment operator
	 */
	void testAssignment()
	{
		m_o1 = m_o2;

		CPPUNIT_ASSERT( m_o1.getImageNumber() == 1 );
		CPPUNIT_ASSERT( m_o1.getPositionX() == 10 );
		CPPUNIT_ASSERT( m_o1.getPositionY() == 11 );
	}

	
	/**
	 *  @brief      Test deep copy
	 */
	void testDeepCopy()
	{
		m_o1.setImageNumber(2);
		m_o1.setPositionX(21);
		m_o1.setPositionY(22);
		
		CPPUNIT_ASSERT( m_o1.getImageNumber() == 2 );
		CPPUNIT_ASSERT( m_o2.getImageNumber() == 1 );

		CPPUNIT_ASSERT( m_o1.getPositionX() == 21 );
		CPPUNIT_ASSERT( m_o2.getPositionX() == 10 );

		CPPUNIT_ASSERT( m_o1.getPositionY() == 22 );
		CPPUNIT_ASSERT( m_o2.getPositionY() == 11 );
	}


	/**
	 *  @brief      Test copy constructor
	 */
	void testCopyConstructor()
	{
		SingleParticle2dx::DataStructures::GlobalParticleInformation aux(m_o2);
		
		CPPUNIT_ASSERT( aux.getImageNumber() == 1 );
		CPPUNIT_ASSERT( aux.getPositionX() == 10 );
		CPPUNIT_ASSERT( aux.getPositionY() == 11 );

		aux.setPositionX(100);
		CPPUNIT_ASSERT( aux.getPositionX() == 100 );
		CPPUNIT_ASSERT( m_o2.getPositionX() == 10 );
	}
	
};

#endif /* end of include guard: GLOBAL_PARTICLE_INFORMATION_TEST_HPP */
