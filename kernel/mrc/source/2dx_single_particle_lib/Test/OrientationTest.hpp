#ifndef ORIENTATIONTEST_HPP_3TQJM02S
#define ORIENTATIONTEST_HPP_3TQJM02S

#include "cppunit/TestFailure.h"
#include "cppunit/TestAssert.h"
#include <cppunit/extensions/HelperMacros.h>

#include "../2dxSingleParticle.hpp"


/**
 *  @brief     Testing of the orientation
 *  @author    Sebastian Scherer
 *  @version   0.1
 *  @date      2012
 *  @copyright GNU Public License
 */
class OrientationTest : public CppUnit::TestFixture  {

CPPUNIT_TEST_SUITE( OrientationTest );
	CPPUNIT_TEST( testConstructor );
	CPPUNIT_TEST( testAssignment );
	CPPUNIT_TEST( testDeepCopy );
	CPPUNIT_TEST( testCopyConstructor );
	CPPUNIT_TEST( testCalcNormal );
	CPPUNIT_TEST( testNorm );
CPPUNIT_TEST_SUITE_END();
	
	
private:
	SingleParticle2dx::DataStructures::Orientation m_o1, m_o2;
	
public:
	
	/**
	 *  @brief      Set up test case
	 */
	void setUp()
	{
		m_o1 = SingleParticle2dx::DataStructures::Orientation();
		m_o2 = SingleParticle2dx::DataStructures::Orientation(1., 2., 3.);
	}
	
	
	/**
	 *  @brief      Tear down test case
	 */
	void tearDown()
	{}


	/**
	 *  @brief      Test Constructor
	 */
	void testConstructor()
	{
		CPPUNIT_ASSERT( m_o1.getTLTAXIS() == 0.0 );
		CPPUNIT_ASSERT( m_o1.getTLTANG() == 0.0 );
		CPPUNIT_ASSERT( m_o1.getTAXA() == 0.0 );

		CPPUNIT_ASSERT( abs(m_o2.getTLTAXIS()-1) < 1e-8 );
		CPPUNIT_ASSERT( abs(m_o2.getTLTANG() -2) < 1e-8 );
		CPPUNIT_ASSERT( abs(m_o2.getTAXA()   -3) < 1e-8 );
		
		Eigen::Vector3f v;
		v << 7, 8, 9;
		
		SingleParticle2dx::DataStructures::Orientation tmp(v);
		CPPUNIT_ASSERT( abs(tmp.getTLTAXIS()-7) < 1e-8);
		CPPUNIT_ASSERT( abs(tmp.getTLTANG()-8) < 1e-8);
		CPPUNIT_ASSERT( abs(tmp.getTAXA()-9) < 1e-8);
		
		v[0] = 12.;
		CPPUNIT_ASSERT( abs(tmp.getTLTAXIS()-7) < 1e-8 );
		CPPUNIT_ASSERT( abs(tmp.getTLTANG()-8) < 1e-8 );
		CPPUNIT_ASSERT( abs(tmp.getTAXA()-9) < 1e-8 );
		CPPUNIT_ASSERT( v[0] == 12. );
		
		tmp.setTLTAXIS(10.);
		tmp.setTLTANG(20.);
		tmp.setTAXA(30.);
		
		CPPUNIT_ASSERT( abs(tmp.getTLTAXIS()-10) < 1e-8 );
		CPPUNIT_ASSERT( abs(tmp.getTLTANG()-20) < 1e-8 );
		CPPUNIT_ASSERT( abs(tmp.getTAXA()-30) < 1e-8 );
		CPPUNIT_ASSERT( v[0] == 12. );
		CPPUNIT_ASSERT( v[1] == 8. );
		CPPUNIT_ASSERT( v[2] == 9. );	
	}


	/**
	 *  @brief      Test assignment operator
	 */
	void testAssignment()
	{
		m_o1 = m_o2;

		CPPUNIT_ASSERT( abs(m_o1.getTLTAXIS()-1) < 1e-8 );
		CPPUNIT_ASSERT( abs(m_o1.getTLTANG()-2) < 1e-8 );
		CPPUNIT_ASSERT( abs(m_o1.getTAXA()-3) < 1e-8 );
	}
	
	
	/**
	 *  @brief      Test deep copy
	 */
	void testDeepCopy()
	{
		
		m_o1.setTLTAXIS(10.);
		m_o1.setTLTANG(11.);
		m_o1.setTAXA(12.);
		
		CPPUNIT_ASSERT( abs(m_o1.getTLTAXIS()-10) < 1e-8 );
		CPPUNIT_ASSERT( abs(m_o2.getTLTAXIS()-1) < 1e-8 );
		
		CPPUNIT_ASSERT( abs(m_o1.getTLTANG()-11) < 1e-8 );
		CPPUNIT_ASSERT( abs(m_o2.getTLTANG()-2) < 1e-8 );

		CPPUNIT_ASSERT( abs(m_o1.getTAXA()-12) < 1e-8  );
		CPPUNIT_ASSERT( abs(m_o2.getTAXA()-3) < 1e-8 );
		
	}


	/**
	 *  @brief      Test copy constructor
	 */
	void testCopyConstructor()
	{
		SingleParticle2dx::DataStructures::Orientation aux(m_o2);
		
		CPPUNIT_ASSERT( abs(aux.getTLTAXIS()-1) < 1e-8 );
		CPPUNIT_ASSERT( abs(aux.getTLTANG()-2) < 1e-8 );
		CPPUNIT_ASSERT( abs(aux.getTAXA()-3) < 1e-8 );

		aux.setTLTAXIS(2.);
		CPPUNIT_ASSERT( abs(aux.getTLTAXIS()-2) < 1e-8 );
		CPPUNIT_ASSERT( abs(m_o2.getTLTAXIS()-1) < 1e-8 );
	}
	
	
	/**
	 *  @brief      Test calc normal
	 */
	void testCalcNormal()
	{
		//SingleParticle2dx::DataStructures::Orientation o_norm;
		//SingleParticle2dx::DataStructures::Orientation::determineNormalFromTiltGeometry( 0, 0, o_norm);
		
		//CPPUNIT_ASSERT( o_norm.getTLTAXIS() == 0);
		//CPPUNIT_ASSERT( o_norm.getTLTANG() == 0);
		//CPPUNIT_ASSERT( abs(o_norm.getTAXA()-1) < 1e-8);	
	}
	
	void testNorm()
	{
		SingleParticle2dx::DataStructures::Orientation o_norm(400, 100, 500);
		CPPUNIT_ASSERT( fabs(o_norm.getTLTAXIS()-40) < 1e-6 );
		CPPUNIT_ASSERT( fabs(o_norm.getTLTANG()-10) < 1e-6 );
		CPPUNIT_ASSERT( fabs(o_norm.getTAXA()-140) < 1e-6 );
	}

};

#endif /* end of include guard: ORIENTATIONTEST_HPP_3TQJM02S */
