#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/ui/text/TestRunner.h>

#include "OrientationTest.hpp"
#include "GlobalParticleInformationTest.hpp"
#include "ParticleShiftTest.hpp"
#include "ParticleTest.hpp"
#include "Projection2dTest.hpp"
#include "FFTTest.hpp"
#include "UtilityTest.hpp"
#include "ProjectionExampleTest.hpp"


CPPUNIT_TEST_SUITE_REGISTRATION( OrientationTest );
CPPUNIT_TEST_SUITE_REGISTRATION( GlobalParticleInformationTest );
CPPUNIT_TEST_SUITE_REGISTRATION( ParticleShiftTest );
CPPUNIT_TEST_SUITE_REGISTRATION( ParticleTest );
CPPUNIT_TEST_SUITE_REGISTRATION( Projection2dTest );
CPPUNIT_TEST_SUITE_REGISTRATION( FFTTest );
CPPUNIT_TEST_SUITE_REGISTRATION( UtilityTest );
CPPUNIT_TEST_SUITE_REGISTRATION( ProjectionExampleTest );

int main()
{
  CppUnit::TextUi::TestRunner runner;
  CppUnit::TestFactoryRegistry &registry = CppUnit::TestFactoryRegistry::getRegistry();
  runner.addTest( registry.makeTest() );
  bool wasSuccessful = runner.run( "", false );
  return wasSuccessful;
}
