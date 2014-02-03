#ifndef PROJECTION_EXAMPLE_TEST_HPP
#define PROJECTION_EXAMPLE_TEST_HPP

#include "cppunit/TestFailure.h"
#include "cppunit/TestAssert.h"
#include <cppunit/extensions/HelperMacros.h>

#include <eigen3/Eigen/Dense>

#include "../2dxSingleParticle.hpp"
#include "../Typedefs.hpp"



class ProjectionExampleTest: public CppUnit::TestFixture  {

	CPPUNIT_TEST_SUITE( ProjectionExampleTest );
		//CPPUNIT_TEST( testProj );
		//CPPUNIT_TEST( testMRCIN );
		//CPPUNIT_TEST( testPartCont );
		//CPPUNIT_TEST( testPicker );
		CPPUNIT_TEST( testConfig );
	CPPUNIT_TEST_SUITE_END();
	
public:
	typedef SingleParticle2dx::value_type value_type;	

private:
	int n;
	
	boost::multi_array<value_type,2> re_proj;
	SingleParticle2dx::DataStructures::Projection2d proj;
	
	
public:
	
	/**
	 *  @brief      Set up test case
	 */
	void setUp()
	{
		n = SingleParticle2dx::ConfigContainer::Instance()->getParticleSize();
		re_proj.resize( boost::extents[n][n] );
		proj = SingleParticle2dx::DataStructures::Projection2d(n,n);
	}
	
	
	/**
	 *  @brief      Tear down
	 */
	void tearDown()
	{}
	
	void testConfig()
	{
		SingleParticle2dx::Utilities::ImageConfigReader config("2dx_image.cfg");
		config.printImageConfigFile();
		
		SingleParticle2dx::Utilities::MergeVariableReader config_merge("test_out.txt");
		config_merge.printConfigFile();
		
		//std::cout << std::endl << "config test: " << config.getConfigElement("imagesidelength")[0] << std::endl;
	}
	
	
	void testPicker()
	{
		SingleParticle2dx::DataStructures::ParticleContainer container;
		SingleParticle2dx::DataStructures::ParticleContainer::pickParticlesFromCCProfile("glpf0000655701", container);
	}
	
	
	void testPartCont()
	{
		SingleParticle2dx::DataStructures::Reconstruction3d rec3d(n, n, n);
		
		boost::multi_array<value_type,3> re_rec;
		re_rec.resize( boost::extents[n][n][n] );
		
		boost::multi_array<value_type,2> temp_2d;
		temp_2d.resize( boost::extents[n][n] );
		
		for(int i=0; i<n; i++)
		{
			for (int j=0; j<n; j++)
			{
				for (int k=0; k<n; k++)
				{
					if ( sqrt((i-n/2)*(i-n/2)+(j-n/2)*(j-n/2)+(k-n/2)*(k-n/2)) < 4 )
					{
						re_rec[i][j][k] = 1;
					}
				}
			}
		}
		
		SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&re_rec);
		rec3d.setFourierSpaceData(re_rec);
		rec3d.printVolumeToFile( "ball_3d.mrc" );
		
		SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
				
		value_type pi = config->getPI();
		SingleParticle2dx::DataStructures::Orientation proj_orientation(90,0,90);
		SingleParticle2dx::DataStructures::Projection2d proj(n, n);
		
		value_type dangle = 90;
		
		SingleParticle2dx::DataStructures::GlobalParticleInformation info;
		SingleParticle2dx::DataStructures::ParticleContainer container;
				
		for (value_type tltaxis=0; tltaxis<360; tltaxis+=dangle)
		{
			for (value_type tltang=0; tltang<=90; tltang+=dangle/2)
			{
				for (value_type taxa=0; taxa<360; taxa+=dangle)
				{
					proj_orientation.setTLTAXIS(tltaxis);
					proj_orientation.setTLTANG(tltang);
					proj_orientation.setTAXA(taxa);
					
					rec3d.calculateProjection(proj_orientation, proj);
					SingleParticle2dx::DataStructures::Particle part(n, n, proj.getOrientation(), info);
					
					proj.getRealSpaceData(temp_2d);
					part.setFourierSpaceData(temp_2d);
					part.updateOrientation(proj_orientation);
					container.addParticle(part);
				}
			}
		}
		
		container.writeContainerToDisk( "TEST_PARTICLES_OUT" );
		rec3d.updateReconstruction(container);
		rec3d.printVolumeToFile( "ball_3d_reconstruction.mrc" );
		
	}
	
	
	void testMRCIN()
	{
		boost::multi_array<value_type,2> read_in_2d;
		SingleParticle2dx::Utilities::MRCFileIO::readFromMrc("MRC_TEST_IN.mrc", &read_in_2d);
		SingleParticle2dx::Utilities::MRCFileIO::writeToMrc(&read_in_2d, "MRC_TEST_OUT.mrc");
		
		boost::multi_array<value_type,3> read_in_3d;
		SingleParticle2dx::Utilities::MRCFileIO::readFromMrc("mrc_glpf.mrc", &read_in_3d);
		SingleParticle2dx::Utilities::MRCFileIO::writeToMrc(&read_in_3d, "mrc_glpf_out.mrc");
		
		SingleParticle2dx::DataStructures::Reconstruction3d rec3d_glpf(n, n, n);
		SingleParticle2dx::DataStructures::Projection2d proj_glpf(n,n);
		rec3d_glpf.setFourierSpaceData(read_in_3d);
		
		SingleParticle2dx::DataStructures::Orientation o(0,0,1);
		rec3d_glpf.calculateProjection(o, proj_glpf);
		proj_glpf.writeToFile( "test_proj_glpf.mrc" );
		
		SingleParticle2dx::DataStructures::Orientation o2(0,1,1);
		rec3d_glpf.calculateProjection(o2, proj_glpf);
		proj_glpf.writeToFile( "test_proj2_glpf.mrc" );
		
		SingleParticle2dx::DataStructures::Orientation o3(1,1,2);
		rec3d_glpf.calculateProjection(o3, proj_glpf);
		proj_glpf.writeToFile( "test_proj3_glpf.mrc" );
		
	}
	
	/**
	 *  @brief      Test projection (example)
	 */
	void testProj()
	{
		SingleParticle2dx::DataStructures::Reconstruction3d rec3d(n, n, n);
		
		boost::multi_array<value_type,3> re_rec;
		re_rec.resize( boost::extents[n][n][n] );
			
		/*
		for(int i=0; i<n; i++)
		{
			for (int j=0; j<n; j++)
			{
				for (int k=0; k<n; k++)
				{
					re_rec[i][j][k] = 0;
				}
			}
		}
		*/
		
		int dx = 6;
		for (int i=n/2-dx/3; i<n/2+dx/3; i++)
		{
			for (int ii = dx; ii<n-dx; ii++)
			{
				for (int jj = dx; jj<n-dx; jj++)
				{
					//if ( sqrt((ii-n/2)*(ii-n/2) + (jj-n/2)*(jj-n/2)) <= n/2 )
					//{
						re_rec[ii][jj][i] = 2;						
					//}
				}
			}
		}
		
		SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&re_rec);
		rec3d.setFourierSpaceData(re_rec);
		
		SingleParticle2dx::DataStructures::Orientation o(1,1,1);
		//rec3d.prepareForProjections();
		rec3d.calculateProjection(o, proj);
		proj.getRealSpaceData(re_proj);
		
		SingleParticle2dx::Utilities::MRCFileIO::writeToMrc(&re_proj, "test_output.mrc");
		SingleParticle2dx::Utilities::MRCFileIO::writeToMrc(&re_rec, "test2_output.mrc");
		proj.writeToFile( "test3_output.mrc" );
		rec3d.printVolumeToFile( "test4_output.mrc" );
	
		}

};


#endif /* end of include guard: PROJECTION_EXAMPLE_TEST_HPP */
