#include "../2dxSingleParticle.hpp"

#include <fstream>

#include <boost/lexical_cast.hpp>

#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>


int main (int argc, char const *argv[])
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	int n = 116;
	
	config->setParticleSize(n);
	config->setTrialAngleGenerator(4);
	config->setProjectionMethod(2);
	config->setParallelProjection(false);
	config->setRefinementMethod(0);
	config->setBackprojectionMethod(2);
	config->setReconstructionMaskingMethod(0);
	config->setProjectionMaskingMethod(0);
	config->setCacheProjections(false);
	config->setLPProjectionRadius(n/2);
	
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d(n,n,n);
	SingleParticle2dx::DataStructures::Projection2d p(n,n);
	SingleParticle2dx::DataStructures::ParticleContainer c;
	
	rec3d.readFromFile("test.mrc");
	rec3d.forceProjectionPreparation(c);
	
	SingleParticle2dx::DataStructures::Orientation o(0,0,0);
	rec3d.calculateProjection(o, p);
	p.writeToFile("test_proj.mrc");
	
	SingleParticle2dx::DataStructures::Orientation o2(0,45,0);
	rec3d.calculateProjection(o2, p);
	p.writeToFile("test_proj2.mrc");
	
		
	return 0;
}
