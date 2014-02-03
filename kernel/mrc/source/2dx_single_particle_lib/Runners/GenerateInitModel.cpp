#include "../2dxSingleParticle.hpp"

#include <fstream>

int main ()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	int n = config->getParticleSize();
	
	config->setProjectionMethod(4);
	config->setCacheProjections(false);
	config->setParallelProjection(false);
	config->setProjectionMaskingMethod(0);
	config->setRefinementMethod(0);
	config->setTrialAngleGenerator(4);
	config->setBackprojectionMethod(0);
	config->setLPProjectionRadius(n);
	
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d(n,n,n);
	
	SingleParticle2dx::Utilities::UtilityFunctions::generateInitialModelForInitRef(rec3d);
	
	rec3d.scale(1/1.34);
	
	SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( 33 );
	
	SingleParticle2dx::DataStructures::ParticleContainer c_dummy;
	rec3d.forceProjectionPreparation(c_dummy);
	
	SingleParticle2dx::DataStructures::Projection2d proj1(n,n);
	SingleParticle2dx::DataStructures::Projection2d proj2(n,n);
	SingleParticle2dx::DataStructures::Projection2d proj3(n,n);
	
	SingleParticle2dx::DataStructures::Orientation o1(0,0,0);
	SingleParticle2dx::DataStructures::Orientation o2(0,90,0);
	SingleParticle2dx::DataStructures::Orientation o3(0,90,90);
	
	rec3d.calculateProjection(o1, proj1);
	rec3d.calculateProjection(o2, proj2);
	rec3d.calculateProjection(o3, proj3);
	
	if(config->getShowSights())
	{	
		proj1.setMidddleTarget();
		proj2.setMidddleTarget();
		proj3.setMidddleTarget();
	}
	
	SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( 66 );
	
	std::string cont_folder_name = config->getContainerName() + "/Div_output/";
	std::string filename_p1 = cont_folder_name + "init_topview.mrc";
	std::string filename_p2 = cont_folder_name + "init_sideview1.mrc";
	std::string filename_p3 = cont_folder_name + "init_sideview2.mrc";
	
	proj1.writeToFile(filename_p1);
	proj2.writeToFile(filename_p2);
	proj3.writeToFile(filename_p3);
	
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(filename_p1, "Top View", config->getScriptName(), false, false);
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(filename_p2, "Side View X", config->getScriptName(), false, false);
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(filename_p3, "Side View Y", config->getScriptName(), false, false);
	
	rec3d.writeToFile(config->getContainerName() + "/Rec_3d/Init3DFromMRC.map");
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(config->getContainerName() + "/Rec_3d/Init3DFromMRC.map", "Initial 3D Reference", config->getScriptName(), true, true);
	
	SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( 100 );
	
	return 0;
}
