#include "../2dxSingleParticle.hpp"



int main(int argc, char const *argv[])
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	SingleParticle2dx::value_type mode = 0;
	
	int n = config->getParticleSize();
	
	if ( argc != 2 )
	{
		std::cerr << "Usage: ./2dx_NoTilt_Exe.exe <mode>" << std::endl;
	}
	else
	{
		mode = atoi(argv[1]);
	}
	
	if ( mode == 1 )
	{
		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("generating 3d model", 1);
		
		config->setProjectionMethod(3);
//		config->setLocalCTFCorrection(true);

		SingleParticle2dx::DataStructures::ParticleContainer cont;
		std::vector<std::string> image_dirs = SingleParticle2dx::ConfigContainer::Instance()->getImageDirectories();
		SingleParticle2dx::DataStructures::PickingDiagnostics dia;
		for (SingleParticle2dx::value_type i=0; i<config->getNumberOfImages(); i++)
		{
			std::string working_dir = (config->getProjectDirectory() + image_dirs[i]);
			SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("picking from: " + working_dir, 2);
			SingleParticle2dx::DataStructures::ParticleContainer::pickParticlesFromCCProfile(working_dir, cont, dia, i);
		}
		cont.setParticleNumbers();
		SingleParticle2dx::DataStructures::Reconstruction3d rec3d(n, n, n);
		rec3d.generateInitialModel(cont);
		rec3d.writeToFile( "NonTilt_Ref.map" );
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("NonTilt_Ref.map", "MAP: Reference used for TAXA determination", "2dx_SP_ZeroTiltOrientation", true);
		
	}
	else if ( mode == 2 )
	{
		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("finding best TAXA", 1);
		
		config->setTrialAngleGenerator(3);
	//	config->setLocalCTFCorrection(true);
		config->setProjectionMaskingMethod(2);
		//config->setReconstructionMaskingMethod(4);
		
		SingleParticle2dx::DataStructures::Reconstruction3d rec3d(n, n, n);
		SingleParticle2dx::real_array3d_type data3d (boost::extents[n][n][n]);
	//	SingleParticle2dx::Utilities::MRCFileIO::readFromMrc( "NonTilt_Ref.map", &data3d);
		rec3d.setFourierSpaceData( data3d );
		
		rec3d.applyLowPassFilter();
		rec3d.applyMask();	
	
		SingleParticle2dx::DataStructures::ParticleContainer cont;
		std::vector<std::string> image_dirs = SingleParticle2dx::ConfigContainer::Instance()->getImageDirectories();	
		
		std::string working_dir = (config->getProjectDirectory() + image_dirs[0]);
		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("picking from: " + working_dir, 2);
		SingleParticle2dx::DataStructures::PickingDiagnostics dia;
		SingleParticle2dx::DataStructures::ParticleContainer::pickParticlesFromCCProfile(working_dir, cont, dia, 0);
		
		cont.setParticleNumbers();
		SingleParticle2dx::real_array2d_type data_2d (boost::extents[n][n]);
		cont.getAverage(data_2d);
		
		rec3d.updateReconstruction(cont);
		std::string filename = "SCRATCH/notilt_taxas.txt";
		boost::shared_ptr<FILE> file( fopen ( filename.c_str(), "w" ), SingleParticle2dx::Utilities::CustomizedDeleter() );
		
		int use_counter = 0;
		for (int i=0; i<cont.getNumberOfParticles(); i++)
		{
			if ( cont(i).getUseForReconstruction() )
			{
				use_counter++;
				fprintf(file.get(), "%f\n", fmod(cont(i).getNewOrientation().getTAXA(),90));
			}
		}
		
		rec3d.setFourierSpaceData( data3d );
		rec3d.applyLowPassFilter();
		rec3d.applyMask();
		
		SingleParticle2dx::DataStructures::ParticleContainer cont2;
		
		SingleParticle2dx::DataStructures::Orientation o;
		SingleParticle2dx::DataStructures::GlobalParticleInformation i;
		
		SingleParticle2dx::DataStructures::Particle p(n,n, o, i);
		p.setFourierSpaceData(&data_2d);
		p.writeToFile ("2d_ref.mrc");
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("2d_ref.mrc", "Average used for TAXA finding", "2dx_SP_ZeroTiltOrientation", true);
		
		SingleParticle2dx::DataStructures::Projection2d proj_top(n,n);
		rec3d.calculateProjection(o, proj_top);
		proj_top.writeToFile("top_projection.mrc");
		
		cont2.addParticle(p);
		rec3d.updateReconstruction(cont2);
		
		o.setTAXA(cont2(0).getNewOrientation().getTAXA());
		rec3d.calculateProjection(o, proj_top);
		proj_top.writeToFile("best_projection.mrc");
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("best_projection.mrc", "Best maching Projection", "2dx_SP_ZeroTiltOrientation", true);
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("top_projection.mrc", "Reference Projection", "2dx_SP_ZeroTiltOrientation", false);
		
		std::cout << fmod(cont2(0).getNewOrientation().getTAXA(),90) << std::endl;
		std::cout << use_counter/cont.getNumberOfParticles() << std::endl;
		
	}
	else
	{
		std::cerr << "Unknown mode" << std::endl;
		throw std::runtime_error("Bad operation");
	}
	
	return 0;
}