#include "../2dxSingleParticle.hpp"

#include <fstream>

#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>


int main ()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	std::string cont_path = config->getContainerFilePath();
	std::string cont_bin_folder = cont_path + "/ParticleContainers/cont_part2";
	std::string cont_bin_folder_fp = cont_path + "/FingerPrintContainers/cont_part2/";
	
	std::vector<std::string> folder_content;
	SingleParticle2dx::Utilities::UtilityFunctions::getContentOfFolder(cont_bin_folder, folder_content, ".bin");
	
	SingleParticle2dx::DataStructures::ParticleContainer cont_ave;
	
	if (config->getRefMode() == 0)
	{
		int np = 0;
		#pragma omp parallel for schedule(dynamic,1) reduction(+:np)
		for(int i=0; i< static_cast<int>(folder_content.size()); i++)
		{
			SingleParticle2dx::DataStructures::ParticleContainer cont;
			SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(folder_content[i], cont, true);
		
			if(config->getRestartRef())
			{
				std::vector<std::string> split_vector;
				SingleParticle2dx::Utilities::StringFunctions::splitString( split_vector, folder_content[i], std::string("/") );
				std::string filename_core_fp = split_vector.back();
				std::string folder_bin_fp = cont_bin_folder_fp + filename_core_fp;
				SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(folder_bin_fp, cont, false);
			}
		
			np += cont.getNumberOfParticles();
		
			SingleParticle2dx::DataStructures::ParticleContainer cont_ave_local;
			cont.generateAverageContainer(cont_ave_local, false, false);
			SingleParticle2dx::DataStructures::Particle part2add = cont_ave_local(0);
			part2add.setParticleNumber(i);
			
			SingleParticle2dx::Utilities::AverageWeighter ave_weighter(config->getAveWeightMode(), cont.getNumberOfParticles(), part2add.getNewOrientation().getTLTANG());
			part2add.setWeight(ave_weighter.getWeight());
		
			cont_ave.addParticle(part2add);
		}
	}
	
	config->setTrialAngleGenerator(4);
	config->setBackprojectionMethod(2);
	
	int n = SingleParticle2dx::ConfigContainer::Instance()->getParticleSize();
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d_ave(n, n, n);
	
	// @TODO Load from file
	//SingleParticle2dx::Utilities::UtilityFunctions::generateInitialModelForInitRef(rec3d_ave, cont_ave);
	
	cont_ave.clear();
	
	std::string ave_folder = cont_path + "/Particles/ave_folder";
	if(boost::filesystem::exists(ave_folder))
	{
		boost::filesystem::remove_all(ave_folder);
	}
	boost::filesystem::create_directory(ave_folder);
	
	std::string cont_bin_folder_out = cont_path + "/ParticleContainers/cont_part3/";
	if(boost::filesystem::exists(cont_bin_folder_out))
	{
		boost::filesystem::remove_all(cont_bin_folder_out);
	}
	boost::filesystem::create_directory(cont_bin_folder_out);
	
	std::string cont_stat_file_name = cont_path + "/Div_output/refine_pick.txt";
	if(boost::filesystem::exists(cont_stat_file_name))
	{
		boost::filesystem::remove(cont_stat_file_name);
	}
	
	int counter = 0;
	
	#pragma omp parallel for schedule(dynamic,1) 
	for(int i=0; i< static_cast<int>(folder_content.size()); i++)
	{
		SingleParticle2dx::DataStructures::ParticleContainer cont;
		SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(folder_content[i], cont, true);
	
		std::vector<std::string> split_vector;
		SingleParticle2dx::Utilities::StringFunctions::splitString( split_vector, folder_content[i], std::string("/") );
		std::string filename_core_fp = split_vector.back();
		std::string folder_bin_fp = cont_bin_folder_fp + filename_core_fp;
		SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(folder_bin_fp, cont, false);
		
		SingleParticle2dx::DataStructures::ParticleContainer cont_ave_local;
		cont.generateAverageContainer(cont_ave_local, false, false);

		SingleParticle2dx::DataStructures::Particle part_to_store(n,n);
		part_to_store = cont_ave_local(0);
		
		SingleParticle2dx::DataStructures::Projection2d proj(n,n);
		cont.findNeighbors(1);
		
		SingleParticle2dx::DataStructures::Reconstruction3d rec3d_ave_local = rec3d_ave;
		rec3d_ave_local.forceProjectionPreparation(cont);
		rec3d_ave_local.calculateProjection(cont(0).getNewOrientation(), proj);
		
		SingleParticle2dx::Utilities::UtilityFunctions::alignInplaneShift(cont, proj, true, 0.0, false);
	
		#pragma omp critical (refine_picking_stat_output)
		{
			cont.writeStatToFile(cont_stat_file_name, true);
		}
		
		SingleParticle2dx::Utilities::UtilityFunctions::applyInplaneShift(cont);
		
		cont_ave_local.clear();
		cont.generateAverageContainer(cont_ave_local, false, false);
		SingleParticle2dx::DataStructures::Particle part2add = cont_ave_local(0);
		part2add.setParticleNumber(i);
		
		SingleParticle2dx::Utilities::AverageWeighter ave_weighter(config->getAveWeightMode(), cont.getNumberOfParticles(), part2add.getNewOrientation().getTLTANG());
		part2add.setWeight(ave_weighter.getWeight());
		
		cont_ave.addParticle(part2add);
		
		#pragma omp critical (write_refine_picking_particles_to_folder)
		{
			part_to_store.writeToFile(ave_folder + "/uncorrected_ave_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i) + ".mrc");
			SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(config->getContainerName() + "/Particles/ave_folder/uncorrected_ave_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i) + ".mrc", "Ave: uncorrected_ave_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i), config->getScriptName(), false, false);
			
			cont_ave_local(0).writeToFile(ave_folder + "/corrected_ave_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i) + ".mrc");
			SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(config->getContainerName() + "/Particles/ave_folder/corrected_ave_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i) + ".mrc", "Ave: corrected_ave_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i), config->getScriptName(), false, false);
		
			proj.writeToFile(ave_folder + "/projection_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i) + ".mrc");
			SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(config->getContainerName() + "/Particles/ave_folder/projection_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i) + ".mrc", "Ave: Reprojection_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i), config->getScriptName(), false, false);
		}
		
		std::vector<std::string> split_vector_out;
		SingleParticle2dx::Utilities::StringFunctions::splitString( split_vector_out, folder_content[i], std::string("/") );
		std::string filename_core_out = split_vector.back();
		
		std::string folder_bin_out = cont_bin_folder_out + filename_core_out;
		SingleParticle2dx::DataStructures::ParticleContainer::serializeContainerToDisk(cont, true, folder_bin_out);
		
		#pragma omp critical (update_progress_for_refine_picking)
		{
			counter++;
			int proc = static_cast<int>(static_cast<float>(counter)/(static_cast<float>(folder_content.size()))*100);
			SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( proc );
		}
	}
	
	rec3d_ave.generateInitialModel(cont_ave);
	rec3d_ave.applyMask();
	rec3d_ave.applyLowPassFilter();
	
	
	rec3d_ave.writeToFile( cont_path + "/Rec_3d/ref_model.map" );
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(config->getContainerName() + "/Rec_3d/ref_model.map", "MAP: Reference Model", config->getScriptName(), true);
	
	return 0;
}
