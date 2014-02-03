#include "../2dxSingleParticle.hpp"

#include <fstream>


#include <boost/program_options.hpp>

std::string stat_filename = "/Div_output/mrc_stat.txt";

void writeMRAStatOutput(int number_of_changes, std::vector<int>& vec)
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	std::cout << "::hi from MRA stat" << std::endl;
	
	std::string result_string = SingleParticle2dx::Utilities::StringFunctions::TtoString(number_of_changes) + "\t";
	
	for(int i=0; i<static_cast<int>(vec.size()); i++)
	{
		result_string += SingleParticle2dx::Utilities::StringFunctions::TtoString(vec[i]) + "\t";
	}
	
	std::string stat_file = config->getContainerName() + stat_filename;
	boost::shared_ptr<FILE> file( fopen ( stat_file.c_str(), "a" ), SingleParticle2dx::Utilities::CustomizedDeleter() );
	fprintf(file.get(), "%s\n", result_string.c_str());
	
	std::cout << "::" << result_string << std::endl;
}

int main ()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();

	
	SingleParticle2dx::Utilities::MergeVariableReader config_merge("mergevars_written_to_file.txt");
	if( static_cast<bool>(config_merge.getFloatElement("config_only_sp2dx")[0]) )
	{
		std::cout << config_merge.getFloatElement("config_only_sp2dx")[0] << std::endl;
		std::cout << "::ONLY WRITING CONFIG" << std::endl;
		std::ofstream ofs("config.bin");
		SingleParticle2dx::archive_out_type oa(ofs);
		oa << config;
		return 0;
	}

	std::cout << "starting" << std::endl;
	
	int n = SingleParticle2dx::ConfigContainer::Instance()->getParticleSize();
	std::string cont_path = config->getContainerFilePath();
	
	SingleParticle2dx::DataStructures::ParticleContainer cont;
	std::string cont_bin_folder = cont_path + "/ParticleContainers/cont_part4";
	
	std::vector<std::string> folder_content;
	SingleParticle2dx::Utilities::UtilityFunctions::getContentOfFolder(cont_bin_folder, folder_content, ".bin");
	
	std::cout << "starting 2" << std::endl;
	
	#pragma omp parallel for schedule(dynamic,1)
	for(int i=0; i<static_cast<int>(folder_content.size()); i++)
	{
		SingleParticle2dx::DataStructures::ParticleContainer cont_local;
		SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(folder_content[i], cont_local, true);
		
		cont_local.resetImageNumber(i);
		
		#pragma omp critical(merge_nn)
		{
			SingleParticle2dx::DataStructures::ParticleContainer::mergeContainers(cont, cont_local);
		}
	}
	
	std::cout << "container read" << std::endl;
	
//	cont.addRandomNoiseToContainer(12);
	cont.applyMaskToContainerInParallel();
	
	SingleParticle2dx::DataStructures::Reconstruction3d rec_full(n,n,n);
	rec_full.generateInitialModel(cont);
	rec_full.writeToFile(config->getContainerName() + "/Rec_3d/reconstruction_mra_full.map");
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(config->getContainerName() + "/Rec_3d/reconstruction_mra_full.map", "MAP: Non-classified Model", config->getScriptName(), false, true);
	
	return 0;
	
	cont(0).writeToFile(config->getContainerName() + "/Div_output/particle_0.mrc");
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(config->getContainerName() + "/Div_output/particle_0.mrc", "Particle", config->getScriptName(), false, true);
	
	std::cout << ":MRA-NN Test: " << cont(0).getNeighbors().size() << std::endl;
	
	//cont.sortContainer();
	
	std::cout << "sorting done" << std::endl;
	
	std::cout << "::NUMBER OF CLASSES: " << cont.getNumberOfClasses() << std::endl;
	std::cout << "::NUMBER OF CLASSES config: " << config->getNumberOfClasses() << std::endl;
	
	cont.setParticleNumbers();
	
	std::cout << cont.getNumberOfParticles() << std::endl;
	
	std::vector<SingleParticle2dx::DataStructures::Reconstruction3d> rec_vec;
	
	for(int i=0; i<config->getNumberOfClasses(); i++)
	{
		SingleParticle2dx::DataStructures::Reconstruction3d rec3d(n,n,n);
		rec_vec.push_back(rec3d);
	}
	
//	SingleParticle2dx::Utilities::MRAFunctions::calculateMRAReconstruction ( cont, rec_vec );
//	cont.setModuloClasses();
	cont.setRandomClasses();
	
	SingleParticle2dx::Utilities::MRAFunctions::calculateMRAReconstruction ( cont, rec_vec );
	
	std::string stat_file = config->getContainerName() + stat_filename;
	SingleParticle2dx::Utilities::UtilityFunctions::removeFileIfExists(stat_file);
	
	for(int i=0; i<static_cast<int>(rec_vec.size()); i++)
	{
		rec_vec[i].writeToFile(config->getContainerName() + "/Rec_3d/reconstruction_mra_test_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i) + ".map");
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(config->getContainerName() + "/Rec_3d/reconstruction_mra_test_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i) + ".map", "MAP: MRA Model " + SingleParticle2dx::Utilities::StringFunctions::TtoString(i), config->getScriptName(), true, false);
	}
	
	
	std::cout << "check: " << rec_vec.size() << std::endl;
		
	SingleParticle2dx::Utilities::UtilityFunctions::forceReload();
	
	SingleParticle2dx::DataStructures::ParticleContainer dummy_cont;

	for(int i=0; i<config->getMaxIteration(); i++)
	{	
		for(int proj_index=0; proj_index<static_cast<int>(rec_vec.size()); proj_index++)
		{
			rec_vec[proj_index].forceProjectionPreparation(dummy_cont);
		}
		
		if(config->getMRAUseNN())
		{
			cont.findNeighborsIncludeClasses(static_cast<int>(folder_content.size()));
		}
		
		int number_of_changes = cont.MRAClassify(rec_vec);
		
		std::vector<int> vec;
		cont.getNumberOfElementsInEachClass( vec );
		writeMRAStatOutput(number_of_changes, vec);
		
		SingleParticle2dx::Utilities::MRAFunctions::calculateMRAReconstruction ( cont, rec_vec );
				
		SingleParticle2dx::Utilities::UtilityFunctions::generateVariableOutput("Number_of_changes", number_of_changes, config->getScriptName(), false);
		SingleParticle2dx::Utilities::UtilityFunctions::generateVariableOutput("Iteration_number", i+1, config->getScriptName(), true);
		
		int proc = static_cast<int>( 100*(i+1.0)/config->getMaxIteration());
		SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( proc );
		
		if( number_of_changes == 0 )
		{
			break;
		}
	}
	
	cont.writeStatToFile( config->getContainerFilePath() + "/ContainerStatsTXT/container_mra_stat.txt" );
	
	for(int i=0; i<static_cast<int>(rec_vec.size()); i++)
	{
		rec_vec[i].writeToFile(config->getContainerName() + "/Rec_3d/reconstruction_mra_1_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i) + ".map");
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(config->getContainerName() + "/Rec_3d/reconstruction_mra_1_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i) + ".map", "MAP: MRA Model 2 " + SingleParticle2dx::Utilities::StringFunctions::TtoString(i), config->getScriptName(), true, false);
	}
	
	
	SingleParticle2dx::Utilities::UtilityFunctions::forceReload();
	
	
	return 0;
}
