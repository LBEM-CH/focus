#include "../2dxSingleParticle.hpp"

#include <fstream>


#include <boost/program_options.hpp>


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
	
	int n;
	std::string cont_path = config->getContainerFilePath();
	
	SingleParticle2dx::DataStructures::ParticleContainer cont;
	std::string cont_bin_folder = cont_path + "/ParticleContainers/cont_part4";
	
	std::string initial_model_file = cont_path + "/Rec_3d/rec_initial_after_refine1.map";
	std::string initial_model_file_out = config->getContainerName() + "/Rec_3d/rec_initial_nn.map";
	
	std::vector<std::string> folder_content;
	SingleParticle2dx::Utilities::UtilityFunctions::getContentOfFolder(cont_bin_folder, folder_content, ".bin");
	
	std::cout << "starting 2" << std::endl;
	
	#pragma omp parallel for schedule(dynamic,1)
	for(int i=0; i< static_cast<int>(folder_content.size()); i++)
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
	
	//cont.sortContainer();
	
	std::cout << "sorting done" << std::endl;
	
	cont.setParticleNumbers();
	cont.findNeighbors(folder_content.size());
	cont.resetWeightForAllParticles();
	
	n = SingleParticle2dx::ConfigContainer::Instance()->getParticleSize();
	
	std::cout << cont.getNumberOfParticles() << std::endl;
	
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d(n,n,n);
	rec3d.readFromFile(initial_model_file);
	
	rec3d.applyMask();
	rec3d.applyLowPassFilter();
	
	rec3d.writeToFile( initial_model_file_out );
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(initial_model_file_out, "MAP: Initial Model NN", config->getScriptName(), true);
	
	int max_it = config->getMaxIteration();
	
	std::string conv_file_name = "SCRATCH/2dx_SP_nn_convergence.txt";
	SingleParticle2dx::Utilities::UtilityFunctions::removeFileIfExists(conv_file_name);
	
	for(int i=0; i<max_it; i++)
	{
		SingleParticle2dx::real_array3d_type real_data_old(boost::extents[n][n][n]);
		rec3d.getRealSpaceData(real_data_old);
		
		rec3d.updateReconstruction(cont, true, false, false);
		//rec3d.updateReconstruction(cont, false, false);
		
		SingleParticle2dx::Utilities::UtilityFunctions::generateVariableOutput("angular_change_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i), rec3d.getLastAngleChange()/cont.getNumberOfParticles(), config->getScriptName());
				
		rec3d.applyMask();
		rec3d.applyLowPassFilter();
		
		SingleParticle2dx::real_array3d_type real_data(boost::extents[n][n][n]);
		rec3d.getRealSpaceData(real_data);
		float dx = config->getModelKeep() / 100.0;
		for(int ii=0; ii<n; ii++)
		{
			for(int jj=0; jj<n; jj++)
			{
				for(int kk=0; kk<n; kk++)
				{
					real_data_old[ii][jj][kk] = dx*real_data_old[ii][jj][kk] + (1-dx)*real_data[ii][jj][kk];
				}
			}
		}
		rec3d.setFourierSpaceData(real_data_old);
		
		rec3d.writeToFile(config->getContainerName() + "/Rec_3d/reconstruction_nn_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i) + ".map");
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(config->getContainerName() + "/Rec_3d/reconstruction_nn_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i) + ".map", "MAP: Refined Model " + SingleParticle2dx::Utilities::StringFunctions::TtoString(i), config->getScriptName());
		
		SingleParticle2dx::Utilities::UtilityFunctions::writeConvergenceToFile(i+1, rec3d.getLastAngleChange() / cont.getNumberOfParticles(), conv_file_name);
	
		int proc = static_cast<int>(static_cast<float>(i+1)/static_cast<float>(max_it)*100);
		SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( proc );
	
		if ((rec3d.getLastAngleChange()/cont.getNumberOfParticles()) < config->getMinAngleChange())
		{
			SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("CONVERGERD", 1);
			break;
		}	
	}
	
	std::string out_container = config->getContainerName() + "/ParticleContainers/cont_nn.bin";
	SingleParticle2dx::DataStructures::ParticleContainer::serializeContainerToDisk(cont, true, out_container);
	
	rec3d.writeToFile( config->getContainerName() + "/Rec_3d/reconstruction_nn.map" );
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(config->getContainerName() + "/Rec_3d/reconstruction_nn.map", "MAP: Final Reconstruction", config->getScriptName(), true);
	
	cont.writeStatToFile( config->getContainerFilePath() + "/ContainerStatsTXT/container_nn_stat.txt" );
	
	return 0;
}
