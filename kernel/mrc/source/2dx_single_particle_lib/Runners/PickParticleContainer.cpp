#include "../2dxSingleParticle.hpp"

#include <fstream>

#include <boost/filesystem.hpp>



int main ()
{
	typedef boost::archive::binary_oarchive archive_out_type;
	SingleParticle2dx::DataStructures::ParticleContainer cont;

	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	int n;
	
	std::string cont_path = config->getContainerFilePath();
	std::cout << "path= " << cont_path << std::endl;

	n = config->getParticleSize();
	

	SingleParticle2dx::Utilities::UtilityFunctions::removeFolderIfExists("sp_ave");
	boost::filesystem::create_directory("sp_ave");

	SingleParticle2dx::DataStructures::PickingDiagnostics dia;
	std::vector<std::string> image_dirs = SingleParticle2dx::ConfigContainer::Instance()->getImageDirectories();

	std::cout << "project dir:" << config->getProjectDirectory() << std::endl;

	std::vector<SingleParticle2dx::DataStructures::ParticleContainer> cont_vec;

	int counter = 0;
	
	boost::shared_ptr<FILE> file_key( fopen ( (cont_path + "/Div_output/file_key.txt").c_str(), "w" ), SingleParticle2dx::Utilities::CustomizedDeleter() );
	
	std::string cont_bin_folder = cont_path + "/ParticleContainers/cont_part1";
	
	SingleParticle2dx::Utilities::UtilityFunctions::removeFolderIfExists(cont_bin_folder);
	boost::filesystem::create_directory(cont_bin_folder);
	
	SingleParticle2dx::Utilities::UtilityFunctions::generateVariableOutput("number_images", config->getNumberOfImages(), config->getScriptName(), true);
	
	std::string container_stat_file = cont_path + "/ContainerStatsTXT/original.txt";
	SingleParticle2dx::Utilities::UtilityFunctions::removeFileIfExists(container_stat_file);
	
	if(config->getDoDoublePick())
	{
		#pragma omp parallel for schedule(dynamic,1)
		for (int i=0; i<config->getNumberOfImages(); i++)
		{
			SingleParticle2dx::DataStructures::ParticleContainer cont;
			std::vector<std::string> split_vector;
			SingleParticle2dx::Utilities::StringFunctions::splitString( split_vector, image_dirs[i], std::string("/") );
		
			std::string working_dir = (config->getProjectDirectory() + image_dirs[i]);
			SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("picking from: " + working_dir, 2);
			
			std::string cont_bin_file = cont_bin_folder + "/cont" + SingleParticle2dx::Utilities::StringFunctions::TtoString(2*i) + ".bin";
			
			int good_particles = SingleParticle2dx::DataStructures::ParticleContainer::pickParticlesFromCCProfile(working_dir, cont, dia, 2*i, false);
			
			if(cont.getNumberOfParticles() > 0)
			{
				SingleParticle2dx::Utilities::UtilityFunctions::generateVariableOutput(split_vector.back(), good_particles, config->getScriptName(), false);
				SingleParticle2dx::DataStructures::ParticleContainer::serializeContainerToDisk(cont, true, cont_bin_file);
				
				SingleParticle2dx::real_array2d_type ave_im_sx ( boost::extents[cont(0).getSizeX()][cont(0).getSizeY()] );
				cont.getAverage(ave_im_sx);
				SingleParticle2dx::Utilities::MRCFileIO::writeToMrc(&ave_im_sx, "sp_ave/" + cont(0).getGlobalParticleInformation().getImageName() + ".mrc" );
			}
  	  	
  	 		#pragma omp critical (update_progress_for_picking)
			{
				cont.writeStatToFile(container_stat_file, true);
				fprintf(file_key.get(), "%i\t%i\t%s\n", 2*i, cont.getNumberOfParticles(), image_dirs[i].c_str());
				int proc = static_cast<int>(static_cast<float>(counter+1)/(2*static_cast<float>(config->getNumberOfImages()))*100);
				SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( proc );
				counter++;
			}
		}
		
		std::cout << "::FIRST LOOP DONE" << std::endl;
		
		#pragma omp parallel for schedule(dynamic,1)	
		for (int i=0; i<config->getNumberOfImages(); i++)
		{
			SingleParticle2dx::DataStructures::ParticleContainer cont;
			std::vector<std::string> split_vector;
			SingleParticle2dx::Utilities::StringFunctions::splitString( split_vector, image_dirs[i], std::string("/") );
		
			std::string working_dir = (config->getProjectDirectory() + image_dirs[i]);
			SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("picking from: " + working_dir, 2);
			
			std::string cont_bin_file = cont_bin_folder + "/cont" + SingleParticle2dx::Utilities::StringFunctions::TtoString(2*i+1) + ".bin";
			
			int good_particles = SingleParticle2dx::DataStructures::ParticleContainer::pickParticlesFromCCProfile(working_dir, cont, dia, 2*i+1, true);
			
			if(cont.getNumberOfParticles() > 0)
			{
				SingleParticle2dx::Utilities::UtilityFunctions::generateVariableOutput(split_vector.back()+"_dual", good_particles, config->getScriptName(), false);
				SingleParticle2dx::DataStructures::ParticleContainer::serializeContainerToDisk(cont, true, cont_bin_file);
				
				SingleParticle2dx::real_array2d_type ave_im_sx ( boost::extents[cont(0).getSizeX()][cont(0).getSizeY()] );
				cont.getAverage(ave_im_sx);
				SingleParticle2dx::Utilities::MRCFileIO::writeToMrc(&ave_im_sx, "sp_ave/" + cont(0).getGlobalParticleInformation().getImageName() + ".mrc" );
			}
			
  	 		#pragma omp critical (update_progress_for_picking)
			{
				cont.writeStatToFile(container_stat_file, true);
				fprintf(file_key.get(), "%i\t%i\t%s\n", 2*i+1, cont.getNumberOfParticles(), image_dirs[i].c_str());
				int proc = static_cast<int>(static_cast<float>(counter+1)/(2*static_cast<float>(config->getNumberOfImages()))*100);
				SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( proc );
				counter++;
			}
		}
	}
	else
	{
		#pragma omp parallel for schedule(dynamic,1)
		for (int i=0; i<config->getNumberOfImages(); i++)
		{
			SingleParticle2dx::DataStructures::ParticleContainer cont;
			std::vector<std::string> split_vector;
			SingleParticle2dx::Utilities::StringFunctions::splitString( split_vector, image_dirs[i], std::string("/") );
		
			std::string working_dir = (config->getProjectDirectory() + image_dirs[i]);
			SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("picking from: " + working_dir, 2);
			
			std::string cont_bin_file = cont_bin_folder + "/cont" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i) + ".bin";
			
			int good_particles = SingleParticle2dx::DataStructures::ParticleContainer::pickParticlesFromCCProfile(working_dir, cont, dia, i, false);
			
			if(cont.getNumberOfParticles() > 0)
			{
				SingleParticle2dx::Utilities::UtilityFunctions::generateVariableOutput(split_vector.back(), good_particles, config->getScriptName(), false);
				SingleParticle2dx::DataStructures::ParticleContainer::serializeContainerToDisk(cont, true, cont_bin_file);
				
				SingleParticle2dx::real_array2d_type ave_im_sx ( boost::extents[cont(0).getSizeX()][cont(0).getSizeY()] );
				cont.getAverage(ave_im_sx);
				SingleParticle2dx::Utilities::MRCFileIO::writeToMrc(&ave_im_sx, "sp_ave/" + cont(0).getGlobalParticleInformation().getImageName() + ".mrc" );
			}
  	  	
  	 		#pragma omp critical (update_progress_for_picking)
			{
				cont.writeStatToFile(container_stat_file, true);
				fprintf(file_key.get(), "%i\t%i\t%s\n", i, cont.getNumberOfParticles(), image_dirs[i].c_str());
				int proc = static_cast<int>(static_cast<float>(counter+1)/(static_cast<float>(config->getNumberOfImages()))*100);
				SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( proc );
				counter++;
			}
		}
	}
	
  	dia.print();
	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput(SingleParticle2dx::Utilities::StringFunctions::TtoString(dia.getNumberOfGoodParticles()) + " particles picked", 1);
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(config->getContainerName() + "/Div_output/file_key.txt", "Image Keys", config->getScriptName(), false, false);
	SingleParticle2dx::Utilities::UtilityFunctions::forceReload();


	return 0;
}
