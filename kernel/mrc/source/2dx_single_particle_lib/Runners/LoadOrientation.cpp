#include "../2dxSingleParticle.hpp"

#include <fstream>


int main ()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	std::string cont_new_path = config->getContainerFilePath() + "/ParticleContainers/cont_part2";
	std::string cont_old_path = config->getOrientFile() + "/ParticleContainers/cont_part4";
	
	std::vector<std::string> folder_content_new;
	SingleParticle2dx::Utilities::UtilityFunctions::getContentOfFolder(cont_new_path, folder_content_new, ".bin");
	
	std::vector<std::string> folder_content_old;
	SingleParticle2dx::Utilities::UtilityFunctions::getContentOfFolder(cont_old_path, folder_content_old, ".bin");
	
	if (folder_content_new.size() != folder_content_old.size())
	{
		std::cerr << "Number of containers does not match" << std::endl;
		throw std::runtime_error("Bad operation");
	}
	
	int counter = 0;
	
	#pragma omp parallel for schedule(dynamic,1)
	for(int i=0; i< static_cast<int>(folder_content_new.size()); i++)
	{
		std::cout << "new cont     " << folder_content_new[i] << std::endl;
		std::cout << "old cont     " << folder_content_old[i] << std::endl;
		
		SingleParticle2dx::DataStructures::ParticleContainer cont_old;
		SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(folder_content_old[i], cont_old, true);
		
		SingleParticle2dx::DataStructures::ParticleContainer cont_new;
		SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(folder_content_new[i], cont_new, true);
		
		SingleParticle2dx::DataStructures::Orientation o2set = cont_old(0).getNewOrientation();
		
		cont_new.setAllOrientations( o2set );
		
		SingleParticle2dx::DataStructures::ParticleContainer::serializeContainerToDisk(cont_new, true, folder_content_new[i]);
		
		#pragma omp critical (apply_cont_progress_output)
		{
			int proc = static_cast<int>( 100*(counter+1.0)/folder_content_new.size());
			SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( proc );
			counter++;
		}
	}
	
	return 0;
}
