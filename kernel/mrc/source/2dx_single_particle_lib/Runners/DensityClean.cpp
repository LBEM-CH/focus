#include <fstream>


#include <boost/filesystem.hpp>

#include "../2dxSingleParticle.hpp"

int main ()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
		
	std::string cont_path = config->getContainerFilePath();
	std::string cont_bin_folder = cont_path + "/ParticleContainers/cont_part1";
	std::string cont_bin_folder_new = cont_path + "/ParticleContainers/cont_part2";
	
	if(boost::filesystem::exists(cont_bin_folder_new))
	{
		boost::filesystem::remove_all(cont_bin_folder_new);
	}
	boost::filesystem::create_directory(cont_bin_folder_new);
	
	std::vector<std::string> folder_content;
	SingleParticle2dx::Utilities::UtilityFunctions::getContentOfFolder(cont_bin_folder, folder_content, ".bin");
	
	int old_particle_number = 0;
	int new_particle_number = 0;
	int counter = 0;
	
	if(boost::filesystem::exists(cont_path + "/ContainerStatsTXT/container_stat_density.txt"))
	{
		boost::filesystem::remove(cont_path + "/ContainerStatsTXT/container_stat_density.txt");
	}
	
	#pragma omp parallel for schedule(dynamic,1) reduction(+:old_particle_number) reduction(+:new_particle_number)
	for(int i=0; i<static_cast<int>(folder_content.size()); i++)
	{
		SingleParticle2dx::DataStructures::ParticleContainer cont;
		SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(folder_content[i], cont, true);
	
		cont.setParticleNumbers();
		cont.findNeighbors(1);
		
		std::pair<float, float> center_of_mass;
		cont.getCenterOfMass(center_of_mass);
		
		int cm_part_nb = cont.getNearestParticleNumber(center_of_mass);
		cont(cm_part_nb).setIsCMParticle(true);
		
		float sample_density = (cont(cm_part_nb)).calculateDensity();
		float minimal_density = (config->getMinDensity() / 100.0) * sample_density;
		
		SingleParticle2dx::DataStructures::ParticleContainer cont_new;
		
		for(int j=0; j<cont.getNumberOfParticles(); j++)
		{
			if( ((cont(j)).calculateDensity()) > minimal_density )
			{
				cont_new.addParticleFast(cont(j));
			}
			else
			{
				cont(j).setUseForReconstruction(false);
			}
		}
		
		old_particle_number += cont.getNumberOfParticles();
		new_particle_number += cont_new.getNumberOfParticles();
			
		#pragma omp critical (density_clean_output)
		{
			cont.writeStatToFile(cont_path + "/ContainerStatsTXT/container_stat_density.txt" , true);
			int proc = static_cast<int>( 100*(counter+1.0)/folder_content.size());
			SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( proc );
			counter++;
		}
		
		std::string count_new_file = cont_bin_folder_new + "/cont" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i) + ".bin";
		SingleParticle2dx::DataStructures::ParticleContainer::serializeContainerToDisk(cont_new, true, count_new_file);
	}
	
	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Initial number of particles: " + SingleParticle2dx::Utilities::StringFunctions::TtoString(old_particle_number), 1);
	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Cleaned number of particles: " + SingleParticle2dx::Utilities::StringFunctions::TtoString(new_particle_number), 1);
	
	SingleParticle2dx::Utilities::UtilityFunctions::generateVariableOutput("Initial_Number", old_particle_number, config->getScriptName(), false);
	SingleParticle2dx::Utilities::UtilityFunctions::generateVariableOutput("Cleaned_Number", new_particle_number, config->getScriptName(), false);
	SingleParticle2dx::Utilities::UtilityFunctions::forceReload();
	
	return 0;
}
