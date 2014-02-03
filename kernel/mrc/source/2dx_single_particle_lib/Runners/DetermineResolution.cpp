#include "../2dxSingleParticle.hpp"

#include <fstream>


#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>


int main ()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	int n;
	std::string cont_path = config->getContainerFilePath();
	
	SingleParticle2dx::DataStructures::ParticleContainer cont;
	std::string cont_bin_file = cont_path + "/ParticleContainers/cont_nn.bin";
	SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(cont_bin_file, cont, true);
	
	SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( 10 );
	
	cont.setParticleNumbers();

	n = SingleParticle2dx::ConfigContainer::Instance()->getParticleSize();

	SingleParticle2dx::DataStructures::Reconstruction3d rec_even;
	SingleParticle2dx::DataStructures::Reconstruction3d rec_odd;
	
	SingleParticle2dx::DataStructures::ParticleContainer cont_even;
	SingleParticle2dx::DataStructures::ParticleContainer cont_odd;
	SingleParticle2dx::DataStructures::ParticleContainer::splitContainerMemSaving(cont, cont_even, cont_odd);
	
	rec_even.generateInitialModel(cont_even);
	rec_odd.generateInitialModel(cont_odd);
	
	std::string fsc_file_name = config->getContainerName() + "/Div_output/fsc.dat";
	std::pair<std::vector<float>, std::vector<float> > fsc = SingleParticle2dx::DataStructures::ParticleContainer::calculateFSC( rec_even, rec_odd, fsc_file_name);
	
	int res_radius = SingleParticle2dx::Utilities::UtilityFunctions::getGoldStandardFSCRadius(fsc);
	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Gold FSC Radius: " + SingleParticle2dx::Utilities::StringFunctions::TtoString(res_radius), 1);

	return 0;

}
