#include "../2dxSingleParticle.hpp"

#include <fstream>

#include <boost/lexical_cast.hpp>

#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>


int main ()
{
	typedef boost::archive::binary_iarchive archive_in_type;
	typedef boost::archive::binary_oarchive archive_out_type;
	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	std::string cont_path = config->getContainerFilePath();
	
	SingleParticle2dx::DataStructures::ParticleContainer cont;
	
	SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( 10 );
	
	std::string cont_bin_file = cont_path + "/ParticleContainers/shift_corrected.bin";
	SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(cont_bin_file, cont, true);
	
	cont_bin_file = cont_path + "/FingerPrintContainers/initial_geometry_corrected2.bin";
	SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(cont_bin_file, cont, false);
	
	SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( 50 );

	cont.setParticleNumbers();	
	std::cout << cont.getNumberOfParticles() << std::endl;
	
	SingleParticle2dx::DataStructures::ParticleContainer cont_ave;
	cont.generateAverageContainer(cont_ave, false);
	
	std::string out_file_name = config->getOrientFile();
	std::cout << ":outfile: " << out_file_name << std::endl;
	SingleParticle2dx::DataStructures::ParticleContainer::serializeContainerToDisk(cont_ave, true, out_file_name);
	
	SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( 100 );
	
	return 0;
}
