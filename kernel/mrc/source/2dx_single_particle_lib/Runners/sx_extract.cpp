#include <boost/program_options.hpp>
#include <boost/algorithm/string.hpp>

#include "../2dxSingleParticle.hpp"


int main (int argc, char const *argv[])
{
	namespace po = boost::program_options;
	
	std::string cont_name_in;
	std::string cont_name_out;
	
	po::options_description desc("2dx Extract SX Data");
	desc.add_options()
	    ("help", "produce help message")
	    ("folder_cont", po::value<std::string>(&cont_name_in), "Container Folder")
	    ("folder_out", po::value<std::string>(&cont_name_out), "Output folder")
	;
	
	po::variables_map vm;
	po::store(po::parse_command_line(argc, argv, desc), vm);
	po::notify(vm);    

	if ( !vm.count("folder_cont") || !vm.count("folder_out") || vm.count("help") )
	{
		cout << desc << "\n";
	    return 1;
	}
	
	
	std::vector<std::string> folder_content;
	SingleParticle2dx::Utilities::UtilityFunctions::getContentOfFolder(cont_name_in, folder_content, ".bin");
	
	for(int i=0; i< static_cast<int>(folder_content.size()); i++)
	{
		SingleParticle2dx::DataStructures::ParticleContainer cont;
		SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(folder_content[i], cont, true);
		
		std::cout << cont(0).getGlobalParticleInformation().getImageName() << std::endl;
		std::string image_name = cont(0).getGlobalParticleInformation().getImageName();
	
		boost::shared_ptr<FILE> file;
		std::string file_name = cont_name_out + "/" + image_name + "_ctf" + ".txt";
		file.reset( fopen ( file_name.c_str(), "w" ), SingleParticle2dx::Utilities::CustomizedDeleter() );
	
		for(int i=0; i<cont.getNumberOfParticles(); i++)
		{
			fprintf(file.get(), "%s\n", cont(i).getCTFInfo().getDataString().c_str());
		}	
	
		file_name = file_name = cont_name_out + "/" + image_name + "_pos" + ".txt";
		file.reset( fopen ( file_name.c_str(), "w" ), SingleParticle2dx::Utilities::CustomizedDeleter() );
		for(int i=0; i<cont.getNumberOfParticles(); i++)
		{
			fprintf(file.get(), "%i\t%i\n", cont(i).getGlobalParticleInformation().getPositionX(), cont(i).getGlobalParticleInformation().getPositionY());
		}
	
		file_name = file_name = cont_name_out + "/" + image_name + "_align" + ".txt";
		file.reset( fopen ( file_name.c_str(), "w" ), SingleParticle2dx::Utilities::CustomizedDeleter() );
		for(int i=0; i<cont.getNumberOfParticles(); i++)
		{
			fprintf(file.get(), "%f\t%f\t%f\t%f\t%f\n", cont(i).getNewOrientation().getTAXA(), cont(i).getNewOrientation().getTLTANG(), cont(i).getNewOrientation().getTLTAXIS() + 90.0, cont(i).getParticleShift().getShiftX(), cont(i).getParticleShift().getShiftY());
		}	
	}
	
	
	
	
	
	
	return 0;
}
