#include <boost/program_options.hpp>
#include <boost/algorithm/string.hpp>

#include "../2dxSingleParticle.hpp"


int main (int argc, char const *argv[])
{
	namespace po = boost::program_options;
	
	std::string folder_name;
	std::string filename_ave;
	int number;
	
	po::options_description desc("2dx Movie Processing Usage");
	desc.add_options()
	    ("help", "produce help message")
	    ("folder", po::value<std::string>(&folder_name), "Folder name containing the images")
		("number", po::value<int>(&number), "Number of frames")
		("average", po::value<std::string>(&filename_ave), "Resulting average")
	;
	
	po::variables_map vm;
	po::store(po::parse_command_line(argc, argv, desc), vm);
	po::notify(vm);    

	if ( !vm.count("folder") || !vm.count("average") || vm.count("help") )
	{
		cout << desc << "\n";
	    return 1;
	}
	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	config->setParticleSize(4096);
	
	int size = config->getParticleSize();
	config->setParticleMaskingMethod(1);
	std::vector<std::string> images;
	
	SingleParticle2dx::Utilities::UtilityFunctions::getContentOfFolder(folder_name, images);
	std::sort(images.begin(), images.end());
		
	for (std::vector<std::string>::iterator it = images.begin() ; it != images.end(); ++it)
	{
		if (boost::iequals(*it, filename_ave))
		{
			images.erase(it);
			break;
		}
	}
	
	for(int i=0; i<static_cast<int>(images.size()); i++)
	{
		std::cout << "file found " << images[i] << std::endl;
	}
	
	SingleParticle2dx::DataStructures::ParticleContainer cont;
	SingleParticle2dx::DataStructures::Orientation ori;
	SingleParticle2dx::DataStructures::GlobalParticleInformation info;
		
	if ( !vm.count("number"))
	{
		number = static_cast<int>(images.size());
	}
	
	int number_of_images_incharge = std::min(number, static_cast<int>(images.size()));
	
	#pragma omp parallel for schedule(dynamic,1)
	for(int i=0; i<number_of_images_incharge; i++)
	{
		SingleParticle2dx::real_array2d_type input_image(boost::extents[size][size]);
		SingleParticle2dx::Utilities::MRCFileIO::readFromMrc(images[i], &input_image);
		SingleParticle2dx::DataStructures::Particle p(size, size, ori, info);
		p.setFourierSpaceData(&input_image);
		cont.addParticle(p);
	}
	
	std::cout << "done loading" << std::endl;
	
	SingleParticle2dx::real_array2d_type ave_image(boost::extents[size][size]);
		SingleParticle2dx::Utilities::UtilityFunctions::calculateAverage(cont, ave_image, false);
	
	SingleParticle2dx::DataStructures::Particle ave_p(size, size, ori, info);
	ave_p.setFourierSpaceData(&ave_image);
	ave_p.flipXYAxis();
	ave_p.writeToFile(filename_ave);
	
	return 0;
}
