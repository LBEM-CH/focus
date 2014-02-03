#include <boost/program_options.hpp>

#include "../2dxSingleParticle.hpp"


int main(int argc, char* argv[])
{
	namespace po = boost::program_options;
	
	std::string cont_name;
	
	
	po::options_description desc("2dx Kmeans test");
	desc.add_options()
	    ("help", "produce help message")
	    ("folder", po::value<std::string>(&cont_name), "Container to analyze")
	;
	
	po::variables_map vm;
	po::store(po::parse_command_line(argc, argv, desc), vm);
	po::notify(vm);    

	if ( !vm.count("folder") || vm.count("help") )
	{
		cout << desc << "\n";
	    return 1;
	}
	
	SingleParticle2dx::DataStructures::ParticleContainer cont;
	SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(cont_name, cont, true);
	std::vector<SingleParticle2dx::DataStructures::Particle> result;
	SingleParticle2dx::Utilities::ClassificationFunctions::kMeans(cont, 2, result);
	
	std::cout << "result_size: " << result.size() << std::endl;
	
	for(int i=0; i<static_cast<int>(result.size()); i++)
	{
		result[i].writeToFile("part_test_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i) + ".mrc");
	}
	
	return 0;
}
