#include "../2dxSingleParticle.hpp"

#include <fstream>

int main ()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	std::string cont_path = config->getContainerFilePath();
	std::cout << "path= " << cont_path << std::endl;
	
	SingleParticle2dx::Utilities::UtilityFunctions::removeFolderIfExists(cont_path);
	SingleParticle2dx::Utilities::UtilityFunctions::createContainerFolder(cont_path);
	
	return 0;
}
