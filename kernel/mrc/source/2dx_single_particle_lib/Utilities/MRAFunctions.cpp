#include <boost/filesystem.hpp>
#include <boost/algorithm/string/predicate.hpp>

#include "MRAFunctions.hpp"
#include "../Config.hpp"


void SingleParticle2dx::Utilities::MRAFunctions::calculateMRAReconstruction ( SingleParticle2dx::DataStructures::ParticleContainer& cont, std::vector<SingleParticle2dx::DataStructures::Reconstruction3d>& rec_vec)
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	size_type n = config->getParticleSize();
	
	std::vector<SingleParticle2dx::DataStructures::ParticleContainer> cont_vec;
	SingleParticle2dx::DataStructures::ParticleContainer::splitContainerAccordingToClasses(cont, cont_vec);
	
	for(size_type i=0; i<static_cast<size_type>(cont_vec.size()); i++)
	{
		std::cout << ":class " << i << std::endl;
		cont_vec[i].printClassMembers();
		
		rec_vec[i].setBackprojectionMethod ( config->getBackprojectionMethod() );
	}
	
	#pragma omp parallel for
	for(size_type i=0; i<static_cast<size_type>(cont_vec.size()); i++)
	{
		rec_vec[i].generateInitialModel(cont_vec[i]);
	}
}
