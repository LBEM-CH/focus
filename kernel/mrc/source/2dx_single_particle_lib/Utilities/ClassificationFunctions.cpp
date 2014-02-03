#include "ClassificationFunctions.hpp"
#include "../Config.hpp"

#include <analyzer.h>


void SingleParticle2dx::Utilities::ClassificationFunctions::kMeans(SingleParticle2dx::DataStructures::ParticleContainer cont, size_type number_of_classes, std::vector<SingleParticle2dx::DataStructures::Particle>& result)
{
	std::cout << "entering kmeans" << std::endl;
	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	result.clear();
	
	EMAN::Dict params;
	params["verbose"] = 9; 					// Display progress if set, more detail with larger numbers (9 max)
	params["ncls"]    = number_of_classes;	// number of desired classes
	params["maxiter"] = 10;					// maximum number of iterations
	params["mininclass"] = 100;
                   
	EMAN::Analyzer* classifier = EMAN::Factory<EMAN::Analyzer>::get("kmeans");
	classifier->set_params(params);

	std::cout << "classifier setup done" << std::endl;

	size_type size = config->getParticleSize();
	float* float_data_2d;
	
	real_array2d_type rdata( boost::extents[size][size] );
	
	std::cout << "arrays setup" << std::endl;
	
	for(int i=0; i<cont.getNumberOfParticles(); i++)
	{
		std::cout << i << std::endl;
		float_data_2d = (float*) malloc(size*size * sizeof(float));
		EMAN::EMData* eman_data = new EMAN::EMData;
				
		std::cout << "local setup done" << std::endl;
		
		cont(i).getRealSpaceData(&rdata);
		
		std::cout << "got data" << std::endl;
		
		std::copy(rdata.origin(), rdata.origin()+rdata.num_elements(), float_data_2d );
		
		std::cout << "copy done" << std::endl;
		
		eman_data->set_data(float_data_2d, size, size, 1);
		
		std::cout << "data set" << std::endl;
		
		classifier->insert_image(eman_data);
		
		std::cout << "insert done" << std::endl;
	}
	
	std::cout << "classifier ready (analyze called)" << std::endl;
	
	std::vector<EMAN::EMData*> result_vec = classifier->analyze();
	
	std::cout << "eman result size: " << result_vec.size() << std::endl;
	
	std::cout << "analyze done" << std::endl;
	
	for(int i=0; i<static_cast<size_type>(result_vec.size()); i++)
	{
		EMAN::EMData* eman_result;
		eman_result = result_vec[i];
		
		size_type size2d = size * size;
		std::copy(eman_result->get_data(), eman_result->get_data()+size2d, rdata.origin() );
	
		SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&rdata);

		SingleParticle2dx::DataStructures::Particle p(size, size);
		p.setFourierSpaceData(&rdata);
		result.push_back(p);
	}
}
