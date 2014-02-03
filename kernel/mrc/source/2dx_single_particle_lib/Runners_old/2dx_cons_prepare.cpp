#include "../2dxSingleParticle.hpp"

#include <fstream>

#include <boost/lexical_cast.hpp>

#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/split.hpp>


#ifdef USE_CILK
	#include <cilk/cilk.h>
#endif


int main()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	if ( boost::filesystem::exists("CONS_CONT") )
	{
		boost::filesystem::remove_all("CONS_CONT");
	}
	boost::filesystem::create_directory("CONS_CONT");
	
	int n = config->getParticleSize();

	typedef boost::archive::binary_oarchive archive_out_type;
		
	std::vector<std::string> image_dirs = SingleParticle2dx::ConfigContainer::Instance()->getImageDirectories();
	
	std::cout << "project dir:" << config->getProjectDirectory() << std::endl;
	
	SingleParticle2dx::DataStructures::ParticleContainer cont_dummy;
	
	int counter = 0;
	
//	#ifdef USE_CILK
//		cilk_for (int i=0; i<config->getNumberOfImages(); i++)
//	#else
		#pragma omp parallel for schedule(dynamic,1)// num_threads(1)
		for (int i=0; i<config->getNumberOfImages(); i++)
//	#endif
	{
		SingleParticle2dx::DataStructures::ParticleContainer cont;
		SingleParticle2dx::DataStructures::ParticleContainer cont2;
		SingleParticle2dx::DataStructures::PickingDiagnostics dia;
		
		std::string working_dir = (config->getProjectDirectory() + image_dirs[i]);
		
		std::vector<std::string> split_vector;
		boost::split( split_vector, working_dir, boost::is_any_of("/") ); 
		std::string filename_core = split_vector.back();
		std::string cont_name = filename_core + "/cont.bin";
		
		int good_particles;
		int good_particles2;
		
//		#ifdef USE_CILK
//			good_particles = cilk_spawn SingleParticle2dx::DataStructures::ParticleContainer::pickParticlesFromCCProfile(working_dir, cont, dia, 2*i, false);
//			good_particles2 = SingleParticle2dx::DataStructures::ParticleContainer::pickParticlesFromCCProfile(working_dir, cont2, dia, 2*i+1, true);
//			cilk_sync;
//		#else
			good_particles = SingleParticle2dx::DataStructures::ParticleContainer::pickParticlesFromCCProfile(working_dir, cont, dia, 2*i, false);
//			good_particles2 = SingleParticle2dx::DataStructures::ParticleContainer::pickParticlesFromCCProfile(working_dir, cont2, dia, 2*i+1, true);
//		#endif
		
		SingleParticle2dx::Utilities::UtilityFunctions::generateVariableOutput(filename_core, good_particles, config->getScriptName(), false);
//		SingleParticle2dx::Utilities::UtilityFunctions::generateVariableOutput(filename_core, good_particles2, config->getScriptName(), false);
			
		std::string cont_name_out = "CONS_CONT/" + filename_core + ".bin";
		SingleParticle2dx::DataStructures::ParticleContainer::serializeContainerToDisk(cont, true, cont_name_out);
//		cont_name_out = "CONS_CONT/" + filename_core + "_dual.bin";		
//		SingleParticle2dx::DataStructures::ParticleContainer::serializeContainerToDisk(cont2, true, cont_name_out);
		
		#pragma omp critical (update_progress_for_picking)
		{
			int proc = static_cast<int>(static_cast<float>(counter+1)/static_cast<float>(config->getNumberOfImages())*100);
			SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( proc );
			counter++;
		}
	}
	
	SingleParticle2dx::Utilities::UtilityFunctions::forceReload();
	
	return 0;
}
