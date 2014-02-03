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
	
	int n = config->getParticleSize();

	typedef boost::archive::binary_iarchive archive_in_type;	
	
	std::vector<std::string> image_dirs = SingleParticle2dx::ConfigContainer::Instance()->getImageDirectories();
	
	std::cout << "project dir:" << config->getProjectDirectory() << std::endl;
	
//	#ifdef USE_CILK
//		cilk_for (int i=0; i<config->getNumberOfImages(); i++)
//	#else
		for (int i=0; i<config->getNumberOfImages(); i++)
//	#endif
	{
		SingleParticle2dx::DataStructures::ParticleContainer cont_dummy;
		SingleParticle2dx::DataStructures::ParticleContainer cont;
		SingleParticle2dx::DataStructures::ParticleContainer cont2;
		SingleParticle2dx::DataStructures::PickingDiagnostics dia;
		
		std::string working_dir = (config->getProjectDirectory() + image_dirs[i]);
		
		std::vector<std::string> split_vector;
		boost::split( split_vector, working_dir, boost::is_any_of("/") ); 
		std::string filename_core = split_vector.back();
		std::string cont_name = filename_core + "/cont.bin";
		
		std::string cont_name_in = "CONS_CONT/" + filename_core + ".bin";		
		SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(cont_name_in, cont, true);
	//	cont_name_in = "CONS_CONT/" + filename_core + "_dual.bin";
	//	SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(cont_name_in, cont2, true);
		
		
		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput(boost::lexical_cast<std::string>(cont.getNumberOfParticles()) + " particles picked", 1);
		
		if(cont.getNumberOfParticles() < 10)
		{
			continue;
		}

		SingleParticle2dx::real_array2d_type real_data( boost::extents[n][n] );
		cont.getAverage(real_data);
		SingleParticle2dx::DataStructures::Orientation o_av = cont(0).getInitialOrientation();
			
		//	std::string config_name = (working_dir + "/" + "2dx_image.cfg");
		//	SingleParticle2dx::Utilities::ImageConfigReader image_config (config_name);
			
		//	float tltaxis = image_config.getConfigElement("TLTAXIS")[0];
		//	float tltang = image_config.getConfigElement("TLTANG")[0];
		//	float taxa = image_config.getConfigElement("TAXA")[0];
	
		//	SingleParticle2dx::DataStructures::Orientation o_av(tltaxis, tltang, taxa);
		cont.clear();
		SingleParticle2dx::DataStructures::GlobalParticleInformation i_av;
		SingleParticle2dx::DataStructures::Particle p_av(n, n, o_av, i_av);
		p_av.setOrientation(o_av);
		p_av.setFourierSpaceData(&real_data);
		p_av.writeToFile(filename_core + "/consAvg.mrc");
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(filename_core + "/consAvg.mrc", "Avg " + image_dirs[i], config->getScriptName(), false, false);
		cont.addParticle(p_av);
		
		
		SingleParticle2dx::DataStructures::Reconstruction3d rec3d_full(n, n, n);
		
		rec3d_full.generateInitialModel(cont);
		rec3d_full.applyLowPassFilter();
		
		SingleParticle2dx::DataStructures::Projection2d p(n,n);
		SingleParticle2dx::DataStructures::Orientation o;
		rec3d_full.updateReconstruction(cont_dummy);
		rec3d_full.calculateProjection(o, p);
		
		p.writeToFile(filename_core + "/consProj.mrc");
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(filename_core + "/consProj.mrc", "Proj " + image_dirs[i], config->getScriptName(), true, false);
		
		o = cont(0).getInitialOrientation();
		rec3d_full.calculateProjection(o, p);
		p.writeToFile(filename_core + "/consProj_init.mrc");
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(filename_core + "/consProj_init.mrc", "Proj along dummy" + image_dirs[i], config->getScriptName(), true, false);
		
		
//		cont2.getAverage(real_data);
//		o_av = cont2(0).getInitialOrientation();
//		cont2.clear();
//		SingleParticle2dx::DataStructures::Particle p_av2(n, n, o_av, i_av);
//		p_av2.setOrientation(o_av);
//		p_av2.setFourierSpaceData(&real_data);
//		p_av2.writeToFile(filename_core + "/consAvg_dual.mrc");
//		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(filename_core + "/consAvg_dual.mrc", "Avg (dual) " + image_dirs[i] , config->getScriptName(), false, false);
//		cont2.addParticle(p_av2);
		
		
		
//		rec3d_full.generateInitialModel(cont2);
//		rec3d_full.applyLowPassFilter();
//		rec3d_full.updateReconstruction(cont_dummy);
//		rec3d_full.calculateProjection(o, p);
		
//		p.writeToFile(filename_core + "/consProj_dual.mrc");
//		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(filename_core + "/consProj_dual.mrc", "Proj (dual) " + image_dirs[i], config->getScriptName(), true, false);
		
		
		int proc = static_cast<int>(static_cast<float>(i+1)/static_cast<float>(config->getNumberOfImages())*100);
		SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( proc );
	}
	
	SingleParticle2dx::Utilities::UtilityFunctions::forceReload();
	
	return 0;
}
