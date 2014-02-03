#include <fstream>

#include <boost/filesystem.hpp>

#include "../2dxSingleParticle.hpp"

int main()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	int n = config->getParticleSize();
	std::string cont_path = config->getContainerFilePath();
	std::string cont_bin_folder = cont_path + "/ParticleContainers/cont_part2";
	
	std::vector<std::string> folder_content;
	SingleParticle2dx::Utilities::UtilityFunctions::getContentOfFolder(cont_bin_folder, folder_content, ".bin");
	
	SingleParticle2dx::DataStructures::ParticleContainer cont_ave;
	int counter = 0;
	
	int ceil_sqrt_np = ceil(sqrt(static_cast<float>(folder_content.size())));
	SingleParticle2dx::real_array2d_type all_data( boost::extents[ceil_sqrt_np*n][ceil_sqrt_np*n] );
	typedef SingleParticle2dx::real_array2d_type::index_range range;
	
	std::string cont_stat_file = cont_path + "/ContainerStatsTXT/cont_stat_analyze.txt";
	if(boost::filesystem::exists(cont_stat_file))
	{
		boost::filesystem::remove(cont_stat_file);
	}
	
	int np = 0;
	#pragma omp parallel for schedule(dynamic,1) reduction(+:np)
	for(int i=0; i< static_cast<int>(folder_content.size()); i++)
	{
		SingleParticle2dx::DataStructures::ParticleContainer cont;
		SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(folder_content[i], cont, true);
		np += cont.getNumberOfParticles();
		
		SingleParticle2dx::DataStructures::ParticleContainer cont_ave_local;
		cont.generateAverageContainer(cont_ave_local, true, false);

		SingleParticle2dx::DataStructures::Particle part2add = cont_ave_local(0);
		
		SingleParticle2dx::Utilities::AverageWeighter ave_weighter(config->getAveWeightMode(), cont.getNumberOfParticles(), part2add.getNewOrientation().getTLTANG());
		part2add.setWeight(ave_weighter.getWeight());
		cont_ave.addParticle(part2add);
		
		#pragma omp critical(output_during_cont_analyze)
		{
			cont.writeStatToFile(cont_stat_file, true);
			int proc = static_cast<int>( 100*(counter+1.0)/folder_content.size());
			SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( proc );
			counter++;
		}
		
		SingleParticle2dx::real_array2d_type real_data( boost::extents[n][n] );
		cont_ave_local(0).getRealSpaceData(&real_data);
		int index_x = i / ceil_sqrt_np;
		int index_y = i % ceil_sqrt_np;
		
		all_data[boost::indices[range(index_x*n,index_x*n+n)] [range(index_y*n,index_y*n+n)] ] = real_data;	
	}
	
	cont_ave.setParticleNumbers();
	
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d_ave(n, n, n);
	rec3d_ave.generateInitialModel(cont_ave);
	rec3d_ave.applyMask();
	rec3d_ave.applyLowPassFilter();
	
	
	rec3d_ave.writeToFile(config->getContainerName() + "/Rec_3d/rec_initial.map" );
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(config->getContainerName() + "/Rec_3d/rec_initial.map", "MAP: Initial 3D reconstruction", config->getScriptName(), true, false);
	
	SingleParticle2dx::DataStructures::Orientation o_topview(0,0,0);
	SingleParticle2dx::DataStructures::Projection2d top_proj(n,n);
	SingleParticle2dx::DataStructures::ParticleContainer c_dummy;
	
	rec3d_ave.forceProjectionPreparation(c_dummy);
	rec3d_ave.calculateProjection(o_topview, top_proj);
	top_proj.writeToFile(config->getContainerName() + "/Div_output/topview_proj.mrc");
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(config->getContainerName() + "/Div_output/topview_proj.mrc", "MAP: TopView", config->getScriptName(), false, false);
	
	SingleParticle2dx::Utilities::MRCFileIO::writeToMrc(&all_data, config->getContainerName() + "/Div_output/particles.mrc");
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(config->getContainerName() + "/Div_output/particles.mrc", "MAP: Particles", config->getScriptName(), true, false);
	
	SingleParticle2dx::Utilities::UtilityFunctions::writeAveContainerStatToFile(cont_ave, cont_path + "/ContainerStatsTXT/picking_stat.txt");
	
	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Number of particles: " + SingleParticle2dx::Utilities::StringFunctions::TtoString(np), 1);	
	SingleParticle2dx::Utilities::UtilityFunctions::generateVariableOutput("Particle_Number", np, config->getScriptName(), false);
	
	SingleParticle2dx::Utilities::UtilityFunctions::generateVariableOutput("Images", folder_content.size(), config->getScriptName(), false);
	
	SingleParticle2dx::Utilities::UtilityFunctions::forceReload();
	
	return 0;
}
