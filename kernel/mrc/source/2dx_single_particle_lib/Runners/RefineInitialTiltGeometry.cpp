#include <fstream>

#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>

#include "../2dxSingleParticle.hpp"
#include "../eman2/libEM/emutil.h"


void setWeightAccordingToCCValue(SingleParticle2dx::DataStructures::ParticleContainer& cont)
{
	std::vector<float> cc_vec;
	for(int i=0; i<cont.getNumberOfParticles(); i++)
	{
		cc_vec.push_back(cont(i).getSimMeasure());
	}
	float max_cc = *std::max_element(cc_vec.begin(),cc_vec.end());
	std::cout << ":Max CC value:" << max_cc << std::endl;
	
	for(int i=0; i<cont.getNumberOfParticles(); i++)
	{
		cont(i).setWeight(1.0*max_cc/cont(i).getSimMeasure());
	}
}


int shiftAlign(SingleParticle2dx::DataStructures::ParticleContainer& cont, std::vector<std::string>& names, SingleParticle2dx::DataStructures::Reconstruction3d& rec)
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	int n = config->getParticleSize();
	
	int s_max = config->getCrossCorrelationWindowSize()/2;
	
	std::string cont_path = config->getContainerFilePath();
	std::string cont_bin_folder = cont_path + "/ParticleContainers/cont_part4";
	
	std::string stat_file = cont_path + "/ContainerStatsTXT/cont_stat_refinit.txt";
	if(boost::filesystem::exists(stat_file))
	{
		boost::filesystem::remove_all(stat_file);
	}
	
	int delete_count = 0;
	
	#pragma omp parallel for schedule(dynamic,1) reduction(+:delete_count)
	for(int i=0; i<cont.getNumberOfParticles(); i++)
	{
		SingleParticle2dx::DataStructures::Projection2d proj(n,n);
		rec.calculateProjection(cont(i).getNewOrientation(), proj);
		
		SingleParticle2dx::DataStructures::ParticleContainer cont_local;
		SingleParticle2dx::DataStructures::ParticleContainer cont_ave_local;
		SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(names[i], cont_local, true);
		
		cont_local.applyMaskToContainer();
		cont_local.findNeighbors(1);
		
		SingleParticle2dx::Utilities::UtilityFunctions::alignInplaneShift(cont_local, proj, true, 0.0, false);
		
		int xs, ys;
		std::vector<int> index_to_delete;
		
		for(int j=0; j<cont_local.getNumberOfParticles(); j++)
		{
			xs = fabs(cont_local(j).getParticleShift().getShiftX());
			ys = fabs(cont_local(j).getParticleShift().getShiftY());
			
			if( (xs >= s_max) || (ys >= s_max) )
			{
				index_to_delete.push_back(j);
			}
		}
		
		std::cout << "::delete = " << index_to_delete.size() << std::endl;
		delete_count += index_to_delete.size();
		
		std::vector<std::string> split_vector;
		SingleParticle2dx::Utilities::StringFunctions::splitString( split_vector, names[i], std::string("/") );
		std::string filename_core = split_vector.back();
		
		std::string folder_bin = cont_bin_folder + "/" + filename_core;
		std::cout << ":debug: " << folder_bin << std::endl;
		
	//	for(int j=static_cast<int>(index_to_delete.size()-1); j>0; j--)
	//	{
	//		cont_local.deleteParticle(j);
	//	}
		
		#pragma omp critical (write_stat_init_ref)
		{
			//cont_local.writeStatToFile(stat_file, true);
		}
		
	//	for(int j=0; j<cont_local.getNumberOfParticles(); j++)
	//	{
	//		cont_local(j).getGlobalParticleInformation().setPositionX(cont_local(j).getGlobalParticleInformation().getPositionX() + cont_local(j).getParticleShift().getShiftX());
	//		cont_local(j).getGlobalParticleInformation().setPositionY(cont_local(j).getGlobalParticleInformation().getPositionY() + cont_local(j).getParticleShift().getShiftY());
	//	}
		
	//	SingleParticle2dx::Utilities::UtilityFunctions::applyInplaneShift(cont_local);
		
		cont_local.setAllOrientations(cont(i).getNewOrientation());
		SingleParticle2dx::DataStructures::ParticleContainer::serializeContainerToDisk(cont_local, true, folder_bin);
		
		cont_local.generateAverageContainer(cont_ave_local, false, false);
		
		SingleParticle2dx::real_array2d_type real_data_2d( boost::extents[n][n] );
		cont_ave_local(0).getRealSpaceData(&real_data_2d);
		
		cont(i).setFourierSpaceData(&real_data_2d);
	}
	return delete_count;
}



int main ()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	std::string cont_path = config->getContainerFilePath();
	std::string cont_bin_folder;

	if(config->getRestartRef())
	{
		cont_bin_folder = cont_path + "/ParticleContainers/cont_part4";
	}
	else
	{
		cont_bin_folder = cont_path + "/ParticleContainers/cont_part2";
	}		
	
	std::vector<std::string> folder_content;
	SingleParticle2dx::Utilities::UtilityFunctions::getContentOfFolder(cont_bin_folder, folder_content, ".bin");
	
	SingleParticle2dx::DataStructures::ParticleContainer cont_ave;

	std::map<int,std::string> reading_order;
	std::vector<string> containers;

	int np = 0;
	#pragma omp parallel for schedule(dynamic,1) reduction(+:np)
	for(int i=0; i< static_cast<int>(folder_content.size()); i++)
	{
		SingleParticle2dx::DataStructures::ParticleContainer cont;
		SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(folder_content[i], cont, true);
		
		cont.applyMaskToContainer();
		
		np += cont.getNumberOfParticles();
		
		SingleParticle2dx::DataStructures::ParticleContainer cont_ave_local;
		cont.generateAverageContainer(cont_ave_local, false, false);
		
		SingleParticle2dx::DataStructures::Particle part2add = cont_ave_local(0);
		part2add.setParticleNumber(i);
		
		SingleParticle2dx::Utilities::AverageWeighter ave_weighter(config->getAveWeightMode(), cont.getNumberOfParticles(), part2add.getNewOrientation().getTLTANG());
		part2add.setWeight(ave_weighter.getWeight());
		
		#pragma omp critical (add_reading_order)
		{
			reading_order[part2add.getGlobalParticleInformation().getImageNumber()] = folder_content[i];
			cont_ave.addParticle(part2add);
			containers.push_back(folder_content[i]);
		}
	}
	
	int n = SingleParticle2dx::ConfigContainer::Instance()->getParticleSize();
	std::string rec_folder = config->getContainerName() + "/Rec_3d";
	
	std::cout << np << std::endl;
	
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d_ave(n, n, n);
	
	
	if(config->getRefMode() == 0)
	{
		rec3d_ave.generateInitialModel(cont_ave);
		
	}
	else if(config->getRefMode() == 1)
	{
		rec3d_ave.readFromFile(config->getContainerFilePath() + "/Rec_3d/Init3DFromMRC.map");
	}
	
	rec3d_ave.writeToFile( rec_folder + "/rec_initial_before_refine1.map" );
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput( rec_folder + "/rec_initial_before_refine1.map", "MAP: Initial 3D reconstruction", config->getScriptName(), true);
	SingleParticle2dx::Utilities::ConvAnalyzer conv_analyzer(config->getMinAngleChange(), 3);
	
	int shake_count = 1;
	
	std::string conv_file_name = "SCRATCH/2dx_SP_convergence.txt";
	SingleParticle2dx::Utilities::UtilityFunctions::removeFileIfExists(conv_file_name);
	
	SingleParticle2dx::real_array3d_type real_data(boost::extents[n][n][n]);
	SingleParticle2dx::real_array3d_type real_data_old(boost::extents[n][n][n]);
	

	for (int i=0; i<config->getMaxIteration(); i++)
	{
		rec3d_ave.getRealSpaceData(real_data_old);
		
		rec3d_ave.updateReconstruction(cont_ave, false, false);
	//	setWeightAccordingToCCValue(cont_ave);
		
		int delete_count = shiftAlign(cont_ave, containers, rec3d_ave);
		SingleParticle2dx::Utilities::UtilityFunctions::generateVariableOutput("partilces_deleted", delete_count, config->getScriptName());
		rec3d_ave.generateInitialModel(cont_ave);
		
		rec3d_ave.applyMask();
		rec3d_ave.applyLowPassFilter();
		
		rec3d_ave.getRealSpaceData(real_data);
		
		float dx = config->getModelKeep() / 100.0;
			
		for(int ii=0; ii<n; ii++)
		{
			for(int jj=0; jj<n; jj++)
			{
				for(int kk=0; kk<n; kk++)
				{
					real_data_old[ii][jj][kk] = dx*real_data_old[ii][jj][kk] + (1-dx)*real_data[ii][jj][kk];
				}
			}
		}
			
		rec3d_ave.setFourierSpaceData(real_data_old);
		
		SingleParticle2dx::Utilities::UtilityFunctions::generateVariableOutput("angular_change_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i), rec3d_ave.getLastAngleChange() / cont_ave.getNumberOfParticles(), config->getScriptName());
		
		conv_analyzer.addChange(rec3d_ave.getLastAngleChange() / cont_ave.getNumberOfParticles());
		SingleParticle2dx::Utilities::UtilityFunctions::writeConvergenceToFile(i+1, rec3d_ave.getLastAngleChange() / cont_ave.getNumberOfParticles(), conv_file_name);
		
		if (config->getKeepAll())
		{
			rec3d_ave.writeToFile( rec_folder + "/rec_initial_refine_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i) + ".map" );
			SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(rec_folder + "/rec_initial_refine_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i) + ".map", "MAP: refine " + SingleParticle2dx::Utilities::StringFunctions::TtoString(i), config->getScriptName(), false);
		}

		if ( conv_analyzer.isConverged() )
		{
			SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("CONVERGERD, increase search range", 1);
			break;
		}
	}
	
	rec3d_ave.writeToFile( rec_folder + "/rec_initial_after_refine1.map" );
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput( rec_folder + "/rec_initial_after_refine1.map", "MAP: Refined1 Initial 3D reconstruction", config->getScriptName(), true);
	
	
	int ceil_sqrt_np = ceil(sqrt(static_cast<float>(folder_content.size())));
	SingleParticle2dx::real_array2d_type all_data( boost::extents[2*ceil_sqrt_np*n][2*ceil_sqrt_np*n] );
	typedef SingleParticle2dx::real_array2d_type::index_range range;
	SingleParticle2dx::real_array2d_type real_data_2d( boost::extents[n][n] );

	std::string partAndProjFolder;
	
	partAndProjFolder = config->getContainerName() + "/Particles/part_and_proj_1";
	
	
	SingleParticle2dx::Utilities::UtilityFunctions::removeFolderIfExists(partAndProjFolder);
	boost::filesystem::create_directory(partAndProjFolder);

	for(int i=0; i<cont_ave.getNumberOfParticles(); i++)
	{
		cont_ave(i).getRealSpaceData(&real_data_2d);
		
		int index_x = i / ceil_sqrt_np;
		int index_y = i % ceil_sqrt_np;
		
		all_data[boost::indices[range(index_x*2*n,index_x*2*n+n)] [range(index_y*2*n,index_y*2*n+n)] ] = real_data_2d;	
		
		SingleParticle2dx::DataStructures::Orientation o = cont_ave(i).getNewOrientation();
		SingleParticle2dx::DataStructures::Projection2d p(n,n);
		rec3d_ave.calculateProjection(o, p);
		p.getRealSpaceData(&real_data_2d);
		all_data[boost::indices[range(index_x*2*n+n,index_x*2*n+(2*n))] [range(index_y*2*n+n,index_y*2*n+(2*n))] ] = real_data_2d;
		
		cont_ave(i).writeToFile(partAndProjFolder + "/image_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i) + "_part.mrc");
		p.writeToFile(partAndProjFolder + "/image_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i) + "_proj.mrc");
	}
	
	SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&all_data);
	
	for(int i=0; i<static_cast<int>(all_data.shape()[0]); i++)
	{
		for(int j=0; j<static_cast<int>(all_data.shape()[1]); j++)
		{
			if ( i%(2*n) == 0 )
			{
				all_data[j][i] = 10;
			}
			if ( j%(2*n) == 0 )
			{
				all_data[j][i] = 10;
			}
		}
	}
	
	SingleParticle2dx::Utilities::MRCFileIO::writeToMrc(&all_data, config->getContainerName() + "/Div_output/partANDproj_1.mrc");
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput( config->getContainerName() + "/Div_output/partANDproj_1.mrc", "MAP: Particles And Projections", config->getScriptName(), true, false);
	
	
	boost::shared_ptr<FILE> file;
	std::string file_name = config->getContainerName() + "/Div_output/ang_diff.txt";
	file.reset( fopen ( file_name.c_str(), "w" ), SingleParticle2dx::Utilities::CustomizedDeleter() );
	
	for(int i=0; i<cont_ave.getNumberOfParticles(); i++)
	{
		float diff_1 = cont_ave(i).getNewOrientation().getTLTAXIS() - cont_ave(i).getInitialOrientation().getTLTAXIS();
		float diff_2 = cont_ave(i).getNewOrientation().getTLTANG() - cont_ave(i).getInitialOrientation().getTLTANG();
		float diff_3 = cont_ave(i).getNewOrientation().getTAXA() - cont_ave(i).getInitialOrientation().getTAXA();
		fprintf(file.get(), "%i\t%f\t%f\t%f\n", i, diff_1, diff_2, diff_3);
	}
	
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(file_name, "Angular Differences", config->getScriptName(), false, false);
	
	
	file_name =  config->getContainerName() + "/Div_output/good_init_angles.txt";
	file.reset( fopen ( file_name.c_str(), "w" ), SingleParticle2dx::Utilities::CustomizedDeleter() );
	for(int i=0; i<cont_ave.getNumberOfParticles(); i++)
	{
		float ang1 = cont_ave(i).getNewOrientation().getTLTAXIS();
		float ang2 = cont_ave(i).getNewOrientation().getTLTANG();
		float ang3 = cont_ave(i).getNewOrientation().getTAXA() + 90;
		fprintf(file.get(), "%s\t%f\t%f\t%f\n", cont_ave(i).getGlobalParticleInformation().getImageName().c_str(), ang1, ang2, ang3);
	}
	
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(file_name, "Angular (refined)", config->getScriptName(), false, false);
		
	
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(partAndProjFolder, "Projection Matching output", config->getScriptName(), false, false);
	SingleParticle2dx::Utilities::UtilityFunctions::forceReload();
	
	return 0;
}
