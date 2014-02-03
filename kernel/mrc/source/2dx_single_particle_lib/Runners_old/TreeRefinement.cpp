#include "../2dxSingleParticle.hpp"

#include <fstream>

#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>



void applyWeight(SingleParticle2dx::DataStructures::ParticleContainer& cont_tree, int number_of_images, std::map<int,float>& cc_map)
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	boost::shared_ptr<FILE> file;
	std::string file_name = config->getContainerFilePath() + "/Div_output/cc_ang_t2.txt";
	file.reset( fopen ( file_name.c_str(), "w" ), SingleParticle2dx::Utilities::CustomizedDeleter() );
	
	for(int i=0; i<number_of_images; i++)
	{
		float tiltang = 0;
		float local_cc_sum = 0;
		for(int j=0; j<cont_tree.getNumberOfParticles(); j++)
		{
			if(cont_tree(j).getGlobalParticleInformation().getImageNumber() == i)
			{
				local_cc_sum += cont_tree(j).getSimMeasure();
				tiltang = cont_tree(j).getNewOrientation().getTLTANG();
			}
		}
		
		if(tiltang > 90)
		{
			tiltang -= 180;
		}
		tiltang = fabs(tiltang);
		fprintf(file.get(), "%f\t%f\n", tiltang, local_cc_sum);
		
		float multiplier = cc_map[i] / local_cc_sum;
		
		for(int j=0; j<cont_tree.getNumberOfParticles(); j++)
		{
			if(cont_tree(j).getGlobalParticleInformation().getImageNumber() == i)
			{
				//cont_tree(j).setWeight( multiplier * cont_tree(j).getWeight() );
				//cont_tree(j).setWeight( cont_tree(j).getWeight() );
			}
		}
	}
}


int getNumberofNodes(int level)
{
	int tmp = powf(2,level-1);
	return (tmp*tmp);
}


int determineTreeNodeNumber(SingleParticle2dx::DataStructures::Particle& part, int level)
{
	int width = part.getGlobalParticleInformation().getImageWidth();
	int node_width = width/ powf(2,level-1);
	
	int posx = part.getGlobalParticleInformation().getPositionX();
	int posy = part.getGlobalParticleInformation().getPositionY();
	
	int index_x = posx / node_width;
	int index_y = posy / node_width;
	
	int node_number = index_y * powf(2,level-1) + index_x;
	
	return node_number;
}


void setupFiles(std::string& cont_bin_folder, std::string& cont_bin_folder_fp, std::string& init_rec_file, std::string& init_model_out_file, std::string& cont_stat_file_name, std::string& tree_cont_folder, std::string& tree_conv_volume_corename, std::string& final_map_output, std::string& tree_refined_container, std::string& tree_refined_stat)
{	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	std::string cont_path = config->getContainerFilePath();
	int tree_level = config->getTreeLevel();

	switch (tree_level)
	{
		case 2:
			cont_bin_folder = cont_path + "/ParticleContainers/cont_part3";
			cont_bin_folder_fp = cont_path + "/FingerPrintContainers/cont_part3/";
			init_rec_file = config->getContainerName() + "/Rec_3d/rec_initial_after_refine2.map";
			init_model_out_file = config->getContainerName() + "/Rec_3d/init_tree_2.map";
			cont_stat_file_name = cont_path + "/Div_output/tree_shifts_t2.txt";
			tree_cont_folder = config->getContainerName() + "/Div_output/cont_t2";
			tree_conv_volume_corename = config->getContainerName() + "/Rec_3d/t2_";
			final_map_output = config->getContainerName() + "/Rec_3d/rec_initial_after_t2.map";
			tree_refined_container = cont_path + "/FingerPrintContainers/cont_t2/";
			tree_refined_stat = cont_path + "/ContainerStatsTXT/tree_refine_t2.txt";
			break;
		case 3:
			cont_bin_folder = cont_path + "/ParticleContainers/cont_part3";
			cont_bin_folder_fp = cont_path + "/FingerPrintContainers/cont_part3/";
			//cont_bin_folder_fp = cont_path + "/FingerPrintContainers/cont_t2/";
			init_rec_file =  config->getContainerName() + "/Rec_3d/rec_initial_after_t2.map";
			init_model_out_file = config->getContainerName() + "/Rec_3d/init_tree_3.map";
			cont_stat_file_name = cont_path + "/Div_output/tree_shifts_t3.txt";
			tree_cont_folder = config->getContainerName() + "/Div_output/cont_t3";
			tree_conv_volume_corename = config->getContainerName() + "/Rec_3d/t3_";
			final_map_output = config->getContainerName() + "/Rec_3d/rec_initial_after_t3.map";
			tree_refined_container = cont_path + "/FingerPrintContainers/cont_t3/";
			tree_refined_stat = cont_path + "/ContainerStatsTXT/tree_refine_t3.txt";
			break;
		case 4:
			cont_bin_folder = cont_path + "/ParticleContainers/cont_part3";
			cont_bin_folder_fp = cont_path + "/FingerPrintContainers/cont_part3/";
			//cont_bin_folder_fp = cont_path + "/FingerPrintContainers/cont_t3/";
			init_rec_file =  config->getContainerName() + "/Rec_3d/rec_initial_after_t3.map";
			init_model_out_file = config->getContainerName() + "/Rec_3d/init_tree_4.map";
			cont_stat_file_name = cont_path + "/Div_output/tree_shifts_t4.txt";
			tree_cont_folder = config->getContainerName() + "/Div_output/cont_t4";
			tree_conv_volume_corename = config->getContainerName() + "/Rec_3d/t4_";
			final_map_output = config->getContainerName() + "/Rec_3d/rec_initial_after_t4.map";
			tree_refined_container = cont_path + "/FingerPrintContainers/cont_t4/";
			tree_refined_stat = cont_path + "/ContainerStatsTXT/tree_refine_t4.txt";
			break;
		default:
			std::cout << "::Unsupported tree level" << std::endl;
			throw std::runtime_error("Bad operation");
			break;
	}
}


int main (int argc, char const *argv[])
{
	int mode;
	namespace po = boost::program_options;
	
	po::options_description desc("2dx Init TiltGeo Refinement");
	desc.add_options()
	    ("help", "produce help message")
	    ("mode", po::value<int>(&mode), "sirst (1) or second (2) refinement")
	;
	
	po::variables_map vm;
	po::store(po::parse_command_line(argc, argv, desc), vm);
	po::notify(vm);    

	if ( !vm.count("mode") )
	{
		cout << desc << "\n";
	    return 1;
	}
	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	std::string cont_bin_folder;
	std::string cont_bin_folder_fp;
	std::string init_rec_file;
	std::string init_model_out_file;
	std::string cont_stat_file_name;
	std::string tree_cont_folder;
	std::string tree_conv_volume_corename;
	std::string final_map_output;
	std::string tree_refined_container;
	std::string tree_refined_stat;
	
	setupFiles(cont_bin_folder, cont_bin_folder_fp, init_rec_file, init_model_out_file, cont_stat_file_name, tree_cont_folder, tree_conv_volume_corename, final_map_output, tree_refined_container, tree_refined_stat);
	
	int n = config->getParticleSize();
	int tree_level = config->getTreeLevel();
	
	SingleParticle2dx::DataStructures::Reconstruction3d rec_3d(n,n,n);
	rec_3d.setMinimal(false);
	rec_3d.readFromFile(init_rec_file);
	rec_3d.applyMask();
	rec_3d.applyLowPassFilter();
	
	int old_projection_method = config->getProjectionMethod();
	config->setProjectionMethod(2);
	bool old_parallel_projection = config->getParallelProjection();
	config->setParallelProjection(false);
	bool old_cache_projection = config->getCacheProjections();
	config->setCacheProjections(false);
	int old_trial_ang = config->getTrialAngleGenerator();
	config->setTrialAngleGenerator(4);
	
	std::string cont_path = config->getContainerFilePath();
	std::vector<std::string> folder_content;
	SingleParticle2dx::Utilities::UtilityFunctions::getContentOfFolder(cont_bin_folder, folder_content, ".bin");
	
	SingleParticle2dx::DataStructures::ParticleContainer cont_tree;
	
	if(boost::filesystem::exists(cont_stat_file_name))
	{
		boost::filesystem::remove(cont_stat_file_name);
	}
	
	rec_3d.setProjectionMethod(2);
	
	std::map<int, float> cc_map;
	int counter = 0;
	
	if(mode==1)
	{
		rec_3d.writeToFile(init_model_out_file);
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput( init_model_out_file, "MAP: Initial 3D reconstruction", config->getScriptName(), false);
		
		boost::shared_ptr<FILE> file;
		
		if(tree_level == 2)
		{
			std::string file_name = config->getContainerFilePath() + "/Div_output/cc_ang.txt";
			file.reset( fopen ( file_name.c_str(), "w" ), SingleParticle2dx::Utilities::CustomizedDeleter() );
		}
	
		#pragma omp parallel for schedule (dynamic,1)
		for(int i=0; i< static_cast<int>(folder_content.size()); i++)
		{
			SingleParticle2dx::DataStructures::ParticleContainer cont;
			SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(folder_content[i], cont, true);
		
			std::vector<std::string> split_vector;
			SingleParticle2dx::Utilities::StringFunctions::splitString( split_vector, folder_content[i], std::string("/") );
			std::string filename_core_fp = split_vector.back();
			std::string folder_bin_fp = cont_bin_folder_fp + filename_core_fp;
			SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(folder_bin_fp, cont, false);
		
			int width = cont(0).getGlobalParticleInformation().getImageWidth();
			cont.findNeighbors(1);
		
			SingleParticle2dx::DataStructures::QuadTreeNode tree(width, width/2., width/2., cont);
			tree.split();
			tree.plot();
			
			SingleParticle2dx::DataStructures::Reconstruction3d rec_local(n,n,n);
			rec_local.setMinimal(false);
			rec_local.setProjectionMethod(2);
			rec_local.readFromFile(init_rec_file);
			rec_local.applyMask();
			rec_local.applyLowPassFilter();
		
			tree.prepareLayer(tree_level, rec_local);
			tree.extractLayer(tree_level, cont_tree, cont.getNumberOfParticles(), i, rec_local);
		
			SingleParticle2dx::DataStructures::ParticleContainer cont_ave_local;
			cont.generateAverageContainer(cont_ave_local, false, false);
		
			if(cont_ave_local.getNumberOfParticles() > 1)
			{
				std::cout << "::Error with average generation, more than one particle" << std::endl;
				throw std::runtime_error("Bad operation");
			}
		
			rec_local.updateReconstruction(cont_ave_local, false, false, true);
		
			#pragma omp critical (extract_tree_layer_and_write_stat)
			{
				cont.writeStatToFile(cont_stat_file_name, true);
				cc_map[i] = cont_ave_local(0).getSimMeasure();
				
				if(tree_level == 2)
				{
					float tiltang = cont_ave_local(0).getNewOrientation().getTLTANG();
					if(tiltang > 90)
					{
						tiltang -= 180;
					}
					tiltang = fabs(tiltang);
					fprintf(file.get(), "%f\t%f\n", tiltang, cc_map[i]);
				}
				
				counter++;
				int proc = static_cast<int>(static_cast<float>(counter)/(static_cast<float>(folder_content.size()))*100);
				SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( proc );
			}
	
		}
	
		applyWeight(cont_tree, static_cast<int>(folder_content.size()), cc_map);
	
		cont_tree.writeContainerToDisk(tree_cont_folder, 1);
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(tree_cont_folder, "Extracted Tree", config->getScriptName(), false);
		SingleParticle2dx::DataStructures::ParticleContainer::serializeContainerToDisk(cont_tree, true, "tree.bin");
	}

	config->setProjectionMethod(old_projection_method);
	config->setTrialAngleGenerator(old_trial_ang);
	config->setParallelProjection(old_parallel_projection);
	config->setCacheProjections(old_cache_projection);
	rec_3d.setProjectionMethod(config->getProjectionMethod());

	if(mode==2)
	{
		SingleParticle2dx::DataStructures::ParticleContainer cont_tree;
		SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk("tree.bin", cont_tree, true);
	
		int shake_count = 1;
		SingleParticle2dx::Utilities::ConvAnalyzer conv_analyzer(config->getMinAngleChange(), 3);
		std::string conv_file_name = "SCRATCH/2dx_SP_convergence.txt";
		SingleParticle2dx::Utilities::UtilityFunctions::removeFileIfExists(conv_file_name);
	
		for (int i=0; i<config->getMaxIteration(); i++)
		{
			rec_3d.updateReconstruction(cont_tree, false, false);
			rec_3d.applyMask();
			rec_3d.applyLowPassFilter();
		
			SingleParticle2dx::Utilities::UtilityFunctions::generateVariableOutput("angular_change_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i), rec_3d.getLastAngleChange(), config->getScriptName());
		
			conv_analyzer.addChange(rec_3d.getLastAngleChange());
			SingleParticle2dx::Utilities::UtilityFunctions::writeConvergenceToFile(i+1, rec_3d.getLastAngleChange());
		
			if (config->getKeepAll())
			{
				rec_3d.writeToFile( tree_conv_volume_corename + SingleParticle2dx::Utilities::StringFunctions::TtoString(i) + ".map" );
				SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput( tree_conv_volume_corename + SingleParticle2dx::Utilities::StringFunctions::TtoString(i) + ".map", "MAP: refine " + SingleParticle2dx::Utilities::StringFunctions::TtoString(i), config->getScriptName(), false);
			}

			if ( conv_analyzer.isConverged() )
			{
				SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("CONVERGERD", 1);
				break;
			}
		
			if (conv_analyzer.isCycling())
			{
				std::cout << "::Should SHAKE NOWWWWW!!!!!!!!!" << std::endl;
				//cont_ave.shakeContainer( config->getShakingRateTree() * shake_count );
				cont_tree.shakeContainer( 0.2 * shake_count );
				shake_count++;
				rec_3d.generateInitialModel(cont_tree);
				rec_3d.applyMask();
				conv_analyzer.reset();
				conv_analyzer.addChange(rec_3d.getLastAngleChange());
			}
		}
	
		rec_3d.writeToFile( final_map_output );
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(final_map_output, "MAP: Refined Tree Reconstruction", config->getScriptName(), true);
	
		SingleParticle2dx::DataStructures::ParticleContainer::serializeContainerToDisk(cont_tree, true, "tree_refined.bin");
	}

	if (mode==3)
	{
		SingleParticle2dx::DataStructures::ParticleContainer cont_tree;
		SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk("tree_refined.bin", cont_tree, true);
	
		std::string cont_stat_file_name_tree = tree_refined_stat;
		if(boost::filesystem::exists(cont_stat_file_name_tree))
		{
			boost::filesystem::remove(cont_stat_file_name_tree);
		}
	
		cont_bin_folder = tree_refined_container;
		if(boost::filesystem::exists(cont_bin_folder))
		{
			boost::filesystem::remove_all(cont_bin_folder);
		}
		boost::filesystem::create_directory(cont_bin_folder);
	
		#pragma omp parallel for schedule (dynamic,1)
		for(int i=0; i< static_cast<int>(folder_content.size()); i++)
		{
			SingleParticle2dx::DataStructures::ParticleContainer cont;
			SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(folder_content[i], cont, true);
		
			std::vector<std::string> split_vector;
			SingleParticle2dx::Utilities::StringFunctions::splitString( split_vector, folder_content[i], std::string("/") );
			std::string filename_core_fp = split_vector.back();
			std::string folder_bin_fp = cont_bin_folder_fp + filename_core_fp;
			SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(folder_bin_fp, cont, false);
		
			std::vector<SingleParticle2dx::DataStructures::Particle> part_vec;
		
			for(int j=0; j<cont_tree.getNumberOfParticles(); j++)
			{
				if( cont_tree(j).getGlobalParticleInformation().getImageNumber() == i)
				{
					part_vec.push_back(cont_tree(j));
				}
			}
		
			if ( static_cast<int>(part_vec.size()) != getNumberofNodes(tree_level) )
			{
				std::cout << "::Bad number of particles in final storing vector" << std::endl;
				throw std::runtime_error("Bad operation");
			}
		
			std::sort(part_vec.begin(), part_vec.end(), SingleParticle2dx::DataStructures::compareParticlesPosDep);
		
			for(int j=0; j<cont.getNumberOfParticles(); j++)
			{
				cont(j).updateOrientation(part_vec[determineTreeNodeNumber(cont(j), tree_level)].getNewOrientation());
			}
		
			split_vector.clear();
			SingleParticle2dx::Utilities::StringFunctions::splitString( split_vector, folder_content[i], std::string("/") );
			std::string filename_core = split_vector.back();
		
			std::string folder_bin = cont_bin_folder + filename_core;
			SingleParticle2dx::DataStructures::ParticleContainer::serializeContainerToDisk(cont, false, folder_bin);
		
			#pragma omp critical (tree_write_final_stat)
			{
				cont.writeStatToFile(cont_stat_file_name_tree, true);
			}
		}
	}
	
	return 0;
	
}
