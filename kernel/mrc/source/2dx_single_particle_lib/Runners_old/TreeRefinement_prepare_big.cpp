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
		
		float multiplier = cc_map[i] / local_cc_sum;
		
		for(int j=0; j<cont_tree.getNumberOfParticles(); j++)
		{
			if(cont_tree(j).getGlobalParticleInformation().getImageNumber() == i)
			{
				cont_tree(j).setWeight( multiplier * cont_tree(j).getWeight() );
				cont_tree(j).setWeight( cont_tree(j).getWeight() );
			}
		}
	}
}


int main (int argc, char const *argv[])
{
	int number;
	namespace po = boost::program_options;
	
	po::options_description desc("2dx Init TiltGeo Refinement");
	desc.add_options()
	    ("help", "produce help message")
	    ("number", po::value<int>(&number), "Image number")
	;
	
	po::variables_map vm;
	po::store(po::parse_command_line(argc, argv, desc), vm);
	po::notify(vm);    

	if ( !vm.count("number") )
	{
		cout << desc << "\n";
	    return 1;
	}
	
	SingleParticle2dx::DataStructures::ParticleContainer cont_tree;
	
	if(number>0)
	{
		SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk("tree_refined.bin", cont_tree, true);
	}
	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	std::string cont_path = config->getContainerFilePath();
	std::string cont_bin_folder = cont_path + "/ParticleContainers/cont_part3";
	std::string cont_bin_folder_fp = cont_path + "/FingerPrintContainers/cont_part3/";
	//std::string cont_bin_folder_fp = cont_path + "/FingerPrintContainers/cont_t3/";
	std::string init_rec_file =  config->getContainerName() + "/Rec_3d/rec_initial_after_t3.map";
	std::string init_model_out_file = config->getContainerName() + "/Rec_3d/init_tree_4.map";
	std::string cont_stat_file_name = cont_path + "/Div_output/tree_shifts_t4.txt";
	std::string tree_cont_folder = config->getContainerName() + "/Div_output/cont_t4";
	std::string tree_conv_volume_corename = config->getContainerName() + "/Rec_3d/t4_";
	std::string final_map_output = config->getContainerName() + "/Rec_3d/rec_initial_after_t4.map";
	std::string tree_refined_container = cont_path + "/FingerPrintContainers/cont_t4/";
	std::string tree_refined_stat = cont_path + "/ContainerStatsTXT/tree_refine_t4.txt";
	
	int n = config->getParticleSize();
	int tree_level = config->getTreeLevel();
	
	SingleParticle2dx::DataStructures::Reconstruction3d rec_3d(n,n,n);
	rec_3d.setMinimal(false);
	rec_3d.readFromFile(init_rec_file);
	rec_3d.applyMask();
	rec_3d.applyLowPassFilter();
	
	config->setProjectionMethod(2);
	config->setParallelProjection(false);
	config->setCacheProjections(false);
	config->setTrialAngleGenerator(4);
	
	std::vector<std::string> folder_content;
	SingleParticle2dx::Utilities::UtilityFunctions::getContentOfFolder(cont_bin_folder, folder_content, ".bin");	
	rec_3d.setProjectionMethod(2);
	
	std::map<int, float> cc_map;
	int counter = 0;
	
	if(number>0)
	{
		rec_3d.writeToFile(init_model_out_file);
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput( init_model_out_file, "MAP: Initial 3D reconstruction", config->getScriptName(), false);
	}
		
	boost::shared_ptr<FILE> file;
	SingleParticle2dx::DataStructures::ParticleContainer cont;
	SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(folder_content[number], cont, true);
	
	std::vector<std::string> split_vector;
	SingleParticle2dx::Utilities::StringFunctions::splitString( split_vector, folder_content[number], std::string("/") );
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
	tree.extractLayer(tree_level, cont_tree, cont.getNumberOfParticles(), number, rec_local);
	
	SingleParticle2dx::DataStructures::ParticleContainer cont_ave_local;
	cont.generateAverageContainer(cont_ave_local, false, false);
	
	if(cont_ave_local.getNumberOfParticles() > 1)
	{
		std::cout << "::Error with average generation, more than one particle" << std::endl;
		throw std::runtime_error("Bad operation");
	}
	
	rec_local.updateReconstruction(cont_ave_local, false, false);
		
	cc_map[number] = cont_ave_local(0).getSimMeasure();
	
	int proc = static_cast<int>(static_cast<float>(number)/(static_cast<float>(folder_content.size()))*100);
	SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( proc );
	
	applyWeight(cont_tree, static_cast<int>(folder_content.size()), cc_map);
	cont_tree.writeContainerToDisk(tree_cont_folder, 1);
	SingleParticle2dx::DataStructures::ParticleContainer::serializeContainerToDisk(cont_tree, true, "tree.bin");
	
	return 0;
}
