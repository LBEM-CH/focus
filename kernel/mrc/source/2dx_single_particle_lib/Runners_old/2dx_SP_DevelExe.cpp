#include "../2dxSingleParticle.hpp"

#include <fstream>

#include <boost/lexical_cast.hpp>

#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>


int main (int argc, char const *argv[])
{
	namespace po = boost::program_options;
	
	typedef boost::archive::binary_iarchive archive_in_type;
	typedef boost::archive::binary_oarchive archive_out_type;
	
	std::string binary_config_file;
	std::string binary_particle_file;
	
	po::options_description desc("Allowed options");
	desc.add_options()
	    ("help", "produce help message")
	    ("config", po::value<std::string>(&binary_config_file), "Config container file")
		("particles", po::value<std::string>(&binary_particle_file), "Particle container file")
	;
	
	po::variables_map vm;
	po::store(po::parse_command_line(argc, argv, desc), vm);
	po::notify(vm);    

	if (vm.count("help")) {
	    cout << desc << "\n";
	    return 1;
	}
	
	SingleParticle2dx::DataStructures::ParticleContainer cont;
	SingleParticle2dx::DataStructures::ParticleContainer cont1;
	SingleParticle2dx::DataStructures::ParticleContainer cont2;
	SingleParticle2dx::ConfigContainer* config;
	int n;
	
	if ( vm.count("config") && vm.count("particles") )
	{
		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Restoring Container and Config from binary files...", 2);

		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Restoring Config from binary file...", 1);
		{
		        std::ifstream ifs(binary_config_file.c_str());
		        archive_in_type ia(ifs);
		        ia >> config;
		}

		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Restoring Container from binary file...", 1);
		{
			std::ifstream ifs(binary_particle_file.c_str());
			archive_in_type ia(ifs);
			ia >> cont;
		}
		
		boost::filesystem::remove_all("SCRATCH");
		boost::filesystem::create_directory("SCRATCH");
		
		boost::filesystem::remove_all("LOGS");
		boost::filesystem::create_directory("LOGS");
		
		n = config->getParticleSize();
		
	}
	else
	{
		config = SingleParticle2dx::ConfigContainer::Instance();

		if (config->getConfOnly())
		{
			std::cout << "storing config.bin" << std::endl;
			std::ofstream ofs2("config.bin");
			{
				archive_out_type oa(ofs2);
				oa << config;
			}
			return 0;
		}

		n = config->getParticleSize();

		SingleParticle2dx::DataStructures::PickingDiagnostics dia;
		std::vector<std::string> image_dirs = SingleParticle2dx::ConfigContainer::Instance()->getImageDirectories();

		std::cout << "project dir:" << config->getProjectDirectory() << std::endl;
		
		std::string cont_name = "cont.bin";
		
		if ( boost::filesystem::exists(cont_name) && config->getConsReuse() )
		{
			std::cout << "reloading ref container: " << cont_name << std::endl;
			std::ifstream ifs(cont_name.c_str());
			archive_in_type ia(ifs);
			ia >> cont;
		}
		else
		{

  	  		for (SingleParticle2dx::value_type i=0; i<config->getNumberOfImages(); i++)
  	  		{
  	  			std::string working_dir = (config->getProjectDirectory() + image_dirs[i]);
  	  			SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("picking from: " + working_dir, 2);
  	  			
				SingleParticle2dx::DataStructures::ParticleContainer::pickParticlesFromCCProfile(working_dir, cont, dia, i, false);

			//	SingleParticle2dx::DataStructures::ParticleContainer::pickParticlesFromCCProfile(working_dir, cont, dia, 2*i, false);
			//	SingleParticle2dx::DataStructures::ParticleContainer::pickParticlesFromCCProfile(working_dir, cont, dia, 2*i+1, true);
  	  		}
  	  	
			cont.setParticleNumbers();
  	  		dia.print();
  	  		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput(boost::lexical_cast<std::string>(cont.getNumberOfParticles()) + " particles picked", 1);
			
			std::ofstream ofs(cont_name.c_str());
			{
				archive_out_type oa(ofs);
				oa << cont;
			}

		}

	//	boost::filesystem::remove("mergevars_written_to_file.txt");

		if ( config->getOnlyStack() )
		{
			SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Storing Container and Config to binary files...", 2);

			std::ofstream ofs("container.bin");
			{
				archive_out_type oa(ofs);
				oa << cont;
			}

			std::ofstream ofs2("config.bin");
			{
				archive_out_type oa(ofs2);
				oa << config;
			}

			SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("...done", 2);

			return 0;
		}
	}
	
	cont.setParticleNumbers();
	
	n = SingleParticle2dx::ConfigContainer::Instance()->getParticleSize();
	
	std::cout << cont.getNumberOfParticles() << std::endl;
	std::vector<SingleParticle2dx::DataStructures::ParticleContainer> cont_vec;
	
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d_ave(n, n, n);
	SingleParticle2dx::DataStructures::ParticleContainer cont_ave;
	cont.generateAverageContainer(cont_ave);
	rec3d_ave.generateInitialModel(cont_ave);
	rec3d_ave.applyMask();
	rec3d_ave.applyLowPassFilter();
	
	rec3d_ave.writeToFile( "initial_model_ave.map" );
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("initial_model_ave.map", "MAP: Initial Model from AVE", config->getScriptName(), true);
	
	config->setEmanTrialMax(15);
	config->setEmanTrialN(30);
		
	for (int i=0; i<20; i++)
	{
		rec3d_ave.updateReconstruction(cont_ave, false);
		rec3d_ave.applyMask();
		rec3d_ave.applyLowPassFilter();

		if ( rec3d_ave.getLastAngleChange()<0.25 )
		{
			SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("CONVERGERD, increase search range", 1);
			break;
		}
	}
	
	rec3d_ave.writeToFile( "initial_model_it.map" );
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("initial_model_it.map", "MAP: Initial Model from iteration", config->getScriptName(), true);
	
	cont.updateInitialTiltGeometry(cont_ave);
	cont.splitIntoPerImageContainer(cont_vec);
	
	for(int i=0; i<static_cast<int>(cont_vec.size()); i++)
	{
		std::cout << cont_vec[i].getNumberOfParticles() << std::endl;
		cont_vec[i].findNeighbors(true);
	}
	
	std::cout << "split done" << std::endl;
	
	int refine_counter = 0;
	
	for(int level=1; level<3; level++)
	{
		SingleParticle2dx::DataStructures::ParticleContainer cont_1;
		
		for(int i=0; i<static_cast<int>(cont_vec.size()); i++)
		{
			std::cout << cont_vec[i].getNumberOfParticles() << std::endl;
			int width = cont_vec[i](0).getGlobalParticleInformation().getImageWidth();
			std::cout << width << std::endl;
			SingleParticle2dx::DataStructures::QuadTreeNode tree(width, width/2., width/2., cont_vec[i]);
			tree.split();
			tree.plot();
			tree.prepareLayer(level, rec3d_ave);
			tree.extractLayer(level, cont_1);
		}

		cont_1.writeContainerToDisk("cont_tree_1", 1);

		config->setEmanTrialMax(3.0);
		config->setEmanTrialN(15);

		SingleParticle2dx::size_type cc_size = config->getCrossCorrelationWindowSize();
		config->setCrossCorrelationWindowSize(11);

		float last_change = 0;
		int ref_count = 0;
		
		SingleParticle2dx::Utilities::ConvAnalyzer conv_analyzer(std::max(cont_1.getNumberOfParticles()*0.01, 0.25), 8);

		int shake_count = 1;

		for (int i=0; i<400; i++)
		{
			refine_counter++;
			
			rec3d_ave.updateReconstruction(cont_1, false);
			rec3d_ave.applyMask();
			rec3d_ave.applyLowPassFilter();
			
			last_change = rec3d_ave.getLastAngleChange();
			conv_analyzer.addChange(last_change);

			if (conv_analyzer.isCycling())
			{
				if(ref_count == 0)
				{
					conv_analyzer.addChange(0.0);
				}
				else
				{
					std::cout << "::Should SHAKE NOWWWWW!!!!!!!!!" << std::endl;
					cont_1.shakeContainer( 0.25 * shake_count );
					shake_count++;
					rec3d_ave.generateInitialModel(cont_1);
					rec3d_ave.applyMask();
					conv_analyzer.reset();
					conv_analyzer.addChange(last_change);
				}
			}

			
			if (conv_analyzer.isConverged())
			{
				SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("CONVERGERD, increase search range", 1);
				//break;
				ref_count++;
				config->setEmanTrialN(2*config->getEmanTrialN());
				rec3d_ave.writeToFile("init_" + boost::lexical_cast<std::string>(level) + "_" + boost::lexical_cast<std::string>(ref_count) + ".map");
				SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("init_" + boost::lexical_cast<std::string>(level) + "_" + boost::lexical_cast<std::string>(ref_count) + ".map", "MAP: Refined Initial Model " + boost::lexical_cast<std::string>(level) + " " + boost::lexical_cast<std::string>(ref_count), config->getScriptName());
				
				
				conv_analyzer.reset();
				
				shake_count = 1;
			}
			
			SingleParticle2dx::Utilities::UtilityFunctions::writeConvergenceToFile(refine_counter, last_change, level, 2);

			if ( ref_count >= 2 )
			{
				SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("CONVERGERD, ref_count = max", 1);
				break;
			}
		}
		
		int index = 0;
		for(int i=0; i<static_cast<int>(cont_vec.size()); i++)
		{
			int width = cont_vec[i](0).getGlobalParticleInformation().getImageWidth();
			SingleParticle2dx::DataStructures::QuadTreeNode tree(width, width/2., width/2., cont_vec[i]);
			tree.split();
			tree.updateOrientations(level, cont_1, index);
		}
		
	}
	
	SingleParticle2dx::DataStructures::ParticleContainer cont_new;
	for(int i=0; i<static_cast<int>(cont_vec.size()); i++)
	{
		SingleParticle2dx::DataStructures::ParticleContainer::mergeContainers(cont_new, cont_vec[i]);
		cont_vec[i].clear();
	}
	
	cont_new.writeStatToFile( "container_stat_tree.txt" );

	SingleParticle2dx::DataStructures::Reconstruction3d rec3d(n,n,n);
	rec3d.setBackprojectionMethod ( 3 );
	rec3d.generateInitialModel(cont_new);
	rec3d.applyLowPassFilter();
	
	rec3d.writeToFile( "initial_model_ref.map" );
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("initial_model_ref.map", "MAP: Initial Model", config->getScriptName(), true);
	
	cont_new.findNeighbors();
	
	int max_it = config->getMaxIteration();
	config->setEmanTrialMax(.75);
	config->setEmanTrialN(30);
	config->setCrossCorrelationWindowSize(11);
	
	for(int i=0; i<max_it; i++)
	{
		std::cout << "before reconstruction" << std::endl;
		rec3d.updateReconstruction(cont_new);
		
		SingleParticle2dx::Utilities::UtilityFunctions::generateVariableOutput("angular_change_" + boost::lexical_cast<std::string>(i), rec3d.getLastAngleChange(), config->getScriptName());
		SingleParticle2dx::Utilities::UtilityFunctions::writeConvergenceToFile(i+1, rec3d.getLastAngleChange());
				
		rec3d.applyMask();
		rec3d.applyLowPassFilter();
		
		rec3d.writeToFile("reconstruction_" + boost::lexical_cast<std::string>(i) + ".map");
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("reconstruction_" + boost::lexical_cast<std::string>(i) + ".map", "MAP: Refined Model " + boost::lexical_cast<std::string>(i), config->getScriptName());
	
		int proc = static_cast<int>(static_cast<float>(i+1)/static_cast<float>(max_it)*100);
		SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( proc );
	
		if (rec3d.getLastAngleChange() == 0.0)
		{
			SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("CONVERGERD", 1);
			break;
		}	
	}
	
	cont_new.writeStatToFile( "container_stat.txt" );
	
	std::pair<std::vector<float>, std::vector<float> > fsc_info = SingleParticle2dx::DataStructures::ParticleContainer::calculateFSC( cont_new );
	int res_radius = SingleParticle2dx::Utilities::UtilityFunctions::getGoldStandardFSCRadius(fsc_info);
	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Gold FSC Radius: " + boost::lexical_cast<std::string>(res_radius), 1);
	
/*
	n = SingleParticle2dx::ConfigContainer::Instance()->getParticleSize();
	int width = cont(0).getGlobalParticleInformation().getImageWidth();
	std::cout << width << std::endl;
	SingleParticle2dx::DataStructures::QuadTreeNode tree(width, width/2., width/2., cont);
	tree.split();
	std::cout << "::depth = " << tree.getDepth() << std::endl;
	tree.plot();
	
	SingleParticle2dx::DataStructures::ParticleContainer cont_1;
	tree.extractLayer(1, cont_1);
	cont_1.writeContainerToDisk("cont_tree_1", 1);
	
	SingleParticle2dx::DataStructures::ParticleContainer cont_2;
	tree.extractLayer(2, cont_2);
	cont_2.writeContainerToDisk("cont_tree_2", 1);
	
	SingleParticle2dx::DataStructures::ParticleContainer cont_3;
	tree.extractLayer(3, cont_3);
	cont_3.writeContainerToDisk("cont_tree_3", 1);
	
	SingleParticle2dx::DataStructures::ParticleContainer cont_4;
	tree.extractLayer(4, cont_4);
	cont_4.writeContainerToDisk("cont_tree_4", 1);

	
	return 0;
	
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d_ave(n, n, n);
	SingleParticle2dx::DataStructures::ParticleContainer cont_ave;
	cont.generateAverageContainer(cont_ave);
	rec3d_ave.generateInitialModel(cont_ave);
	rec3d_ave.applyMask();
	rec3d_ave.applyLowPassFilter();
	
	rec3d_ave.writeToFile( "initial_model_ave.map" );
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("initial_model_ave.map", "MAP: Initial Model from AVE", config->getScriptName(), true);
	
	config->setEmanTrialMax(2.5);
	config->setEmanTrialN(2);
	
	SingleParticle2dx::size_type cc_size = config->getCrossCorrelationWindowSize();
	config->setCrossCorrelationWindowSize(1);
	
	float last_change = 0;
	int ref_count = 0;
	
	for (int i=0; i<200; i++)
	{
		rec3d_ave.updateReconstruction(cont_ave, false);
		rec3d_ave.applyMask();
		rec3d_ave.applyLowPassFilter();
		
		if ( (rec3d_ave.getLastAngleChange()<0.25) || (last_change==rec3d_ave.getLastAngleChange()) )
		{
			SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("CONVERGERD, increase search range", 1);
			//break;
			ref_count++;
			config->setEmanTrialN(2*config->getEmanTrialN());
			rec3d_ave.writeToFile("init_" + boost::lexical_cast<std::string>(ref_count) + ".map");
			SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("init_" + boost::lexical_cast<std::string>(ref_count) + ".map", "MAP: Refined Initial Model " + boost::lexical_cast<std::string>(ref_count), config->getScriptName());
		}
		
		last_change = rec3d_ave.getLastAngleChange();
		
		if ( ref_count > 4 )
		{
			SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("CONVERGERD, ref_count = max", 1);
			break;
		}
		
	}
	config->setCrossCorrelationWindowSize(cc_size);
	
	for (int i=0; i<static_cast<int>(cont_ave.getNumberOfParticles()); i++)
	{
		std::cout << "Image: " << i << std::endl;
		std::cout << "\tTLTAXIS: " << cont_ave(i).getInitialOrientation().getTLTAXIS() << " " << cont_ave(i).getNewOrientation().getTLTAXIS() << std::endl;
		std::cout << "\tTLTANG: " << cont_ave(i).getInitialOrientation().getTLTANG() << " " << cont_ave(i).getNewOrientation().getTLTANG() << std::endl;
		std::cout << "\tTAXA: " << cont_ave(i).getInitialOrientation().getTAXA() << " " << cont_ave(i).getNewOrientation().getTAXA() << std::endl << std::endl;
	}
	
	rec3d_ave.writeToFile( "initial_model_it.map" );
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("initial_model_it.map", "MAP: Initial Model after 10 refines", config->getScriptName(), true);
	//cont.updateInitialTiltGeometry(cont_ave);
	
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d(n, n, n);
	
//	cont.shuffleContainer(100000);
	
//	cont.shakeContainer(3);
	rec3d.generateInitialModel(cont);
	rec3d.applyLowPassFilter();
	
	cont.findNeighbors();
	
	rec3d.writeToFile( "initial_model.map" );
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("initial_model.map", "MAP: Initial Model", config->getScriptName(), true);

	int max_it = config->getMaxIteration();
	
	config->setEmanTrialMax(2.0);
	config->setEmanTrialN(2);
	
	for(int i=0; i<max_it; i++)
	{
		rec3d.updateReconstruction(cont);
		
		SingleParticle2dx::Utilities::UtilityFunctions::generateVariableOutput("angular_change_" + boost::lexical_cast<std::string>(i), rec3d.getLastAngleChange(), config->getScriptName());
		SingleParticle2dx::Utilities::UtilityFunctions::writeConvergenceToFile(i+1, rec3d.getLastAngleChange());
				
		rec3d.applyMask();
		
		rec3d.applyLowPassFilter();
		
		rec3d.writeToFile("reconstruction_" + boost::lexical_cast<std::string>(i) + ".map");
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("reconstruction_" + boost::lexical_cast<std::string>(i) + ".map", "MAP: Refined Model " + boost::lexical_cast<std::string>(i), config->getScriptName());
	
		int proc = static_cast<int>(static_cast<float>(i+1)/static_cast<float>(max_it)*100);
		SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( proc );
	
		if (rec3d.getLastAngleChange() == 0.0)
		{
			SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("CONVERGERD", 1);
			break;
		}	
	}
	

	rec3d.generateInitialModel(cont);
	rec3d.applyLowPassFilter();
	rec3d.applyFinalMask();
	rec3d.writeToFile( "reconstruction.map" );

	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("reconstruction.map", "MAP: Final Reconstruction", config->getScriptName(), true);
	
	cont.writeStatToFile( "container_stat.txt" );
		
	return 0;
*/
}
