#include "../2dxSingleParticle.hpp"

#include <fstream>

#include <boost/lexical_cast.hpp>

#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/program_options.hpp>



int main (int argc, char const *argv[])
{
	namespace po = boost::program_options;
	
	typedef boost::archive::binary_iarchive archive_in_type;
	typedef boost::archive::binary_oarchive archive_out_type;
	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	int n = config->getParticleSize();
	int cc_n = config->getCrossCorrelationWindowSize();
	
	std::string reference_image;
	std::string refine_image;
	
	po::options_description desc("Allowed options");
	desc.add_options()
	    ("help", "produce help message")
	    ("ReferenceImage", po::value<std::string>(&reference_image), "Image used as reference")
		("RefineImage", po::value<std::string>(&refine_image), "Image to refine")
	;
	
	po::variables_map vm;
	po::store(po::parse_command_line(argc, argv, desc), vm);
	po::notify(vm);    

	if (vm.count("help")) {
	    cout << desc << "\n";
	    return 1;
	}
	
	if ( vm.count("ReferenceImage") && vm.count("RefineImage") )
	{
		config->setImageDirsManually(reference_image, refine_image);
	}

	std::vector<std::string> image_dirs = SingleParticle2dx::ConfigContainer::Instance()->getImageDirectories();
	
	std::cout << "project dir:" << config->getProjectDirectory() << std::endl;
	
	SingleParticle2dx::DataStructures::ParticleContainer cont_dummy;
	SingleParticle2dx::DataStructures::ParticleContainer cont_ref;
	SingleParticle2dx::DataStructures::PickingDiagnostics dia;
	
	std::string working_dir = (config->getProjectDirectory() + image_dirs[0]);
	std::vector<std::string> split_vector;
	boost::split( split_vector, working_dir, boost::is_any_of("/") ); 
	std::string filename_core = split_vector.back();
	std::string cont_name = filename_core + "/cont.bin";
	
	SingleParticle2dx::DataStructures::Projection2d p(n,n);
	SingleParticle2dx::DataStructures::Orientation o;
	
	if (!config->getConsContOnly())
	{
		if ( boost::filesystem::exists(cont_name) && config->getConsReuse() )
		{
			std::cout << "reloading ref container: " << cont_name << std::endl;
			std::ifstream ifs(cont_name.c_str());
			archive_in_type ia(ifs);
			ia >> cont_ref;
		}
		else
		{
			SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("picking from: " + working_dir, 2);
			SingleParticle2dx::DataStructures::ParticleContainer::pickParticlesFromCCProfile(working_dir, cont_ref, dia, 0);
			cont_ref.setParticleNumbers();
			dia.print();
		
			std::ofstream ofs(cont_name.c_str());
			{
				archive_out_type oa(ofs);
				oa << cont_ref;
			}
		}
	
		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput(boost::lexical_cast<std::string>(cont_ref.getNumberOfParticles()) + " particles picked", 1);
		
		if (true)
		{
			SingleParticle2dx::real_array2d_type real_data( boost::extents[n][n] );
			cont_ref.getAverage(real_data);
			SingleParticle2dx::DataStructures::Orientation o_av = cont_ref(0).getInitialOrientation();
			cont_ref.clear();
			SingleParticle2dx::DataStructures::GlobalParticleInformation i_av;
			SingleParticle2dx::DataStructures::Particle p_av(n, n, o_av, i_av);
			p_av.setFourierSpaceData(&real_data);
			cont_ref.addParticle(p_av);
		}
		
		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput(boost::lexical_cast<std::string>(cont_ref.getNumberOfParticles()) + " particles picked", 1);
		SingleParticle2dx::DataStructures::Reconstruction3d rec_ref(n, n, n);
		rec_ref.generateInitialModel(cont_ref);
		rec_ref.applyLowPassFilter();
		rec_ref.updateReconstruction(cont_dummy);
		rec_ref.calculateProjection(o, p);
		p.writeToFile("proj_ref.mrc");
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("proj_ref.mrc", "Reference Projection", config->getScriptName(), false);
	}
	
	std::vector<float> cc_values;
	std::vector<SingleParticle2dx::DataStructures::Orientation> orientations;
	
	for (SingleParticle2dx::size_type i=1; i<config->getNumberOfImages(); i++)
	{
		SingleParticle2dx::DataStructures::ParticleContainer cont_im;
		std::string working_dir = (config->getProjectDirectory() + image_dirs[i]);
		
		std::vector<std::string> split_vector;
		boost::split( split_vector, working_dir, boost::is_any_of("/") ); 
		std::string filename_core = split_vector.back();
		
		std::string cont_name = filename_core + "/cont.bin";
		
		if ( boost::filesystem::exists(cont_name) && config->getConsReuse() && (!config->getConsContOnly()))
		{
			std::cout << "reloading ref container: " << cont_name << std::endl;
			std::ifstream ifs(cont_name.c_str());
			archive_in_type ia(ifs);
			ia >> cont_im;
			
			std::string config_name = (working_dir + "/" + "2dx_image.cfg");
			SingleParticle2dx::Utilities::ImageConfigReader image_config (config_name);
			
			float tltaxis = image_config.getConfigElement("TLTAXIS")[0];
			float tltang = image_config.getConfigElement("TLTANG")[0];
			float taxa = image_config.getConfigElement("TAXA")[0];
			
			SingleParticle2dx::DataStructures::Orientation o2set(tltaxis, tltang, taxa);
			cont_im.setAllOrientations( o2set );
			
		}
		else 
		{
			SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("picking from: " + working_dir, 2);
			SingleParticle2dx::DataStructures::ParticleContainer::pickParticlesFromCCProfile(working_dir, cont_im, dia, i);
			cont_im.setParticleNumbers();
			dia.print();
			
			std::ofstream ofs(cont_name.c_str());
			{
				archive_out_type oa(ofs);
				oa << cont_im;
			}
		}
		
		if (config->getConsContOnly())
		{
			return 0;
		}
		
		
		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput(boost::lexical_cast<std::string>(cont_im.getNumberOfParticles()) + " particles picked", 1);
		
		std::vector<SingleParticle2dx::DataStructures::ParticleContainer*> cont_vec;
		
		if (true)
		{
			SingleParticle2dx::real_array2d_type real_data( boost::extents[n][n] );
			cont_im.getAverage(real_data);
			SingleParticle2dx::DataStructures::Orientation o_av = cont_im(0).getInitialOrientation();
			cont_im.clear();
			SingleParticle2dx::DataStructures::GlobalParticleInformation i_av;
			SingleParticle2dx::DataStructures::Particle p_av(n, n, o_av, i_av);
			p_av.setFourierSpaceData(&real_data);
			cont_im.addParticle(p_av);
		}
		
		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput(boost::lexical_cast<std::string>(cont_im.getNumberOfParticles()) + " particles picked", 1);
		
		
		int num_threads;
		#pragma omp parallel
		{
			num_threads = omp_get_num_threads(); 
		}
		
		for(int nt=0; nt<num_threads; nt++)
		{
			cont_vec.push_back(new SingleParticle2dx::DataStructures::ParticleContainer);
			for (int np=0; np<cont_im.getNumberOfParticles(); np++)
			{
				cont_vec[nt]->addParticle(cont_im(np));
			}
			cont_vec[nt]->setParticleNumbers();
		}
		
		std::vector<SingleParticle2dx::DataStructures::Orientation> o_vec;
		cont_im.getDistinctAngles(o_vec);
		SingleParticle2dx::DataStructures::Orientation o_im = o_vec[0];
		
		float tltang_im = o_im.getTLTANG();
		float tltaxis_im = o_im.getTLTAXIS();
		float taxa_im = o_im.getTAXA();
		
		int dn = config->getConsClickNumber();
		float dphi = config->getConsClickSize();
		

		
		std::vector<SingleParticle2dx::DataStructures::Orientation> trial_ori_vec;
		for (float tltaxis = tltaxis_im - dn*dphi; tltaxis <= tltaxis_im + dn*dphi; tltaxis+=dphi)
		{
			for (float taxa = taxa_im - dn*dphi; taxa <= taxa_im + dn*dphi; taxa+=dphi)
			{
				float tltang = tltang_im;
				SingleParticle2dx::DataStructures::Orientation o2set(tltaxis, tltang, taxa);
				trial_ori_vec.push_back(o2set);
			}
		}
		
		#pragma omp parallel for schedule(dynamic, 1)
		for (int kk=0; kk<static_cast<int>(trial_ori_vec.size()); kk++)
		{
			int nthread = omp_get_thread_num();
						
			SingleParticle2dx::DataStructures::Orientation o2set = trial_ori_vec[kk];
			cont_vec[nthread]->setAllOrientations( o2set );
					
			SingleParticle2dx::DataStructures::Reconstruction3d rec_im(n, n, n);
			rec_im.generateInitialModel(*cont_vec[nthread]);
			rec_im.applyLowPassFilter();
			SingleParticle2dx::DataStructures::Projection2d p_im(n,n);
			rec_im.updateReconstruction(cont_dummy);
			rec_im.calculateProjection(o, p_im);
					
			SingleParticle2dx::DataStructures::GlobalParticleInformation info;
			SingleParticle2dx::DataStructures::Particle part(n, n, o, info);
			SingleParticle2dx::real_array2d_type real_data( boost::extents[n][n] );
			p_im.getRealSpaceData(&real_data);
			part.setFourierSpaceData(&real_data);
		
			SingleParticle2dx::Methods::BasicCalcCCMethod sim_calculator;
				
			SingleParticle2dx::real_array2d_type cc_res( boost::extents[cc_n][cc_n] );
				
			part.normalizeRealSpace();
			p.normalizeRealSpace();
			sim_calculator.calculateCrossCorrelation(part, p, cc_res);
					
			SingleParticle2dx::Methods::MaxSearchMethod max_calculator;
			SingleParticle2dx::DataStructures::ParticleShift shift;
			float cc_value = max_calculator.findMaximalElementAndSetShift(cc_res, shift);
					
			#pragma omp critical (insertion_cons_refine)
			{
				cc_values.push_back(cc_value);
				orientations.push_back(o2set);
				
				std::cout << cc_value << ": " << o2set.getTLTAXIS() << " " << o2set.getTLTANG() << " " << o2set.getTAXA() << std::endl;
			}
		}
		
		for (int ii=0; ii<static_cast<int>(cc_values.size()); ii++)
		{
			std::cout << cc_values[ii] << ": " << orientations[ii].getTLTAXIS() << " " << orientations[ii].getTLTANG() << " " << orientations[ii].getTAXA() << std::endl;
		}
		
		std::vector<float>::iterator result = std::max_element(cc_values.begin(), cc_values.end());
		int index = std::distance(cc_values.begin(), result);
		
		std::cout << "Best match " << cc_values[index] << ": " << orientations[index].getTLTAXIS() << " " << orientations[index].getTLTANG() << " " << orientations[index].getTAXA() << std::endl;
		
		SingleParticle2dx::DataStructures::Reconstruction3d rec_im(n, n, n);
		SingleParticle2dx::DataStructures::Orientation o2set;
		o2set.setTLTAXIS(orientations[index].getTLTAXIS());
		o2set.setTLTANG(orientations[index].getTLTANG());
		o2set.setTAXA(orientations[index].getTAXA());
		cont_im.setAllOrientations( o2set );
		rec_im.generateInitialModel(cont_im);
		rec_im.applyLowPassFilter();
		SingleParticle2dx::DataStructures::Projection2d p_im(n,n);
		rec_im.updateReconstruction(cont_dummy);
		rec_im.calculateProjection(o, p_im);
		
		p_im.writeToFile(filename_core + "/consBestProj.mrc");
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(filename_core + "/consBestProj.mrc", "Best Proj " + image_dirs[i], config->getScriptName(), true);
		
		std::string o_filename = filename_core + "/best_orientation.txt";
		boost::shared_ptr<FILE> file( fopen ( o_filename.c_str(), "w" ), SingleParticle2dx::Utilities::CustomizedDeleter() );
		
		fprintf(file.get(), "%f\t%f\t%f\n", orientations[index].getTLTAXIS(), orientations[index].getTLTANG(), orientations[index].getTAXA());
		
		int proc = static_cast<int>(static_cast<float>(i+1)/static_cast<float>(config->getNumberOfImages())*100);
		SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( proc );
	}
	
	
	
	return 0;
		
/*
	for (SingleParticle2dx::size_type i=0; i<config->getNumberOfImages(); i++)
	{
		SingleParticle2dx::DataStructures::ParticleContainer cont;
		SingleParticle2dx::DataStructures::PickingDiagnostics dia;
		
		std::string working_dir = (config->getProjectDirectory() + image_dirs[i]);
		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("picking from: " + working_dir, 2);
		SingleParticle2dx::DataStructures::ParticleContainer::pickParticlesFromCCProfile(working_dir, cont, dia, i);
		cont.setParticleNumbers();
		dia.print();
		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput(boost::lexical_cast<std::string>(cont.getNumberOfParticles()) + " particles picked", 1);
		
		SingleParticle2dx::DataStructures::Reconstruction3d rec3d_full(n, n, n);
		rec3d_full.generateInitialModel(cont);
		rec3d_full.applyLowPassFilter();
		
		SingleParticle2dx::DataStructures::Projection2d p(n,n);
		SingleParticle2dx::DataStructures::Orientation o;
		rec3d_full.updateReconstruction(cont_dummy);
		rec3d_full.calculateProjection(o, p);
		p.writeToFile("proj_" + boost::lexical_cast<std::string>(i) + ".mrc");
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("proj_" + boost::lexical_cast<std::string>(i) + ".mrc", "Proj " + image_dirs[i], config->getScriptName(), false);
		
		
	}
*/
	
	return 0;
}
