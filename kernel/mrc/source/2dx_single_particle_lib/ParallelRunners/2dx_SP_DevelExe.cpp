#include "../2dxSingleParticle.hpp"

#include <fstream>

#include <omp.h>

#include <numeric>

#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>
#include <boost/mpi.hpp>

void writeContainerToDisk_MPI(SingleParticle2dx::DataStructures::ParticleContainer& cont, boost::mpi::communicator& world, std::string filename)
{
	if(world.rank()==0)
	{
		boost::filesystem::remove(filename);
	}
	
	for(int i=0; i<world.size(); i++)
	{
		if(world.rank() == i)
		{
			std::cout << "write stat for rank " << i << std::endl;
			cont.writeStatToFile(filename, true);
		}
		world.barrier();
	}
}


int main(int argc, char* argv[])
{
	boost::mpi::environment env(argc, argv);
	boost::mpi::communicator world;

	namespace po = boost::program_options;
	
	typedef boost::archive::binary_iarchive archive_in_type;
	typedef boost::archive::binary_oarchive archive_out_type;
	
	std::string binary_config_file;
	std::string binary_particle_file;
	std::string init_model_file;
	std::string orientation_in_folder;
	std::string orientation_out_folder;
	std::string orientation_restart_folder_in;
	std::string orientation_restart_folder_out;
	
	po::options_description desc("Allowed options");
	desc.add_options()
	    ("help", "produce help message")
	    ("config", po::value<std::string>(&binary_config_file), "Config container file")
		("particles", po::value<std::string>(&binary_particle_file), "Particle container file")
		("orientations_in", po::value<std::string>(&orientation_in_folder), "Particle orientations IN folder")
		("orientations_out", po::value<std::string>(&orientation_out_folder), "Particle orientations OUT folder")
		("model", po::value<std::string>(&init_model_file), "Model file")
		("restart_in", po::value<std::string>(&orientation_restart_folder_in), "Restart folder in")
		("restart_out", po::value<std::string>(&orientation_restart_folder_out), "Restart folder out")
	;
	
	po::variables_map vm;
	po::store(po::parse_command_line(argc, argv, desc), vm);
	po::notify(vm);    

	if (vm.count("help")) {
	    cout << desc << "\n";
	    return 1;
	}
	
	
	int n;
	
	if ( !vm.count("config") || !vm.count("particles") || !vm.count("model") || !vm.count("orientations_out") || !vm.count("restart_out"))
	{
		cout << desc << "\n";
	    return 1;
	}
	
	
	if(world.rank() == 0)
	{
		int num_omp;
		#pragma omp parallel
		{
			num_omp = omp_get_num_threads();
		}
		std::cout << "OMP_NUM_THREADS: " << num_omp << std::endl;
	}
	
	SingleParticle2dx::DataStructures::ParticleContainer cont;

	SingleParticle2dx::ConfigContainer* config;
	
	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Restoring Container and Config from binary files...", 2);
	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Restoring Config from binary file...", 1);
	{
        std::ifstream ifs(binary_config_file.c_str());
        archive_in_type ia(ifs);
        ia >> config;
	}

	n = config->getParticleSize();
	
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d_buffer(n,n,n);
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d_refine(n,n,n);
	std::vector<std::vector<int> > load;
	std::vector<std::string> folder_content;
	
	if (world.rank() == 0)
	{
		std::string cont_path = config->getContainerFilePath();
		std::string cont_bin_folder = cont_path + "/ParticleContainers/cont_part3";
		std::string cont_bin_folder_fp = cont_path + "/FingerPrintContainers/cont_part3/";
		std::string initial_model_file = cont_path + "/Rec_3d/rec_initial_after_t3.map";
		std::string initial_model_file_out = config->getContainerName() + "/Rec_3d/rec_initial_nn.map";
	
		
		SingleParticle2dx::Utilities::UtilityFunctions::getContentOfFolder(binary_particle_file, folder_content, ".bin");
		
		std::cout << "number of images to refine: " << folder_content.size() << " with " << world.size() << " mpi-tasks" << std::endl;
		
		rec3d_buffer.readFromFile(init_model_file);
		rec3d_buffer.applyMask();
		rec3d_buffer.applyLowPassFilter();
		
		rec3d_buffer.writeToFile("init_model_nn_mpi.mrc");
		
		//SingleParticle2dx::Utilities::ClusterUtilityFunctions::balanceLoad_naiv(world.size(), folder_content.size(), load);
		//SingleParticle2dx::Utilities::ClusterUtilityFunctions::balanceLoad_sizedep(world.size(), folder_content.size(), folder_content, load);
		SingleParticle2dx::Utilities::ClusterUtilityFunctions::balanceLoad_optimal_sizedep(world.size(), folder_content.size(), folder_content, load);
		
		boost::shared_ptr<FILE> load_file( fopen ( "LoadBalance.txt", "w" ), SingleParticle2dx::Utilities::CustomizedDeleter() );
		for(int i=0; i<static_cast<int>(load.size()); i++)
		{
			fprintf(load_file.get(), "%i:\t", i);
						
			for(int j=0; j<static_cast<int>(load[i].size()); j++)
			{
				fprintf(load_file.get(), "%i, ", load[i][j]);
			}
			fprintf(load_file.get(), "\n");
		}
	}
		
	boost::mpi::broadcast(world, rec3d_buffer, 0);
	boost::mpi::broadcast(world, load, 0);
	boost::mpi::broadcast(world, folder_content, 0);
	
	std::vector<std::string> container_name_vec;
	
	//#pragma omp parallel for schedule(dynamic,1)
	for(int i=0; i<static_cast<int>(load[world.rank()].size()); i++)
	{
		int index = load[world.rank()][i]; 
	//	std::cout << "index: " << index << std::endl;
		SingleParticle2dx::DataStructures::ParticleContainer cont_local;
		
		SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(folder_content[index], cont_local, true);
		
		std::vector<std::string> split_vector;
		SingleParticle2dx::Utilities::StringFunctions::splitString( split_vector, folder_content[index], std::string("/") );
		
		if(vm.count("orientations_in"))
		{
			std::string o_bin_cont = orientation_in_folder + "/" + split_vector.back();
			SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(o_bin_cont, cont_local, false);
			for(int k=0; k<cont_local.getNumberOfParticles(); k++)
			{
				cont_local(k).setOrientation(cont_local(k).getNewOrientation());
			}
			
		}
		
		cont_local.resetImageNumber(i);
		
	//	#pragma omp critical (merge_containers)
	//	{
			container_name_vec.push_back(split_vector.back());
			SingleParticle2dx::DataStructures::ParticleContainer::mergeContainers(cont, cont_local);
	//	}
	}
	
	cont.findNeighbors(load[world.rank()].size());
	
	if(vm.count("restart_in"))
	{
		std::string restart_cont_name = orientation_restart_folder_in + "/restart_cont_rank_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(world.rank()) + ".bin";
		SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(restart_cont_name, cont, false);
		
		for(int i=0; i<cont.getNumberOfParticles(); i++)
		{
			cont(i).setOrientation(cont(i).getNewOrientation());
		}
	}
	
	int number_of_particles = cont.getNumberOfParticles();
	std::cout << "size of task #" << world.rank() << ": " << number_of_particles << std::endl;
	
	int np_global;
	
	if(world.rank() == 0)
	{
		std::vector<int> n_vec;
		boost::mpi::gather(world, number_of_particles, n_vec, 0);
		
		np_global = std::accumulate(n_vec.begin(), n_vec.end(), 0);
		
		boost::shared_ptr<FILE> load_file( fopen ( "LoadBalance.txt", "a" ), SingleParticle2dx::Utilities::CustomizedDeleter() );
		for(int i=0; i<world.size(); i++)
		{
			fprintf(load_file.get(), "Size %i:\t%i\n", i, n_vec[i]);
		}			
			
		
		std::cout << "Global Number of Particles: " << np_global << std::endl;
	}
	else
	{
		gather(world, number_of_particles, 0);
	}
	
	if(world.rank() == 0)
	{
		std::cout << "LOADING DONE START TO PROCESS" << std::endl;
	}
	
	world.barrier();
	
	float t_start, t_end;
	SingleParticle2dx::Utilities::SystemTimer m_timer;
	
	
//	std::string conv_file_name = "SCRATCH/2dx_SP_convergence.txt";
//	if (world.rank() == 0)
//	{
//		SingleParticle2dx::Utilities::UtilityFunctions::removeFileIfExists(conv_file_name);
//	}
	
	for(int count=0; count<config->getMaxIteration(); count++)
	{
		world.barrier();
		
		SingleParticle2dx::real_array3d_type real_data(boost::extents[n][n][n]);
		SingleParticle2dx::real_array3d_type real_data_old(boost::extents[n][n][n]);
		
		rec3d_buffer.getRealSpaceData(real_data);
		rec3d_buffer.getRealSpaceData(real_data_old);
		
		rec3d_refine.setFourierSpaceData(real_data);
		
		world.barrier();
		
		rec3d_refine.updateReconstruction(cont, true, false, false);
		
	//	int number_of_diff_images = cont.getNumberOfDiffImages();
	//	cont.calcAndSetConsistency(number_of_diff_images);
	//	cont.selectParticlesBasedOnConsistency(number_of_diff_images);
	//	rec3d_refine.generateInitialModel(cont);
		
		rec3d_refine.getRealSpaceData(real_data);
		rec3d_buffer.setFourierSpaceData(real_data);
	
		world.barrier();
		
		t_start = m_timer.GetTime();
		
		if(world.rank() == 0)
		{
			std::vector<SingleParticle2dx::DataStructures::Reconstruction3d> rec_vec;
			std::vector<int> count_vec;
			
			boost::mpi::gather(world, rec3d_buffer, rec_vec, 0);
			
			int np = cont.getNumberOfParticles();
			boost::mpi::gather(world, np, count_vec, 0);
			
			std::cout << "rec_vec size: " << rec_vec.size() << std::endl;
			
			SingleParticle2dx::DataStructures::Reconstruction3d rec_new;
			for(int proc = 0; proc<world.size(); ++proc)
			{
				int j,k;
				
				#pragma omp parallel for private(j,k)
				for(int i=0; i<n; i++)
				{
					for(j=0; j<n; j++)
					{
						for(k=0; k<n; k++)
						{
							std::complex<float> tmp = rec_vec[proc](i,j,k);
							tmp *= count_vec[proc];
							rec_new(i,j,k) += tmp;
						}
					}
				}
			}
			
			rec_new.applyMask();
			rec_new.applyLowPassFilter();
			rec_new.writeToFile("SP_particles_only_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(count+1) + ".mrc");
			
			rec_new.getRealSpaceData(real_data);
			
			float dx = config->getModelKeep() / 100.0;
			
			for(int i=0; i<n; i++)
			{
				for(int j=0; j<n; j++)
				{
					for(int k=0; k<n; k++)
					{
						real_data_old[i][j][k] = dx*real_data_old[i][j][k] + (1-dx)*real_data[i][j][k];
					}
				}
			}
			
			rec_new.setFourierSpaceData(real_data_old);
			rec_new.applyMask();
			rec_new.applyLowPassFilter();
			
			rec3d_buffer = rec_new;
			
			rec3d_buffer.writeToFile("test3d.mrc");
			rec3d_buffer.writeToFile("SP_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(count+1) + ".mrc");
		}
		else
		{
			gather(world, rec3d_buffer, 0);
			
			int np = cont.getNumberOfParticles();
			gather(world, np, 0);
		}
		
		boost::mpi::broadcast(world, rec3d_buffer, 0);
		
		float last_change = rec3d_refine.getLastAngleChange();
		if(world.rank() == 0)
		{
			float change_global = 0;
			boost::mpi::reduce(world, last_change, change_global, std::plus<float>(), 0);
			std::cout << "Global Last Change of all Particles: " << change_global << std::endl;
			//SingleParticle2dx::Utilities::UtilityFunctions::writeConvergenceToFile(count+1, change_global/np_global);
		}
		else
		{
			boost::mpi::reduce(world, last_change, std::plus<float>(), 0);
		}
		
		writeContainerToDisk_MPI(cont, world, "stat_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(count+1) + ".txt");
		
		t_end = m_timer.GetTime();
	}
	
	
	float t_out_start = m_timer.GetTime();
	writeContainerToDisk_MPI(cont, world, "stat.txt");
	float t_out_end = m_timer.GetTime();
	
	if(world.rank() == 0)
	{
		std::cout << "REC_TIME = " << t_end-t_start << std::endl;
		std::cout << "STAT_TIME = " << t_out_end-t_out_start << std::endl;
	}
	
	std::string restart_cont_name = orientation_restart_folder_out + "/restart_cont_rank_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(world.rank()) + ".bin";
	SingleParticle2dx::DataStructures::ParticleContainer::serializeContainerToDisk(cont, false, restart_cont_name);
	
	std::vector<SingleParticle2dx::DataStructures::ParticleContainer> cont_vec;
	cont.splitIntoPerImageContainer(cont_vec);
	
	
	for(int i=0; i<static_cast<int>(cont_vec.size()); i++)
	{
		std::string fp_cont_filename = orientation_out_folder + "/" + container_name_vec[i];
		SingleParticle2dx::DataStructures::ParticleContainer::serializeContainerToDisk(cont_vec[i], false, fp_cont_filename);
	}
	
	return 0;
}
