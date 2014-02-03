#include "../2dxSingleParticle.hpp"

#include <fstream>

#include <omp.h>

#include <numeric>

#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>
#include <boost/mpi.hpp>



int main(int argc, char* argv[])
{
	boost::mpi::environment env(argc, argv);
	boost::mpi::communicator world;

	namespace po = boost::program_options;
	
	typedef boost::archive::binary_iarchive archive_in_type;
	typedef boost::archive::binary_oarchive archive_out_type;
	
	std::string binary_config_file;
	std::string binary_particle_file;
	std::string orientation_restart_folder;
	
	po::options_description desc("Allowed options");
	desc.add_options()
	    ("help", "produce help message")
	    ("config", po::value<std::string>(&binary_config_file), "Config container file")
		("particles", po::value<std::string>(&binary_particle_file), "Particle container file")
		("restart", po::value<std::string>(&orientation_restart_folder), "Restart folder")
	;
	
	po::variables_map vm;
	po::store(po::parse_command_line(argc, argv, desc), vm);
	po::notify(vm);    

	if (vm.count("help")) {
	    cout << desc << "\n";
	    return 1;
	}
	
	int n;
	
	if ( !vm.count("config") || !vm.count("particles") || !vm.count("restart") )
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
	SingleParticle2dx::DataStructures::ParticleContainer cont_even;
	SingleParticle2dx::DataStructures::ParticleContainer cont_odd;
	
	SingleParticle2dx::ConfigContainer* config;
	
	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Restoring Container and Config from binary files...", 2);
	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Restoring Config from binary file...", 1);
	{
        std::ifstream ifs(binary_config_file.c_str());
        archive_in_type ia(ifs);
        ia >> config;
	}

	n = config->getParticleSize();
	
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d_buffer_even(n,n,n);
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d_buffer_odd(n,n,n);
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d_refine_even(n,n,n);
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d_refine_odd(n,n,n);

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
		
	boost::mpi::broadcast(world, load, 0);
	boost::mpi::broadcast(world, folder_content, 0);
	
//	#pragma omp parallel for schedule(dynamic,1)
	for(int i=0; i<static_cast<int>(load[world.rank()].size()); i++)
	{
		int index = load[world.rank()][i]; 
		std::cout << "index: " << index << std::endl;
		SingleParticle2dx::DataStructures::ParticleContainer cont_local;
		SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(folder_content[index], cont_local, true);
		
	//	std::vector<std::string> split_vector;
	//	SingleParticle2dx::Utilities::StringFunctions::splitString( split_vector, folder_content[index], std::string("/") );
	//	std::string o_bin_cont = orientation_in_folder + "/" + split_vector.back();
	//	SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(o_bin_cont, cont_local, false);
		
		#pragma omp critical (merge_containers)
		{
			SingleParticle2dx::DataStructures::ParticleContainer::mergeContainers(cont, cont_local);
		}
	}
	
	std::string restart_cont_name = orientation_restart_folder + "/restart_cont_rank_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(world.rank()) + ".bin";
	SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(restart_cont_name, cont, false);
	
	int number_of_particles = cont.getNumberOfParticles();
	std::cout << "size of task #" << world.rank() << ": " << number_of_particles << std::endl;
	
	if(world.rank() == 0)
	{
		std::vector<int> n_vec;
		boost::mpi::gather(world, number_of_particles, n_vec, 0);
		
		int np_global = std::accumulate(n_vec.begin(), n_vec.end(), 0);
		
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
	
	world.barrier();
	
	float t_start, t_end;
	
	world.barrier();

	
	SingleParticle2dx::DataStructures::ParticleContainer::splitContainerMemSaving(cont, cont_even, cont_odd);
		
	rec3d_refine_even.generateInitialModel(cont_even);
	rec3d_refine_odd.generateInitialModel(cont_odd);
	
	std::cout << "hi after init model done" << std::endl;
		
	SingleParticle2dx::real_array3d_type real_data(boost::extents[n][n][n]);
	
	rec3d_refine_even.getRealSpaceData(real_data);
	rec3d_buffer_even.setFourierSpaceData(real_data);
	
	rec3d_refine_odd.getRealSpaceData(real_data);
	rec3d_buffer_odd.setFourierSpaceData(real_data);
	
	SingleParticle2dx::Utilities::SystemTimer m_timer;
	t_start = m_timer.GetTime();
	
	std::cout << "hi before gather from " << world.rank() << std::endl;
		
	if(world.rank() == 0)
	{
		std::vector<SingleParticle2dx::DataStructures::Reconstruction3d> rec_vec_even;
		std::vector<SingleParticle2dx::DataStructures::Reconstruction3d> rec_vec_odd;
		
		std::vector<int> count_vec_even;
		std::vector<int> count_vec_odd;
		
		world.barrier();
		
		boost::mpi::gather(world, rec3d_buffer_even, rec_vec_even, 0);
		boost::mpi::gather(world, rec3d_buffer_odd, rec_vec_odd, 0);
		
		int np_even = cont_even.getNumberOfParticles();
		boost::mpi::gather(world, np_even, count_vec_even, 0);
		
		int np_odd = cont_odd.getNumberOfParticles();
		boost::mpi::gather(world, np_odd, count_vec_odd, 0);
		
		world.barrier();
		
		std::cout << "rec_vec_even size: " << rec_vec_even.size() << std::endl;
		std::cout << "rec_vec_odd size: " << rec_vec_odd.size() << std::endl;
		
		SingleParticle2dx::DataStructures::Reconstruction3d rec_new_even;
		SingleParticle2dx::DataStructures::Reconstruction3d rec_new_odd;
		
		for(int proc = 0; proc<world.size(); ++proc)
		{
			std::cout << "proc: " << proc << " of " << world.size() << std::endl;
			int j,k;
			#pragma omp parallel for private(j,k)
			for(int i=0; i<n; i++)
			{
				for(j=0; j<n; j++)
				{
					for(k=0; k<n; k++)
					{
						std::complex<float> tmp = rec_vec_even[proc](i,j,k);
						tmp *= count_vec_even[proc];
						rec_new_even(i,j,k) += tmp;
						
						tmp = rec_vec_odd[proc](i,j,k);
						tmp *= count_vec_odd[proc];
						rec_new_odd(i,j,k) += tmp;
					}
				}
			}
		}
		
		std::cout << "assembly done" << std::endl;
		
		rec3d_buffer_even = rec_new_even;
		rec3d_buffer_odd = rec_new_odd;
		
	}
	else
	{
		world.barrier();
		gather(world, rec3d_buffer_even, 0);
		gather(world, rec3d_buffer_odd, 0);
		
		int np_even = cont_even.getNumberOfParticles();
		gather(world, np_even, 0);
		
		int np_odd = cont_odd.getNumberOfParticles();
		gather(world, np_odd, 0);
		
		world.barrier();
	}
	
	world.barrier();
		
	t_end = m_timer.GetTime();
	
	if(world.rank() == 0)
	{
		std::cout << "REC_TIME = " << t_end-t_start << std::endl;
		
		std::pair<std::vector<float>, std::vector<float> > fsc = SingleParticle2dx::DataStructures::ParticleContainer::calculateFSC( rec3d_buffer_even, rec3d_buffer_odd, false);
		std::pair<std::vector<float>, std::vector<float> > fsc_xy = SingleParticle2dx::DataStructures::ParticleContainer::calculateFSC_XY( rec3d_buffer_even, rec3d_buffer_odd, false);
		std::pair<std::vector<float>, std::vector<float> > fsc_z = SingleParticle2dx::DataStructures::ParticleContainer::calculateFSC_Z( rec3d_buffer_even, rec3d_buffer_odd, false);
		
		int res_radius = SingleParticle2dx::Utilities::UtilityFunctions::getGoldStandardFSCRadius(fsc);
		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Gold FSC Radius: " + SingleParticle2dx::Utilities::StringFunctions::TtoString(res_radius), 1);
		
		rec3d_buffer_even.writeToFile("volume_even.mrc");
		rec3d_buffer_odd.writeToFile("volume_odd.mrc");
	}
	
	return 0;
}
