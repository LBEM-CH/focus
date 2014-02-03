#include <fstream>
#include <omp.h>

#include <boost/program_options.hpp>

#include "../Config/ConfigContainer.hpp"
#include "../Utilities/FFTCalculator.hpp"
#include "../Utilities/Timer.hpp"
#include "../Typedefs.hpp"


int main (int argc, char* argv[])
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	SingleParticle2dx::Utilities::SystemTimer m_timer;
	
	int n = 150;
	int N = 0;
	config->setParticleSize(n);
	
	namespace po = boost::program_options;
	po::options_description desc("Allowed options");
	desc.add_options()
	    ("help", "produce help message")
	    ("n", po::value<int>(&N), "Number of FFTS")
	;
	po::variables_map vm;
	po::store(po::parse_command_line(argc, argv, desc), vm);
	po::notify(vm);    

	if (vm.count("help") || !vm.count("n"))
	{
	    std::cout << desc << "\n";
	    return 1;
	}
	
	// Done for forcing the plan-creation
	SingleParticle2dx::real_array2d_type r_data( boost::extents[n][n] );
	SingleParticle2dx::fft_array2d_type c_data( boost::extents[n][n] );
	SingleParticle2dx::Utilities::FFTCalculator::performBackwardFFT(&c_data, &r_data);
	
	int np;
	#pragma omp parallel
	{
		np = omp_get_num_threads();
	}
	
	std::vector<SingleParticle2dx::real_array2d_type> r_vec;
	std::vector<SingleParticle2dx::fft_array2d_type> c_vec;
	
	#pragma omp parallel for
	for(int i=0; i<N; i++)
	{
		SingleParticle2dx::real_array2d_type r_data( boost::extents[n][n] );
		SingleParticle2dx::fft_array2d_type c_data( boost::extents[n][n] );
		
		r_data[0][0] = 0;
		c_data[0][0] = 0;
		
		#pragma omp critical (insert_data_for_benchmark)
		{
			r_vec.push_back(r_data);
			c_vec.push_back(c_data);
		}
	}
	
	std::cout << "FFT-Bench for " << N << " ffts on " << np << " OMP-threads" << std::endl;
	
	float t_start = m_timer.GetTime();

	#pragma omp parallel for
	for(int i=0; i<N; i++)
	{
		SingleParticle2dx::Utilities::FFTCalculator::performBackwardFFT(&c_vec[i], &r_vec[i]);
	}

	float t_used = m_timer.GetTime() - t_start;
	
	std::cout << "time for bench (CPU): " << t_used << std::endl;
	std::cout << "time for one fft (CPU): " << t_used/N << std::endl;
}
