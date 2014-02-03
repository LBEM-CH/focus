#include "../2dxSingleParticle.hpp"


#include <boost/random/normal_distribution.hpp>
#include <boost/random/mersenne_twister.hpp>
#include <boost/random/variate_generator.hpp>
#include <boost/random/uniform_real.hpp>
#include <boost/random.hpp>
#include <boost/generator_iterator.hpp>

#include <boost/program_options.hpp>

#include <boost/mpi.hpp>

float getPI(int n)
{
	boost::mt19937 baseGen(time(0));
	boost::uniform_real<> uniDblUnit(0,1);
	boost::variate_generator<boost::mt19937&, boost::uniform_real<> > generator(baseGen, uniDblUnit);
	
	int count_inside = 0;
	float x,y;
	
	for(int i=0; i<n; i++)
	{
		x = generator();
		y = generator();
		
		//x = ((float) rand() / (RAND_MAX)) + 1;
		//y = ((float) rand() / (RAND_MAX)) + 1;
		
		if ( (x*x + y*y) < 1)
		{
			count_inside++;
		}
	}
	
	return 4.0*count_inside/n;
}


int main(int argc, char* argv[])
{
	boost::mpi::environment env(argc, argv);
	boost::mpi::communicator world;
	
	namespace po = boost::program_options;
	
	int n;
	int ns;
	
	po::options_description desc("Allowed options");
	desc.add_options()
	    ("help", "produce help message")
	    ("n", po::value<int>(&n), "Number of points")
	    ("ns", po::value<int>(&ns), "Number of rounds")
	;
	
	po::variables_map vm;
	po::store(po::parse_command_line(argc, argv, desc), vm);
	po::notify(vm);    

	if (vm.count("help") || !vm.count("n")  || !vm.count("ns"))
	{
	    cout << desc << "\n";
	    return 1;
	}
	
	
	SingleParticle2dx::Utilities::SystemTimer m_timer;
	float t_start = m_timer.GetTime();
	
	int n_samp = ns;
	float result = 0;
	
	#pragma omp parallel for
	for(int i=0; i<n_samp; i++)
	{
		result += getPI(n)/n_samp;
	}
	
	
	if(world.rank() == 0)
	{
		std::vector<float> pi_vec;

		boost::mpi::gather(world, result, pi_vec, 0);
		
		float global_result = 0;
		for(int proc = 0; proc<world.size(); ++proc)
		{
			global_result += pi_vec[proc];
		}
		
		std::cout << "GLOBAL RESULT: " << global_result/world.size() << std::endl;
	}
	else
	{
		gather(world, result, 0);		
	}
	
	float t_used = m_timer.GetTime() - t_start;
	
	//std::cout << "result: " << result << std::endl;
	//std::cout << "time for test " << t_used << std::endl;
	//std::cout << "time for one round " << t_used/ns << std::endl;
	
	return 0;
}
