#include "../2dxSingleParticle.hpp"

int main (int argc, char const *argv[])
{
	SingleParticle2dx::Methods::EMANTrialAngleGenerator gen;
	
	std::vector<SingleParticle2dx::DataStructures::Orientation> o_vec;
	SingleParticle2dx::DataStructures::Orientation init_o(30, 30, 30);
	
	gen.generateAngles(init_o, o_vec );
	
	for (int i=0; i<static_cast<int>(o_vec.size()); i++)
	{
		std::cout << o_vec[i].getDataString() << std::endl;
	}
	
	std::cout << o_vec.size() << " trial angles were generated" << std::endl;
	
	std::cout << "all clear, over and out" << std::endl;
	
	return 0;
}