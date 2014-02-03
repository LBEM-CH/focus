#include "DummyTrialAngleGenerator.hpp"
#include "../../Config.hpp"

#include "../../DataStructures.hpp"


void SingleParticle2dx::Methods::DummyTrialAngleGenerator::generateAngles(SingleParticle2dx::DataStructures::Orientation init_o, std::vector<SingleParticle2dx::DataStructures::Orientation>& o )
{
//	SingleParticle2dx::DataStructures::Orientation o_default;
//	o.push_back(o_default);
	
	o.push_back(init_o);
}
