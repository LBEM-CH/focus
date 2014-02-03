#include "InPlaneTrialAngleGenerator.hpp"
#include "../../Config.hpp"

#include "../../DataStructures.hpp"


void SingleParticle2dx::Methods::InPlaneTrialAngleGenerator::generateAngles(SingleParticle2dx::DataStructures::Orientation init_o, std::vector<SingleParticle2dx::DataStructures::Orientation>& o )
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	value_type dangle = config->getDAng1();
	
	Eigen::Vector3f current_direction;
		
	for (value_type ang1 = 0; ang1<360; ang1+=dangle)
	{
		current_direction << 0, 0, ang1;
		SingleParticle2dx::DataStructures::Orientation o2set;
		o2set.setOrientation(current_direction);
		o.push_back(o2set);
	}
}
