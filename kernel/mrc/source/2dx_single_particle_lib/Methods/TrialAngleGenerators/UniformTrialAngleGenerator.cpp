#include "UniformTrialAngleGenerator.hpp"
#include "../../Config.hpp"

#include "../../DataStructures.hpp"


void SingleParticle2dx::Methods::UniformTrialAngleGenerator::generateAngles(SingleParticle2dx::DataStructures::Orientation init_o, std::vector<SingleParticle2dx::DataStructures::Orientation>& o )
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	SingleParticle2dx::DataStructures::Orientation::value_array_type init_direction = init_o.getOrientation();
	
	value_type dang1 = config->getDAng1();
	value_type minang1 = config->getMinAng1();
	value_type maxang1 = config->getMaxAng1();

	value_type dang2 = config->getDAng2();
	value_type minang2 = config->getMinAng2();
	value_type maxang2 = config->getMaxAng2();

	value_type dang3 = config->getDAng3();
	value_type minang3 = config->getMinAng3();
	value_type maxang3 = config->getMaxAng3();
	
	Eigen::Vector3f current_direction;
		
	for (value_type ang1 = init_direction[0]+minang3; ang1<=init_direction[0]+maxang3; ang1+=dang3)
	{
		for (value_type ang2 = init_direction[1]+minang3; ang2<=init_direction[1]+maxang3; ang2+=dang3)
		{
			for (value_type ang3 = init_direction[2]+minang3; ang3<=init_direction[2]+maxang3; ang3+=dang3)
			{
				current_direction << ang1, ang2, ang3;
				SingleParticle2dx::DataStructures::Orientation o2set;
				o2set.setOrientation(current_direction);
				o.push_back(o2set);
			}
		}
	}
	
	//SingleParticle2dx::DataStructures::Orientation o_dummy;
	//o.push_back(o_dummy);
	
}
