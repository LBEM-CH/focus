#include "FixAxisTrialAngleGenerator.hpp"
#include "../../Config.hpp"

#include "../../DataStructures.hpp"

#include <symmetry.h>

void SingleParticle2dx::Methods::FixAxisTrialAngleGenerator::generateAngles(SingleParticle2dx::DataStructures::Orientation init_o, std::vector<SingleParticle2dx::DataStructures::Orientation>& o )
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	value_type delta = config->getEmanTrialMax();
	
	Eigen::Vector3f current_direction;
	
	value_type tltaxis = init_o.getTLTAXIS();
	
	for(value_type tltaxis = init_o.getTLTAXIS() - config->getEmanTrialN(); tltaxis <= init_o.getTLTAXIS() + config->getEmanTrialN(); tltaxis+=delta)
	{
		for(value_type tltang = -180; tltang <= 180; tltang+=delta)
		{
			for(value_type taxa = 0; taxa < 90; taxa+=delta)
			{
				current_direction << tltaxis, tltang, taxa;
				SingleParticle2dx::DataStructures::Orientation o2set;
				o2set.setOrientation(current_direction);
				o.push_back(o2set);
			}
		}
	}
}
