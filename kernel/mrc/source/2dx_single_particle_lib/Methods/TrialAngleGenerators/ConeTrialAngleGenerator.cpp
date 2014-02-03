#include "ConeTrialAngleGenerator.hpp"
#include "../../Config.hpp"

#include "../../DataStructures.hpp"

#include <symmetry.h>

void SingleParticle2dx::Methods::ConeTrialAngleGenerator::generateAngles(SingleParticle2dx::DataStructures::Orientation init_o, std::vector<SingleParticle2dx::DataStructures::Orientation>& o )
{		
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	//value_type d_tilt = config->getEmanTrialMax();
	value_type step_size = 360/config->getEmanTrialN();
	
	o.push_back(init_o);
				
	for(value_type tilt = config->getDConeTilt(); tilt<=config->getMaxConeTilt(); tilt+=config->getDConeTilt())
	{			
		for (value_type ang1 = 0; ang1<360/4; ang1+=step_size)
		{			
			SingleParticle2dx::DataStructures::Orientation o2set = init_o;
		
			SingleParticle2dx::Utilities::EmanUtilityBindings::rotate(o2set, ang1, tilt, -ang1);
		
			for (value_type ang2 = config->getMinAng3(); ang2<=config->getMaxAng3(); ang2+=config->getDAng3())
			{
				SingleParticle2dx::DataStructures::Orientation o2set2 = o2set;
				SingleParticle2dx::Utilities::EmanUtilityBindings::rotate(o2set2, 0, ang2, 0);
				o.push_back(o2set2);
			}
		}
	}
	
	//std::cout << step_size << "\t" << config->getDConeTilt() << "\t" << config->getMaxConeTilt() << "\t" << config->getEmanTrialN() << "\t" << o.size() << std::endl;
}
