#include "TiltAxisTrialAngleGenerator.hpp"
#include "../../Config.hpp"

#include "../../DataStructures.hpp"


void SingleParticle2dx::Methods::TiltAxisTrialAngleGenerator::generateAngles(SingleParticle2dx::DataStructures::Orientation init_o, std::vector<SingleParticle2dx::DataStructures::Orientation>& o )
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	value_type dangle = config->getEmanTrialMax();
	
	Eigen::Vector3f current_direction;
	
	value_type tangl_max = config->getMaxConeTilt();
	value_type tangl_d   = config->getDConeTilt();
	
	size_type mode = config->getEmanTrialN();


	value_type init_ang;
	if(mode == 1)
		{
			init_ang = init_o.getTLTAXIS();
		}
		else if(mode == 2)
		{
			init_ang = init_o.getTLTANG();
		}
		else if(mode == 3)
		{
			init_ang = init_o.getTAXA();
		}
		else
		{
			throw;
		}
	
	for(value_type ang = init_ang-tangl_max; ang<=init_ang+tangl_max; ang+=tangl_d)
	{
		if(mode == 1)
		{
			current_direction << ang, init_o.getTLTANG(), init_o.getTAXA();
		}
		else if(mode == 2)
		{
			current_direction << init_o.getTLTAXIS(), ang, init_o.getTAXA();
		}
		else if(mode == 3)
		{
			current_direction << init_o.getTLTAXIS(), init_o.getTLTANG(), ang;
		}
		else
		{
			throw;
		}
		
		
		SingleParticle2dx::DataStructures::Orientation o2set;
		o2set.setOrientation(current_direction);
		o.push_back(o2set);
	}
}
