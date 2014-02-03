#include "IsoTrialAngleGenerator.hpp"
#include "../../Config.hpp"

#include "../../DataStructures.hpp"


void SingleParticle2dx::Methods::IsoTrialAngleGenerator::generateAngles(SingleParticle2dx::DataStructures::Orientation init_o, std::vector<SingleParticle2dx::DataStructures::Orientation>& o )
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	value_type dangle = config->getEmanTrialMax();
	
	Eigen::Vector3f current_direction;
	
	value_type dang = config->getDAng3();
	value_type minang = config->getMinAng3();
	value_type maxang = config->getMaxAng3();
	
	size_type mode = static_cast<size_type>(config->getEmanTrialN());
	
	
	if(mode==1)
	{
		for(value_type ang = init_o.getTLTAXIS()+minang; ang<=init_o.getTLTAXIS()+maxang; ang+=dang)
		{	
			current_direction << ang, init_o.getTLTANG(), init_o.getTAXA();
			SingleParticle2dx::DataStructures::Orientation o2set;
			o2set.setOrientation(current_direction);
			o.push_back(o2set);
		}
	}
	else if(mode==2)
	{
		for(value_type ang = init_o.getTLTANG()+minang; ang<=init_o.getTLTANG()+maxang; ang+=dang)
		{	
			current_direction << init_o.getTLTAXIS(), ang, init_o.getTAXA();
			SingleParticle2dx::DataStructures::Orientation o2set;
			o2set.setOrientation(current_direction);
			o.push_back(o2set);
		}
	}
	else if(mode==3)
	{
		for(value_type ang = init_o.getTAXA()+minang; ang<=init_o.getTAXA()+maxang; ang+=dang)
		{	
			current_direction << init_o.getTLTAXIS(), init_o.getTLTANG(), ang;
			SingleParticle2dx::DataStructures::Orientation o2set;
			o2set.setOrientation(current_direction);
			o.push_back(o2set);
		}
	}
	else
	{
		std::cerr << "unsupported mode for IsoTrialAngleGen " << mode << std::endl;
		throw std::runtime_error("Bad operation");
	}
	
	
	
}
