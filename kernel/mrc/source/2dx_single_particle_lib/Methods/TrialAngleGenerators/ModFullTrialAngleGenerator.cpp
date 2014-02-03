#include "ModFullTrialAngleGenerator.hpp"
#include "../../Config.hpp"

#include "../../DataStructures.hpp"

#include <boost/math/special_functions/round.hpp>

#include <symmetry.h>

void SingleParticle2dx::Methods::ModFullTrialAngleGenerator::generateAngles(SingleParticle2dx::DataStructures::Orientation init_o, std::vector<SingleParticle2dx::DataStructures::Orientation>& o )
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	float delta = config->getEmanTrialMax();
	float d_tilt = 2;
	
	Eigen::Vector3f current_direction;
	
	int num=((static_cast<int>(init_o.getTLTANG())+1)/2)*1;
	
	for(float tltaxis = 0; tltaxis < 360; tltaxis+=delta)
	{
		//for(float tltang = num-4; tltang<=num+4; tltang+=2)
		for(float tltang = init_o.getTLTANG(); tltang<100; tltang+=200)
		{
			for(float taxa = 0; taxa < 90; taxa+=delta)
			{
				current_direction << tltaxis, tltang, taxa;
				SingleParticle2dx::DataStructures::Orientation o2set;
				o2set.setOrientation(current_direction);
				o.push_back(o2set);
				
				current_direction << tltaxis, -tltang, taxa;
				o2set.setOrientation(current_direction);
				o.push_back(o2set);
			}
		}
	}
}
