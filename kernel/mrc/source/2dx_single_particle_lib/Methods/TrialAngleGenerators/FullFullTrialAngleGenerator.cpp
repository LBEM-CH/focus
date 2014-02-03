#include "FullFullTrialAngleGenerator.hpp"
#include "../../Config.hpp"

#include "../../DataStructures.hpp"

#include <symmetry.h>

void SingleParticle2dx::Methods::FullFullTrialAngleGenerator::generateAngles(SingleParticle2dx::DataStructures::Orientation init_o, std::vector<SingleParticle2dx::DataStructures::Orientation>& o )
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	value_type delta = config->getEmanTrialMax();
	
	Eigen::Vector3f current_direction;
	
	value_type tltaxis, tltang, taxa;
	
	std::vector<value_type> tltaxis_vector;
	for(size_type tltaxis = 0; tltaxis < 360; tltaxis+=delta)
	{
		tltaxis_vector.push_back(tltaxis);
	}
	
	#pragma omp parallel for private(tltaxis, tltang, taxa, current_direction) schedule(dynamic,1)
	for(size_type i = 0; i<static_cast<size_type>(tltaxis_vector.size()); i++)
	{
		tltaxis = tltaxis_vector[i];
		for(tltang = -180; tltang <= 180; tltang+=delta)
		{
			if ( (fabs(tltang)<50) || (tltang>=130) || (tltang<=-130) )
			{
				for(taxa = 0; taxa < 90; taxa+=delta)
				{
					current_direction << tltaxis, tltang, taxa;
					SingleParticle2dx::DataStructures::Orientation o2set;
					o2set.setOrientation(current_direction);
					
					#pragma omp critical (insert_angle_to_full_full_sampling)
					{
						o.push_back(o2set);
					}
				}
			}
		}
	}
}
