#include "RotTrialAngleGenerator.hpp"

#include "../../Config.hpp"
#include "../../DataStructures.hpp"


void SingleParticle2dx::Methods::RotTrialAngleGenerator::generateAngles(SingleParticle2dx::DataStructures::Orientation init_o, std::vector<SingleParticle2dx::DataStructures::Orientation>& o )
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
		
	value_type dangle = config->getDAng3();
	value_type max_angle_diff = config->getMaxAng3();
		
	EMAN::Transform* t = new EMAN::Transform;
	EMAN::Transform* tloc = new EMAN::Transform;
	EMAN::Transform* tplane = new EMAN::Transform;
	
	value_type tltaxis = init_o.getTLTAXIS();
	value_type tltang = init_o.getTLTANG();
	value_type taxa = init_o.getTAXA();

	Eigen::Vector3f current_direction;

	for (value_type ang1 = -max_angle_diff; ang1<=max_angle_diff; ang1+=dangle)
	{
		//for (value_type ang2 = tltang-max_angle_diff; ang2<=tltang+max_angle_diff; ang2+=dangle)
		for (value_type ang2 = 0; ang2<=max_angle_diff; ang2+=dangle)
		{
			for (value_type ang3 = 0; ang3<=360; ang3+=(3*dangle))
			//for (value_type ang3 = taxa-max_angle_diff; ang3<=taxa+max_angle_diff; ang3+=dangle)
			{
				EMAN::Dict rot;
				rot["type"] = "spider";
				rot["phi"] = tltaxis;
				rot["theta"] = tltang;
				rot["psi"] = taxa;
				t->set_rotation(rot);

				EMAN::Dict rotloc;
				rotloc["type"] = "spider";
				rotloc["phi"] = -ang3;
				rotloc["theta"] = ang2;
				rotloc["psi"] = ang3;
				tloc->set_rotation(rotloc);
				
				EMAN::Dict rotinplane;
				rotinplane["type"] = "spider";
				rotinplane["phi"] = ang1;
				rotinplane["theta"] = 0;
				rotinplane["psi"] = 0;
				tplane->set_rotation(rotinplane);
				
				tloc->rotate(*t);
				tloc->rotate(*tplane);

				rot = tloc->get_rotation("spider");
				
				SingleParticle2dx::DataStructures::Orientation o2set;
				current_direction << static_cast<value_type>(rot["phi"]), static_cast<value_type>(rot["theta"]), static_cast<value_type>(rot["psi"]);
				o2set.setOrientation(current_direction);
				o.push_back(o2set);
			}
		}
	}
}
