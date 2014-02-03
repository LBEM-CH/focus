#include "FullTrialAngleGenerator.hpp"
#include "../../Config.hpp"

#include "../../DataStructures.hpp"

#include <symmetry.h>

void SingleParticle2dx::Methods::FullTrialAngleGenerator::generateAngles(SingleParticle2dx::DataStructures::Orientation init_o, std::vector<SingleParticle2dx::DataStructures::Orientation>& o )
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
		
	EMAN::Dict params;
	//params["n"] = config->getEmanTrialN();
	params["delta"] = config->getEmanTrialMax();
	params["perturb"] = false;
	
	EMAN::Dict sym_params;
	sym_params["nsym"] = 1;

	EMAN::OrientationGenerator* gen;
	EMAN::Symmetry3D* sym;
	gen = EMAN::Factory<EMAN::OrientationGenerator>::get("eman", params);		
	sym = EMAN::Factory<EMAN::Symmetry3D>::get("c", sym_params);
	
	std::vector<EMAN::Transform> orients = gen->gen_orientations(sym);

	EMAN::Transform* t = new EMAN::Transform;
	EMAN::Transform* tloc = new EMAN::Transform;
	EMAN::Transform* tplane = new EMAN::Transform;
	Eigen::Vector3f current_direction;
	
	for (size_type i=0; i<static_cast<size_type>(orients.size()); i++)
	{
		EMAN::Dict ori = orients[i].get_rotation("spider");
		current_direction << static_cast<value_type>(ori["phi"]), static_cast<value_type>(ori["theta"]), static_cast<value_type>(ori["psi"]);
		SingleParticle2dx::DataStructures::Orientation o2set;
		o2set.setOrientation(current_direction);
		o.push_back(o2set);
	}
}
