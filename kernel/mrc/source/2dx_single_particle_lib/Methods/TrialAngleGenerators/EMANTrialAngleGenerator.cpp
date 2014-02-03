#include "EMANTrialAngleGenerator.hpp"
#include "../../Config.hpp"

#include "../../DataStructures.hpp"

#include <symmetry.h>

void SingleParticle2dx::Methods::EMANTrialAngleGenerator::generateAngles(SingleParticle2dx::DataStructures::Orientation init_o, std::vector<SingleParticle2dx::DataStructures::Orientation>& o )
{
	
	
	/*
	d.put("delta", EMObject::FLOAT, "The angular separation of orientations in degrees. This option is mutually exclusively of the n argument.");
	d.put("perturb", EMObject::BOOL, "Whether or not to perturb the generated orientations in a small local area, default is true.");
	d.put("n", EMObject::INT, "The number of orientations to generate. This option is mutually exclusively of the delta argument.Will attempt to get as close to the number specified as possible.");
	d.put("inc_mirror", EMObject::BOOL, "Indicates whether or not to include the mirror portion of the asymmetric unit. Default is false.");
	d.put("alt_min", EMObject::FLOAT, "Minimum altitude value to include (alt=0 is Z axis). Default=0");
	d.put("alt_max", EMObject::FLOAT, "Maximum altitude value to include (alt=90 is X-Y plane). Default=no limit");
	d.put("breaksym", EMObject::BOOL, "If specified, still generates orientations filling the unit (hemi)sphere, but does it by filling one asymmetric unit, then generating all symmetric equivalents.");
	
	*/
		
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
		
	EMAN::Dict params;
//	params["n"] = config->getEmanTrialN();
	params["delta"] = config->getDConeTilt();
	params["alt_min"] = 0;
	params["alt_max"] = config->getEmanTrialMax();
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
		for (value_type ang1 = config->getMinAng3(); ang1<=config->getMaxAng3(); ang1+=config->getDAng3())
		{			
			EMAN::Dict ori = orients[i].get_rotation("spider");

			EMAN::Dict rot;
			rot["type"] = "spider";
			rot["phi"] = init_o.getTLTAXIS();
			rot["theta"] = init_o.getTLTANG();
			rot["psi"] = init_o.getTAXA();
			t->set_rotation(rot);

			EMAN::Dict rotloc;
			rotloc["type"] = "spider";
			rotloc["phi"] = ori["phi"];
			rotloc["theta"] = ori["theta"];
			rotloc["psi"] = -static_cast<value_type>(ori["phi"]);
			tloc->set_rotation(rotloc);
			
			EMAN::Dict rotinplane;
			rotinplane["type"] = "spider";
			rotinplane["phi"] = 0;// - static_cast<value_type>(ori["phi"]);
			rotinplane["theta"] = 0;
			rotinplane["psi"] = ang1;
			tplane->set_rotation(rotinplane);

			tloc->rotate(*t);
			tloc->rotate(*tplane);
		
			rot = tloc->get_rotation("spider");

			current_direction << rot["phi"], rot["theta"], rot["psi"];
			
			SingleParticle2dx::DataStructures::Orientation o2set;
			o2set.setOrientation(current_direction);
			o.push_back(o2set);

		}
	}
	
//	SingleParticle2dx::DataStructures::Orientation dummy_o;
//	o.push_back(dummy_o);
}
