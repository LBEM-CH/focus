#include "EmanUtilityBindings.hpp"

#include <projector.h>

void SingleParticle2dx::Utilities::EmanUtilityBindings::rotate(SingleParticle2dx::DataStructures::Orientation& o, value_type d_phi, value_type d_theta, value_type d_psi)
{
	EMAN::Transform* t1 = new EMAN::Transform;
	EMAN::Transform* t2 = new EMAN::Transform;
	
	EMAN::Dict rot;
	rot["type"] = "spider";
	rot["phi"] = o.getTLTAXIS();
	rot["theta"] = o.getTLTANG();
	rot["psi"] = o.getTAXA();
	
	t1->set_rotation(rot);
	
	rot["phi"] = d_phi;
	rot["theta"] = d_theta;
	rot["psi"] = d_psi;
	
	t2->set_rotation(rot);
	
	t1->rotate(*t2);
	
	rot = t1->get_rotation("spider");
	o.setTLTAXIS(float(rot["phi"]));
	o.setTLTANG(float(rot["theta"]));
	o.setTAXA(float(rot["psi"]));
	
	delete t1;
	delete t2;
}
