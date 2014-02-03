#include "../2dxSingleParticle.hpp"
#include <projector.h>

int main (int argc, char const *argv[])
{
	/* code */
	
	typedef SingleParticle2dx::value_type value_type;
	
	EMAN::Transform* t = new EMAN::Transform;
	EMAN::Transform* tloc = new EMAN::Transform;
	EMAN::Transform* tplane = new EMAN::Transform;
	

	value_type max_angle_diff = 10;
	value_type dangle = 5;
	
	std::string filename = "angle_file.txt";
	boost::shared_ptr<FILE> file( fopen ( filename.c_str(), "w" ), SingleParticle2dx::Utilities::CustomizedDeleter() );

	float tltaxis =  0;
	float tltang  =  0;
	float taxa    =  100;

	for(value_type x=-1; x<=1; x+=0.1)
	{
		for(value_type y=-1; y<=1; y+=0.1)
		{
			EMAN::Dict rot;

			rot["type"] = "spider";
			rot["phi"] = tltaxis;
			rot["theta"] = tltang;
			rot["psi"] = taxa;

			t->set_rotation(rot);

			EMAN::Vec3f res = t->transform(x, y, 0);

			fprintf(file.get(), "%f\t%f\t%f\n", res[0], res[1], res[2]);
		}
	}

	float x=0;
	float y=0;
	float z=1;	

	
	//for (value_type ang1 = 0; ang1<=20; ang1+=dangle)
	//for (value_type ang1 = tltaxis-max_angle_diff; ang1<=tltaxis+max_angle_diff; ang1+=dangle)
	for (value_type ang1 = -max_angle_diff; ang1<=max_angle_diff; ang1+=dangle)
	{
		//for (value_type ang2 = tltang-max_angle_diff; ang2<=tltang+max_angle_diff; ang2+=dangle)
		for (value_type ang2 = 0; ang2<=max_angle_diff; ang2+=dangle)
		{
			for (value_type ang3 = 0; ang3<=360; ang3+=dangle)
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

				EMAN::Vec3f res = tloc->transform(0, 0, 1);
				//res = t->transform(res);
				res.normalize();
				fprintf(file.get(), "%f\t%f\t%f\n", res[0], res[1], res[2]);

				//x=0;
				//y=1;
				//res = t->transform(x, y, z);
				//res.normalize();
				//fprintf(file.get(), "%f\t%f\t%f\n", res[0], res[1], res[2]);

				rot = tloc->get_rotation("spider");
				std::cout << float(rot["phi"]) << " " << float(rot["theta"]) << " " << float(rot["psi"]) << " "  << (float(rot["phi"]) + float(rot["psi"])) << std::endl;
			}
		}
	}
	
	
	
	
	
	delete t;
	return 0;
}
