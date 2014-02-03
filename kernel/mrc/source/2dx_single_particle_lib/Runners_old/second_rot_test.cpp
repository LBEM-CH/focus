#include "../2dxSingleParticle.hpp"
#include <projector.h>

int main ()
{
	SingleParticle2dx::DataStructures::Orientation o(60,15,-80);
	
	std::cout << "rot original: " << o.getDataString() << std::endl;
	
	SingleParticle2dx::Utilities::EmanUtilityBindings::rotate(o, 0, 180, 0);
	
	std::cout << "rot new: " << o.getDataString() << std::endl;
	
	return 0;
	
}
