#include "Dummy3dMaskingMethod.hpp"

SingleParticle2dx::Methods::Dummy3dMaskingMethod::Dummy3dMaskingMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context)
{
	m_context = context;
}
	
	
SingleParticle2dx::Methods::Dummy3dMaskingMethod::~Dummy3dMaskingMethod ()
{}
	

void SingleParticle2dx::Methods::Dummy3dMaskingMethod::apply3dMask()
{
	return;
}