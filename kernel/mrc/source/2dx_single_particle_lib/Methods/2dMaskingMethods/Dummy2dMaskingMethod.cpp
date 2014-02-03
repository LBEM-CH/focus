#include "Dummy2dMaskingMethod.hpp"

SingleParticle2dx::Methods::Dummy2dMaskingMethod::Dummy2dMaskingMethod (SingleParticle2dx::DataStructures::Abstract2dData* context)
{
	m_context = context;
}
	
	
SingleParticle2dx::Methods::Dummy2dMaskingMethod::~Dummy2dMaskingMethod ()
{
	
}
	

void SingleParticle2dx::Methods::Dummy2dMaskingMethod::apply2dMask()
{
	return;
}