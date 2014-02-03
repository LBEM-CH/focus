/*
 *  Copyright (C) 2012 by C-Cina University of Basel
 *  www.c-cina.unibas.ch
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the
 *  Free Software Foundation, Inc.,
 *  59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */


#include "DummyRefinementMethod.hpp"


SingleParticle2dx::Methods::DummyRefinementMethod::DummyRefinementMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context)
{
	m_context = context;
}


SingleParticle2dx::Methods::DummyRefinementMethod::~DummyRefinementMethod ()
{}


void SingleParticle2dx::Methods::DummyRefinementMethod::updateReconstruction(SingleParticle2dx::DataStructures::ParticleContainer& c, bool useneighbors, bool write_debug_output)
{
	int dummy = c.getNumberOfParticles() + 1; //dummy operation to supress warning
	std::cout << "WARNING: Using dummy refinement strategy, which is designed for testing only. No angle will be changed" << std::endl;
	
/*
	SingleParticle2dx::DataStructures::Orientation o;
	o.setTLTAXIS(0);
	o.setTLTANG(0);
	o.setTAXA(0);
	
	SingleParticle2dx::DataStructures::Projection2d p(m_context->getSizeX(), m_context->getSizeY());
	
	m_context->calculateProjection(o, p);
	
	p.writeRealSpaceProjectionToFile("new_projection_test.mrc");
*/	
	return;
}


