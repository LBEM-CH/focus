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

#include <omp.h>

#include "FourierSpaceProjectionMethod.hpp"
#include "../../Utilities.hpp"


SingleParticle2dx::Methods::FourierSpaceProjectionMethod::FourierSpaceProjectionMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context)
{
	m_context = context;
	m_iterpolate_strategy.reset( new SingleParticle2dx::Methods::LinearInterpolate2dMethod );
}

SingleParticle2dx::Methods::FourierSpaceProjectionMethod::~FourierSpaceProjectionMethod ()
{}


void SingleParticle2dx::Methods::FourierSpaceProjectionMethod::interpolate2d(value_type dx, value_type dy, value_type val, std::vector<value_type>& res)
{
	m_iterpolate_strategy.get()->interpolate2d(dx, dy, val, res);
}


void SingleParticle2dx::Methods::FourierSpaceProjectionMethod::calculateProjection(SingleParticle2dx::DataStructures::Orientation& o, SingleParticle2dx::DataStructures::Projection2d& p)
{
	return;
}
