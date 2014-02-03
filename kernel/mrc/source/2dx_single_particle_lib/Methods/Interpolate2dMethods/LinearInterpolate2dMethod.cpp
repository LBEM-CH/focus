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

#include "LinearInterpolate2dMethod.hpp"

SingleParticle2dx::Methods::LinearInterpolate2dMethod::LinearInterpolate2dMethod ()
{}


SingleParticle2dx::Methods::LinearInterpolate2dMethod::~LinearInterpolate2dMethod ()
{}


void SingleParticle2dx::Methods::LinearInterpolate2dMethod::interpolate2d(value_type dx, value_type dy, value_type val, std::vector<value_type>& res)
{
	res[0] = dx * dy * val;
	res[1] = (1-dx) * dy * val;
	res[2] = dx * (1-dy) * val;
	res[3] = (1-dx) * (1-dy) * val;
}