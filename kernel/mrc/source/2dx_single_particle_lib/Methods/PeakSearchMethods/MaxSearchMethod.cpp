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

#include "MaxSearchMethod.hpp"
#include <limits>


SingleParticle2dx::Methods::MaxSearchMethod::MaxSearchMethod ()
{}


SingleParticle2dx::Methods::MaxSearchMethod::~MaxSearchMethod ()
{}


SingleParticle2dx::Methods::MaxSearchMethod::value_type SingleParticle2dx::Methods::MaxSearchMethod::findMaximalElementAndSetShift(real_array2d_type& cc_array, SingleParticle2dx::DataStructures::ParticleShift& shift)
{
	size_type max_i = -1;
	size_type max_j = -1;
	
	value_type max_value = -1000.0;
			
	for (size_type i=0; i<static_cast<size_type>(cc_array.shape()[0]); i++)
	{
		for (size_type j=0; j<static_cast<size_type>(cc_array.shape()[1]); j++)
		{
			if ( cc_array[i][j] > max_value )
			{
				max_i = i;
				max_j = j;
				max_value = cc_array[i][j];		
			}
		}
	}
	
	if ( (max_i == -1) || (max_j == -1) )
	{
		std::cerr << "bad cc map: " << cc_array[0][0] << std::endl;
		throw std::runtime_error("Bad CC map thus not using particle");
	}
	
	shift.setShiftX ( max_i - static_cast<int>(cc_array.shape()[0]/2) );
	shift.setShiftY ( max_j - static_cast<int>(cc_array.shape()[1]/2) );
	
	return max_value;
}
