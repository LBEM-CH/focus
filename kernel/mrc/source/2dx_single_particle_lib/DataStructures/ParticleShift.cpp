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


#include "ParticleShift.hpp"
#include "../Utilities/UtilityFunctions.hpp"



SingleParticle2dx::DataStructures::ParticleShift::ParticleShift ()
 : m_data ( value_array_type(0,0) )
{}


SingleParticle2dx::DataStructures::ParticleShift::ParticleShift (value_type x, value_type y)
 : m_data ( value_array_type(x,y) )
{}


SingleParticle2dx::DataStructures::ParticleShift::ParticleShift (value_array_type rhs)
 : m_data(rhs)
{}


SingleParticle2dx::DataStructures::ParticleShift::~ParticleShift ()
{}


SingleParticle2dx::DataStructures::ParticleShift::ParticleShift (ParticleShift const& rhs)
 : m_data ( rhs.m_data )
{}


SingleParticle2dx::DataStructures::ParticleShift& SingleParticle2dx::DataStructures::ParticleShift::operator= (ParticleShift rhs)
{
	swap (*this, rhs);
	return *this;
}


SingleParticle2dx::DataStructures::ParticleShift::value_type SingleParticle2dx::DataStructures::ParticleShift::getShiftX () const
{
	return m_data.first;
}


void SingleParticle2dx::DataStructures::ParticleShift::setShiftX (const value_type s)
{
	m_data.first = s;
}


SingleParticle2dx::DataStructures::ParticleShift::value_type SingleParticle2dx::DataStructures::ParticleShift::getShiftY() const
{
	return m_data.second;
}


void SingleParticle2dx::DataStructures::ParticleShift::setShiftY (const value_type s)
{
	m_data.second = s;
}

std::string SingleParticle2dx::DataStructures::ParticleShift::getDataString()
{
	std::string result = SingleParticle2dx::Utilities::StringFunctions::TtoString(m_data.first);
	result += "\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString(m_data.second);
	return result;
}


std::string SingleParticle2dx::DataStructures::ParticleShift::getDescString()
{
	return "shift_x\tshift_y";
}


void SingleParticle2dx::DataStructures::ParticleShift::reset()
{
	m_data.first = 0;
	m_data.second = 0;
}


namespace SingleParticle2dx
{
	namespace DataStructures
	{
		void swap (ParticleShift& s1, ParticleShift& s2)
		{
			std::swap(s1.m_data, s2.m_data);
		}
		
	} /* DataStructures */
	
} /* SingleParticle2dx */
