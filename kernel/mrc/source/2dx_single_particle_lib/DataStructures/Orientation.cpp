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


#include "Orientation.hpp"

#include "../Config.hpp"
#include "../Utilities/UtilityFunctions.hpp"


SingleParticle2dx::DataStructures::Orientation::Orientation ()
{
	for(size_type i=0; i<3; i++)
	{
		m_angles.push_back(value_type(0));
	}
}


SingleParticle2dx::DataStructures::Orientation::Orientation (value_type tltaxis, value_type tltang, value_type taxa)
{
	m_angles.push_back(value_type(tltaxis));
	m_angles.push_back(value_type(tltang));
	m_angles.push_back(value_type(taxa));
	(*this).normalize();
}


SingleParticle2dx::DataStructures::Orientation::Orientation (value_array_type rhs)
 : m_angles ( rhs )
{
	(*this).normalize();
}


SingleParticle2dx::DataStructures::Orientation::~Orientation ()
{}
 

SingleParticle2dx::DataStructures::Orientation::Orientation (Orientation const& rhs)
 : m_angles ( rhs.m_angles )
{}


SingleParticle2dx::DataStructures::Orientation& SingleParticle2dx::DataStructures::Orientation::operator= (Orientation rhs)
{
	swap (*this, rhs);
	return *this;
}


SingleParticle2dx::DataStructures::Orientation::value_type SingleParticle2dx::DataStructures::Orientation::getTLTAXIS () const
{
	return m_angles[0];
}


void SingleParticle2dx::DataStructures::Orientation::setTLTAXIS (const value_type tltaxis)
{
	m_angles[0] = tltaxis;
	(*this).normalize();
}


SingleParticle2dx::DataStructures::Orientation::value_type SingleParticle2dx::DataStructures::Orientation::getTLTANG () const
{
	return m_angles[1];
}


void SingleParticle2dx::DataStructures::Orientation::setTLTANG (const value_type tltang)
{
	m_angles[1] = tltang;
	(*this).normalize();
}


SingleParticle2dx::DataStructures::Orientation::value_type SingleParticle2dx::DataStructures::Orientation::getTAXA () const
{
	return m_angles[2];
}


void SingleParticle2dx::DataStructures::Orientation::setTAXA (const value_type taxa)
{
	m_angles[2] = taxa;
	(*this).normalize();
}


void SingleParticle2dx::DataStructures::Orientation::setOrientation(value_array_type rhs)
{
	m_angles = rhs;
	(*this).normalize();
}


void SingleParticle2dx::DataStructures::Orientation::setOrientation(Eigen::Vector3f rhs)
{
	m_angles[0] = rhs[0];
	m_angles[1] = rhs[1];
	m_angles[2] = rhs[2];
	(*this).normalize();
}


SingleParticle2dx::DataStructures::Orientation::value_array_type SingleParticle2dx::DataStructures::Orientation::getOrientation()
{
	return m_angles;
}


bool SingleParticle2dx::DataStructures::Orientation::operator== (Orientation rhs)
{
	if ( (m_angles[0] == rhs.m_angles[0]) && (m_angles[1] == rhs.m_angles[1]) && (m_angles[2] == rhs.m_angles[2]) )
	{
		return true;
	}
	return false;
}


void SingleParticle2dx::DataStructures::Orientation::normalize()
{
//	m_angles[0] = fmod(m_angles[0], 360);
//	m_angles[1] = fmod(m_angles[1], 360);
//	m_angles[2] = fmod(m_angles[2], 360);
}


std::string SingleParticle2dx::DataStructures::Orientation::getDataString()
{
	std::string result = SingleParticle2dx::Utilities::StringFunctions::TtoString(m_angles[0]);
	result += "\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString(m_angles[1]);
	result += "\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString(m_angles[2]);
	return result;
}


std::string SingleParticle2dx::DataStructures::Orientation::getDescString()
{
	return "tiltaxis\ttltang\ttaxa";
}


namespace SingleParticle2dx
{
	namespace DataStructures
	{
		void swap (Orientation& o1, Orientation& o2)
		{
			std::swap (o1.m_angles, o2.m_angles);
		}
		
	} /* DataStructures */
	
} /* SingleParticle2dx */
