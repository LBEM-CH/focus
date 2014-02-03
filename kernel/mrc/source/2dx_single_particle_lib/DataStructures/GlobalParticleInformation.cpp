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

#include "GlobalParticleInformation.hpp"

#include "../Utilities/UtilityFunctions.hpp"


SingleParticle2dx::DataStructures::GlobalParticleInformation::GlobalParticleInformation ()
 : m_image_number ( 0 ),
   m_position (position_array_type(0,0) )
{}


SingleParticle2dx::DataStructures::GlobalParticleInformation::GlobalParticleInformation(number_type nr, position_type x1, position_type x2)
 : m_image_number ( nr ),
   m_position ( position_array_type(x1,x2) )
{}


SingleParticle2dx::DataStructures::GlobalParticleInformation::~GlobalParticleInformation ()
{}


SingleParticle2dx::DataStructures::GlobalParticleInformation::GlobalParticleInformation (GlobalParticleInformation const& rhs)
 : m_image_number ( rhs.m_image_number ),
   m_position ( rhs.m_position ),
   m_image_width ( rhs.m_image_width ),
   m_image_name ( rhs.m_image_name )
{}


SingleParticle2dx::DataStructures::GlobalParticleInformation& SingleParticle2dx::DataStructures::GlobalParticleInformation::operator= (GlobalParticleInformation rhs)
{
	swap (*this, rhs);
	return *this;
}


SingleParticle2dx::DataStructures::GlobalParticleInformation::number_type SingleParticle2dx::DataStructures::GlobalParticleInformation::getImageNumber () const
{
	return m_image_number;
}


void SingleParticle2dx::DataStructures::GlobalParticleInformation::setImageNumber (const number_type nr)
{
	m_image_number = nr;
}


SingleParticle2dx::DataStructures::GlobalParticleInformation::position_type SingleParticle2dx::DataStructures::GlobalParticleInformation::getPositionX () const
{
	return m_position.first;
}


void SingleParticle2dx::DataStructures::GlobalParticleInformation::setPositionX (const position_type pos)
{
	m_position.first = pos;
}


SingleParticle2dx::DataStructures::GlobalParticleInformation::position_type SingleParticle2dx::DataStructures::GlobalParticleInformation::getPositionY () const
{
	return m_position.second;
}


void SingleParticle2dx::DataStructures::GlobalParticleInformation::setPositionY (const position_type pos)
{
	m_position.second = pos;
}


std::string SingleParticle2dx::DataStructures::GlobalParticleInformation::getDataString()
{
	std::string result = SingleParticle2dx::Utilities::StringFunctions::TtoString(m_image_number);
	result += "\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString(m_position.first);
	result += "\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString(m_position.second);
	return result;
}


void SingleParticle2dx::DataStructures::GlobalParticleInformation::setImageWidth(size_type rhs)
{
	m_image_width = rhs;
}


SingleParticle2dx::size_type SingleParticle2dx::DataStructures::GlobalParticleInformation::getImageWidth()
{
	return m_image_width;
}


std::string SingleParticle2dx::DataStructures::GlobalParticleInformation::getDescString()
{
	return "image_number\tpos_x\tpos_y";
}


void SingleParticle2dx::DataStructures::GlobalParticleInformation::setImageName(std::string rhs)
{
	m_image_name = rhs;
}


std::string SingleParticle2dx::DataStructures::GlobalParticleInformation::getImageName()
{
	return m_image_name;
}


namespace SingleParticle2dx
{
	namespace DataStructures
	{
		void swap (GlobalParticleInformation& i1, GlobalParticleInformation& i2)
		{
			std::swap (i1.m_image_number, i2.m_image_number);
			std::swap (i1.m_position, i2.m_position);
			std::swap (i1.m_image_width, i2.m_image_width);
			std::swap (i1.m_image_name, i2.m_image_name);
		}	
	} /* DataStructures */
} /* SingleParticle2dx */
