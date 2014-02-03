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

#include "ClassInformation.hpp"

#include "../Utilities/UtilityFunctions.hpp"


SingleParticle2dx::DataStructures::ClassInformation::ClassInformation ()
{
	m_class_number = size_type(0);
	m_true_class_number = size_type(0);
}


SingleParticle2dx::DataStructures::ClassInformation::ClassInformation (size_type class_number, size_type true_class)
{
	m_class_number = class_number;
	m_true_class_number = true_class;
}


SingleParticle2dx::DataStructures::ClassInformation::ClassInformation (ClassInformation const& rhs)
{
	m_class_number = rhs.m_class_number;
	m_true_class_number = rhs.m_true_class_number;
}


SingleParticle2dx::DataStructures::ClassInformation::~ClassInformation ()
{}


SingleParticle2dx::DataStructures::ClassInformation& SingleParticle2dx::DataStructures::ClassInformation::operator= (ClassInformation rhs)
{
	if(this == &rhs)
	{
		return *this;
	}
	
	swap (*this, rhs);
	return *this;
}


SingleParticle2dx::DataStructures::ClassInformation::size_type SingleParticle2dx::DataStructures::ClassInformation::getClass() const
{
	return m_class_number;
}


void SingleParticle2dx::DataStructures::ClassInformation::setClass(const size_type rhs)
{
	m_class_number = rhs;
}


SingleParticle2dx::DataStructures::ClassInformation::size_type SingleParticle2dx::DataStructures::ClassInformation::getTrueClass() const
{
	return m_true_class_number;
}


void SingleParticle2dx::DataStructures::ClassInformation::setTrueClass(const size_type rhs)
{
	m_true_class_number = rhs;
} 


std::string SingleParticle2dx::DataStructures::ClassInformation::getDataString()
{
	std::string result = SingleParticle2dx::Utilities::StringFunctions::TtoString(getClass());
	result += "\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString(getTrueClass());
	return result;
}
			
			
std::string SingleParticle2dx::DataStructures::ClassInformation::getDescString()
{
	std::string result = "class";
	result += "\ttrue_class";
	return result;
}


namespace SingleParticle2dx
{
	namespace DataStructures
	{
		void swap (ClassInformation& i1, ClassInformation& i2)
		{
			std::swap (i1.m_class_number, i2.m_class_number);
			std::swap (i1.m_true_class_number, i2.m_true_class_number);
			
		}	
	} /* DataStructures */
} /* SingleParticle2dx */
