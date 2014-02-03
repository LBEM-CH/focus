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

#include "CTFInformation.hpp"

#include "../Utilities/UtilityFunctions.hpp"


SingleParticle2dx::DataStructures::CTFParticleInformation::CTFParticleInformation ()
{
	m_defocus = 0;
	m_cs = 0;
	m_voltage = 0;
	m_apix = 0;
	m_amp_contrast = 0;
	m_bfactor = 0;
	m_ast = 0;
	m_ast_ang = 0;
}


SingleParticle2dx::DataStructures::CTFParticleInformation::CTFParticleInformation(value_type defocus, value_type cs, value_type voltage, value_type apix, value_type amp_contrast, value_type bfactor, value_type ast, value_type ast_ang)
{
	m_defocus = defocus;
	m_cs = cs;
	m_voltage = voltage;
	m_apix = apix;
	m_amp_contrast = amp_contrast;
	m_bfactor = bfactor;
	m_ast = ast;
	m_ast_ang = ast_ang;
}


SingleParticle2dx::DataStructures::CTFParticleInformation::~CTFParticleInformation ()
{}


SingleParticle2dx::DataStructures::CTFParticleInformation::CTFParticleInformation (CTFParticleInformation const& rhs)
{
	m_defocus = rhs.m_defocus;
	m_cs = rhs.m_cs;
	m_voltage = rhs.m_voltage;
	m_apix = rhs.m_apix;
	m_amp_contrast = rhs.m_amp_contrast;
	m_bfactor = rhs.m_bfactor;
	m_ast = rhs.m_ast;
	m_ast_ang = rhs.m_ast_ang;
}


SingleParticle2dx::DataStructures::CTFParticleInformation& SingleParticle2dx::DataStructures::CTFParticleInformation::operator= (CTFParticleInformation rhs)
{
	swap (*this, rhs);
	return *this;
}


SingleParticle2dx::DataStructures::CTFParticleInformation::value_type SingleParticle2dx::DataStructures::CTFParticleInformation::getDefocus() const
{
	return m_defocus;
}


SingleParticle2dx::DataStructures::CTFParticleInformation::value_type SingleParticle2dx::DataStructures::CTFParticleInformation::getCs() const
{
	return m_cs;
}


SingleParticle2dx::DataStructures::CTFParticleInformation::value_type SingleParticle2dx::DataStructures::CTFParticleInformation::getVoltage() const
{
	return m_voltage;
}


SingleParticle2dx::DataStructures::CTFParticleInformation::value_type SingleParticle2dx::DataStructures::CTFParticleInformation::getApix() const
{
	return m_apix;
}


SingleParticle2dx::DataStructures::CTFParticleInformation::value_type SingleParticle2dx::DataStructures::CTFParticleInformation::getAmpCon() const
{
	return m_amp_contrast;
}


SingleParticle2dx::DataStructures::CTFParticleInformation::value_type SingleParticle2dx::DataStructures::CTFParticleInformation::getBfacotr() const
{
	return m_bfactor;
}


SingleParticle2dx::DataStructures::CTFParticleInformation::value_type SingleParticle2dx::DataStructures::CTFParticleInformation::getAst() const
{
	return m_ast;
}


SingleParticle2dx::DataStructures::CTFParticleInformation::value_type SingleParticle2dx::DataStructures::CTFParticleInformation::getAngAst() const
{
	return m_ast_ang;
}




void SingleParticle2dx::DataStructures::CTFParticleInformation::setDefocus(value_type rhs)
{
	m_defocus = rhs;
}


void SingleParticle2dx::DataStructures::CTFParticleInformation::setCs(value_type rhs)
{
	m_cs = rhs;
}


void SingleParticle2dx::DataStructures::CTFParticleInformation::setVoltage(value_type rhs)
{
	m_voltage = rhs;
}


void SingleParticle2dx::DataStructures::CTFParticleInformation::setApix(value_type rhs)
{
	m_apix = rhs;
}


void SingleParticle2dx::DataStructures::CTFParticleInformation::setAmpCon(value_type rhs)
{
	m_amp_contrast = rhs;
}


void SingleParticle2dx::DataStructures::CTFParticleInformation::setBfactor(value_type rhs)
{
	m_bfactor = rhs;
}


void SingleParticle2dx::DataStructures::CTFParticleInformation::setAst(value_type rhs)
{
	m_ast = rhs;
}


void SingleParticle2dx::DataStructures::CTFParticleInformation::setAngAst(value_type rhs)
{
	m_ast_ang = rhs;
}


std::string SingleParticle2dx::DataStructures::CTFParticleInformation::getDataString()
{
	std::string result = SingleParticle2dx::Utilities::StringFunctions::TtoString(m_defocus);
	result += "\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString(m_cs);
	result += "\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString(m_voltage);
	result += "\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString(m_apix);
	result += "\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString(m_amp_contrast);
	result += "\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString(m_bfactor);
	result += "\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString(m_ast);
	result += "\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString(m_ast_ang);
	return result;
}


std::string SingleParticle2dx::DataStructures::CTFParticleInformation::getDescString()
{
	return "defocus\tcs\tvoltage\tapix\tamp_cont\tbfactor\tast\tast_ang";
}


namespace SingleParticle2dx
{
	namespace DataStructures
	{
		void swap (CTFParticleInformation& i1, CTFParticleInformation& i2)
		{
			std::swap (i1.m_defocus, i2.m_defocus);
			std::swap (i1.m_cs, i2.m_cs);
			std::swap (i1.m_voltage, i2.m_voltage);
			std::swap (i1.m_apix, i2.m_apix);
			std::swap (i1.m_amp_contrast, i2.m_amp_contrast);
			std::swap (i1.m_bfactor, i2.m_bfactor);
			std::swap (i1.m_ast, i2.m_ast);
			std::swap (i1.m_ast_ang, i2.m_ast_ang);
		}	
	} /* DataStructures */
} /* SingleParticle2dx */
