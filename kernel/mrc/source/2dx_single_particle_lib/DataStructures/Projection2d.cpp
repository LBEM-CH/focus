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

#include "Projection2d.hpp"
#include "../Utilities.hpp"


SingleParticle2dx::DataStructures::Projection2d::Projection2d ()
 : m_orientation ( SingleParticle2dx::DataStructures::Orientation() )
{
	m_fourier_data.reset ( new fft_array_type(boost::extents[1][1]) );
	SingleParticle2dx::DataStructures::Abstract2dData::resetData();
	
	setupFromConfig();
}


SingleParticle2dx::DataStructures::Projection2d::Projection2d (size_type size_x, size_type size_y)
 : m_orientation ( SingleParticle2dx::DataStructures::Orientation() )
{
	m_fourier_data.reset ( new fft_array_type(boost::extents[size_x][size_y]) );
	SingleParticle2dx::DataStructures::Abstract2dData::resetData();
	
	setupFromConfig();
}


SingleParticle2dx::DataStructures::Projection2d::Projection2d (size_type size_x, size_type size_y, SingleParticle2dx::DataStructures::Orientation o)
 : m_orientation ( o )
{
	m_fourier_data.reset ( new fft_array_type(boost::extents[size_x][size_y]) ),
	SingleParticle2dx::DataStructures::Abstract2dData::resetData();
	
	setupFromConfig();
}


SingleParticle2dx::DataStructures::Projection2d::Projection2d (Projection2d const& rhs)
 : m_orientation ( rhs.m_orientation )
{
	m_fourier_data.reset ( new fft_array_type(*(rhs.m_fourier_data.get())) );
	
	setupFromConfig();
}


SingleParticle2dx::DataStructures::Projection2d::~Projection2d ()
{}


void SingleParticle2dx::DataStructures::Projection2d::setupFromConfig()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	setMaskingMethod( config->getProjectionMaskingMethod());
	
	m_lowpass_radius = config->getLPProjectionRadius();
	m_lowpass_relax_param = config->getLPProjectionSigma();
}


SingleParticle2dx::DataStructures::Projection2d& SingleParticle2dx::DataStructures::Projection2d::operator= (Projection2d rhs)
{
	swap (*this, rhs);
	return *this;
}


SingleParticle2dx::DataStructures::Projection2d::fft_type& SingleParticle2dx::DataStructures::Projection2d::operator () (const size_type i, const size_type j)
{
	boost::array<fft_array_type::index,2> idx = {{i,j}};
	return m_fourier_data.get()->operator()(idx);
}


SingleParticle2dx::DataStructures::Orientation& SingleParticle2dx::DataStructures::Projection2d::getOrientation()
{
	return m_orientation;
}


void SingleParticle2dx::DataStructures::Projection2d::setOrientation (SingleParticle2dx::DataStructures::Orientation o)
{
	m_orientation = o;
}


void SingleParticle2dx::DataStructures::Projection2d::setMaskingMethod( value_type key )
{
	switch (static_cast<int>(key))
	{
		case 0:
			m_2dmasking_strategy.reset ( new SingleParticle2dx::Methods::Dummy2dMaskingMethod( this ) );
			break;
		case 1:
			m_2dmasking_strategy.reset ( new SingleParticle2dx::Methods::Cos2dMaskingMethod( this ) );
			break;
		default:
			std::cerr << "Unknow 2d masking method for projection class" << std::endl;
			m_2dmasking_strategy.reset ( new SingleParticle2dx::Methods::Dummy2dMaskingMethod( this ) );
		//	throw std::runtime_error("Bad operation");
	}
}


namespace SingleParticle2dx
{
	namespace DataStructures
	{
		void swap (Projection2d& p1, Projection2d& p2 )
		{
			boost::swap (p1.m_fourier_data, p2.m_fourier_data);
			boost::swap (p1.m_2dmasking_strategy, p2.m_2dmasking_strategy);
			std::swap (p1.m_orientation, p2.m_orientation);
		}
		
	} /* DataStructures */
	
} /* SingleParticle2dx */
