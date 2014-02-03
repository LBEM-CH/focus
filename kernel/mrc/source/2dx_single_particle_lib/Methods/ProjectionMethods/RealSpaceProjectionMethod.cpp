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

#include "RealSpaceProjectionMethod.hpp"
#include "../../Utilities.hpp"


SingleParticle2dx::Methods::RealSpaceProjectionMethod::RealSpaceProjectionMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context)
{
	m_context = context;
	m_iterpolate_strategy.reset( new SingleParticle2dx::Methods::LinearInterpolate2dMethod );
}

SingleParticle2dx::Methods::RealSpaceProjectionMethod::~RealSpaceProjectionMethod ()
{}


void SingleParticle2dx::Methods::RealSpaceProjectionMethod::interpolate2d(value_type dx, value_type dy, value_type val, std::vector<value_type>& res)
{
	m_iterpolate_strategy.get()->interpolate2d(dx, dy, val, res);
}


void SingleParticle2dx::Methods::RealSpaceProjectionMethod::calculateProjection(SingleParticle2dx::DataStructures::Orientation& o, SingleParticle2dx::DataStructures::Projection2d& p)
{
	p.resetData();
	Eigen::Matrix3f rot = SingleParticle2dx::Utilities::UtilityFunctions::determineRotation(o);
	size_type n = p.getSizeX();
	
	size_type x1, y1;
	value_type dx, dy;
	value_type ii, jj;
	Vector3d n_vec, new_vec;
		
	Vector3d n_sizehalf;
	n_sizehalf << n/2, n/2, n/2;

	real_array2d_type real_data_2d;
	real_data_2d.resize( boost::extents[n][n] );
	SingleParticle2dx::Utilities::DataContainerFunctions::resetData(&real_data_2d);
	
	real_array3d_type real_data_3d;
	real_data_3d.resize( boost::extents[n][n][n] );
	m_context->getRealSpaceData(real_data_3d);
	
	std::vector<value_type> interpolated_values (4, value_type(0) );
	size_type i, j, k;
	
	for(i=0; i<n; i++)
	{
		for(j=0; j<n; j++)
		{
			for(k=0; k<n; k++)
			{
				n_vec << i, j, k;
				n_vec -= n_sizehalf;
				new_vec = rot*n_vec;
				new_vec += n_sizehalf;
				
				ii = new_vec[0];
				jj = new_vec[1];
				
				//if ( (ii<0) || (jj<0) || (ii>=(n-1)) || (jj>=(n-1)) )
				//{
				//	continue;
				//}
				
				if (ii<0)
				{
					ii += n;
				}
				
				if (jj<0)
				{
					jj += n;
				}
				
				if (ii >= n)
				{
					ii -= n;
				}
				
				if (jj >= n)
				{
					jj -= n;
				}
				
				x1 = floor(ii);
				y1 = floor(jj);
					
				dx = 1 - ii + x1;
				dy = 1 - jj + y1;
				
				interpolate2d(dx, dy, real_data_3d[i][j][k], interpolated_values);
				real_data_2d[x1][y1] += interpolated_values[0];
				real_data_2d[x1+1][y1] += interpolated_values[1];
				real_data_2d[x1][y1+1] += interpolated_values[2];
				real_data_2d[x1+1][y1+1] += interpolated_values[3];
			}
		}
	}
		
	p.setFourierSpaceData(&real_data_2d);
	p.normalizeRealSpace();
	p.setOrientation(o);
}
