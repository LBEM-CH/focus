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


#include "BasicCalcCCMethod.hpp"
#include "../../Utilities.hpp"
#include "../../Config.hpp"


SingleParticle2dx::Methods::BasicCalcCCMethod::BasicCalcCCMethod ()
{}


SingleParticle2dx::Methods::BasicCalcCCMethod::~BasicCalcCCMethod ()
{}


void SingleParticle2dx::Methods::BasicCalcCCMethod::calculateCrossCorrelation( SingleParticle2dx::DataStructures::Particle& part, SingleParticle2dx::DataStructures::Projection2d& proj, real_array2d_type& res)
{
	value_type nx = part.getSizeX();
	value_type ny = part.getSizeY();
	
	fft_array2d_type fft_data( boost::extents[nx][ny] );
	
	real_array2d_type real_data( boost::extents[nx][ny] );
	real_array2d_type real_data_shifted( boost::extents[nx][ny] );
	
	fft_type part_image(0,0);
	fft_type ref_image(0,0);
	value_type r2;
	value_type w;

	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	for (size_type i=0; i<part.getSizeX(); i++)
	{
		for (size_type j=0; j<part.getSizeY(); j++)
		{
			r2 = sqrt( (i-nx/2)*(i-nx/2) + (j-ny/2)*(j-ny/2) );	
			{		
				if ( (r2<config->getMaxCCFreq()) && (r2>config->getMinCCFreq()) )
				{
					w = 1;
				}
				else if ( r2<(config->getMaxCCFreq()+config->getCCFreqSigma()) )
				{
					value_type dr = r2-config->getMaxCCFreq();
					w = 1 - dr/config->getCCFreqSigma();
				}
				else
				{
					w = 0;
				}
			
				part_image = part(i,j);
				ref_image = proj(i,j);
				fft_data[i][j].real(part_image.real() * ref_image.real() + part_image.imag() * ref_image.imag());
				fft_data[i][j].imag(-part_image.real() * ref_image.imag() + part_image.imag() * ref_image.real());
			
				fft_data[i][j] *= w;
			}
		}
	}
	
	SingleParticle2dx::Utilities::FFTCalculator::performBackwardFFT (&fft_data, &real_data);

	size_type half_window = floor(SingleParticle2dx::ConfigContainer::Instance()->getCrossCorrelationWindowSize()/2.);
	
	int ii,jj;
	for (int i=0; i<nx; i++)
	{
		for (int j=0; j<ny; j++)
		{
			ii = i - nx/2;
			jj = j - nx/2;
			
			if (ii<0)
			{
				ii += nx;
			}
			
			if (jj<0)
			{
				jj += ny;
			}
			
			if (ii >= nx)
			{
				ii -= nx;
			}
			
			if (jj >= ny)
			{
				jj -= ny;
			}
			
			real_data_shifted[i][j] = real_data[ii][jj] / nx;
		}
	}
	
	res = real_data_shifted[boost::indices[range(nx/2-half_window,nx/2+half_window+1)][range(ny/2-half_window,ny/2+half_window+1)]];
	
}
