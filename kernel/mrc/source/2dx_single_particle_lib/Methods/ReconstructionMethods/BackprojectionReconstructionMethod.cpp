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

#include "BackprojectionReconstructionMethod.hpp"
#include "../../Utilities.hpp"

SingleParticle2dx::Methods::BackprojectionReconstructionMethod::BackprojectionReconstructionMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context)
{
	m_context = context;
	size_type n = context->getSizeX();
	m_norm_volume.reset( new real_array3d_type(boost::extents[n][n][n]) );
}


SingleParticle2dx::Methods::BackprojectionReconstructionMethod::~BackprojectionReconstructionMethod ()
{}


void SingleParticle2dx::Methods::BackprojectionReconstructionMethod::setupForBackProjection()
{
	m_context->resetData();
	SingleParticle2dx::Utilities::DataContainerFunctions::resetData( m_norm_volume.get() );
}
			
			
void SingleParticle2dx::Methods::BackprojectionReconstructionMethod::finishReconstruction()
{
	SingleParticle2dx::Utilities::MRCFileIO::writeToMrc(m_norm_volume.get(), "norm_factor_output.mrc");
	
	value_type norm_factor;
	fft_type fft_zero(value_type(0), value_type(0));
	
	for(size_type i=0; i<m_context->getSizeX(); i++)
	{
		for(size_type j=0; j<m_context->getSizeY(); j++)
		{
			for(size_type k=0; k<m_context->getSizeZ(); k++)
			{
				norm_factor = (*m_norm_volume.get())[i][j][k];
				if ( fabs(norm_factor) > 0)
				{
					(*m_context)(i,j,k) /= norm_factor;
				}
				else
				{
					(*m_context)(i,j,k) = fft_zero;
				}
			}
		}
	}
}
						
			
void SingleParticle2dx::Methods::BackprojectionReconstructionMethod::insertData(SingleParticle2dx::DataStructures::ParticleContainer &c)
{
	for (size_type i=0; i<c.getNumberOfParticles(); i++)
	{
		insertParticle(c(i));
	}
}


void SingleParticle2dx::Methods::BackprojectionReconstructionMethod::insertParticle(SingleParticle2dx::DataStructures::Particle& p)
{
	Eigen::Matrix3f rot = SingleParticle2dx::Utilities::UtilityFunctions::determineRotation(p.getNewOrientation());
	size_type n = p.getSizeX();
	size_type R_max = n/2;
	
	Vector3d n_sizehalf;
	n_sizehalf << n/2, n/2, n/2;
	
	Vector3d tmp_vec;
	fft_type fft_data_point;
	std::pair<size_type, size_type> indx, indy, indz;
	
	for (size_type i=0; i<n; i++)
	{
		for (size_type j=0; j<n; j++)
		{
			
			//value_type wgt=exp(-((i-n/2)*(i-n/2)+(j-n/2)*(j-n/2))/powf(R_max*R_max*0.9,2.0));
			fft_data_point = p(i,j);// * wgt;
			
			tmp_vec << i,j,n/2;
			tmp_vec -= n_sizehalf;
			tmp_vec = rot * tmp_vec;
			tmp_vec += n_sizehalf;
			
			indx = calcLowerAndUpperIndex(tmp_vec[0]);		
			indy = calcLowerAndUpperIndex(tmp_vec[1]);		
			indz = calcLowerAndUpperIndex(tmp_vec[2]);
			
			value_type R;
			value_type interpolation_weight;
			std::vector<value_type> diff(3, value_type(0));
			std::vector<size_type> indices_3d(3, size_type(0));
			
			for(size_type L=indx.first; L<=indx.second; L++)
			{
				indices_3d[0] = L;
				for(size_type M=indy.first; M<=indy.second; M++)
				{
					indices_3d[1] = M;
					for(size_type N=indz.first; N<=indz.second; N++)
					{
						indices_3d[2] = N;
						
						R = sqrt((L-n/2)*(L-n/2) + (M-n/2)*(M-n/2) + (N-n/2)*(N-n/2));
						//std::cout << R << "  (" << R_max << ")" << std::endl;
						
						//if (R < R_max)
						{
							for(size_type i=0; i<3; i++)
							{
								diff[i] = tmp_vec[i] - indices_3d[i];
								//std::cout << diff[i] << std::endl;
							}
							
							interpolation_weight = calcInterpolationWeight(diff);
							adjust3DIndices(indices_3d);
							(*m_norm_volume.get())[indices_3d[0]][indices_3d[1]][indices_3d[2]] += interpolation_weight;
							(*m_context)(indices_3d[0], indices_3d[1], indices_3d[2]) += fft_data_point * interpolation_weight;
							
						}
					}
				}
			}	
		}
	}
}


std::pair<SingleParticle2dx::Methods::BackprojectionReconstructionMethod::size_type, SingleParticle2dx::Methods::BackprojectionReconstructionMethod::size_type> SingleParticle2dx::Methods::BackprojectionReconstructionMethod::calcLowerAndUpperIndex( value_type xobs )
{
	std::pair<size_type, size_type> result;
	
	value_type aux = xobs - value_type(0.5);
	result.first = round(aux);
	
	if (aux < 0)
	{
		result.first -= size_type(1);
	}
	if (result.first < 0)
	{
		result.first = 0;
	}
	
	aux = xobs + value_type(0.5);
	result.second = round(aux);
	
	if (aux < 0)
	{
		result.second -= size_type(1);
	}
	if (result.second >= m_context->getSizeX())
	{
		result.second = m_context->getSizeX() - size_type(1);
	}
	
	return result;
}

void SingleParticle2dx::Methods::BackprojectionReconstructionMethod::adjust3DIndices( std::vector<size_type>& ind )
{
	value_type temp;
	value_type nx = m_context->getSizeX();
	for (size_type i=0; i<3; i++)
	{
		temp = ind[i];
		if (temp >= nx)
		{
			temp -= nx; 
		}
		if (temp < 0)
		{
			temp += nx;
		}
		ind[i] = temp;
	}
}

SingleParticle2dx::Methods::BackprojectionReconstructionMethod::value_type SingleParticle2dx::Methods::BackprojectionReconstructionMethod::calcInterpolationWeight(std::vector<value_type>& ind)
{
	value_type result = 1;
	for (size_type i=0; i<3; i++)
	{
		result *= SingleParticle2dx::Utilities::UtilityFunctions::sinc(ind[i]);
	}
	//std::cout << result << "\n";
	return result;
}

