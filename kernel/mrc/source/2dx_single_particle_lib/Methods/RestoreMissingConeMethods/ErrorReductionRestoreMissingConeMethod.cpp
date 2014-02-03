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


#include "ErrorReductionRestoreMissingConeMethod.hpp"

#include "../../DataStructures.hpp"


SingleParticle2dx::Methods::ErrorReductionRestoreMissingConeMethod::ErrorReductionRestoreMissingConeMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context)
{
	m_context = context;
	std::cout << "::ErrorRed Missing Cone generated" << std::endl;
}


SingleParticle2dx::Methods::ErrorReductionRestoreMissingConeMethod::~ErrorReductionRestoreMissingConeMethod ()
{}

void SingleParticle2dx::Methods::ErrorReductionRestoreMissingConeMethod::restoreMissingCone()
{
	std::cout << "::MISSING CONE" << std::endl;
	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	size_type n = m_context->getSizeX();
	value_type t = 5e-6;
	size_type r_max = config->getLPReconstructionRadius();
	value_type r;
	
	real_array3d_type realdata( boost::extents[n][n][n] );
	fft_array3d_type fftdata_old( boost::extents[n][n][n] );
	fft_array3d_type fftdata_new( boost::extents[n][n][n] );
	
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d(n, n, n);
	rec3d.setMaskingMethod(1);
	
//	if (config->getKeepAll())
//	{
		m_context->writeToFile("REC_SP/before_missing_cone.map");
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("REC_SP/before_missing_cone.map", "MAP: before missing cone", config->getScriptName(), false, false);
//	}
	
	m_context->getRealSpaceData(realdata);
	SingleParticle2dx::Utilities::FFTCalculator::performForwardFFT (&realdata, &fftdata_old);
	SingleParticle2dx::Utilities::DataContainerFunctions::fillRandomFourierSpace(&fftdata_old, config->getMaxConeTilt(), 1);
	SingleParticle2dx::Utilities::FFTCalculator::performBackwardFFT (&fftdata_old, &realdata);
	
	for (size_type iteration=0; iteration<50; iteration++)
	{
		std::cout << iteration << std::endl;
		
		rec3d.setFourierSpaceData(realdata);
		
		rec3d.applyMask();
		
		rec3d.getRealSpaceData(realdata);

		for(size_type i=0; i<n; i++)
		{
			for(size_type j=0; j<n; j++)
			{
				for(size_type k=0; k<n; k++)
				{
					if ( realdata[i][j][k] < 0 )
					{
						realdata[i][j][k] = 0;
					}
				}
			}
		}

		SingleParticle2dx::Utilities::FFTCalculator::performForwardFFT (&realdata, &fftdata_new);

		for(size_type i=0; i<n; i++)
		{
			for(size_type j=0; j<n; j++)
			{
				for(size_type k=0; k<n; k++)
				{
					r = sqrt((i-n/2)*(i-n/2) + (j-n/2)*(j-n/2) + (k-n/2)*(k-n/2));
					if ( (r<r_max) && (abs(fftdata_new[i][j][k])>t) )
					{
						fftdata_new[i][j][k] = fftdata_old[i][j][k];
					}
					else if ( r>r_max )
					{
						fftdata_new[i][j][k] = fft_type(0,0);
					}
				}
			}
		}

		std::copy(fftdata_new.origin(), fftdata_new.origin() + fftdata_new.num_elements(), fftdata_old.origin() );

		SingleParticle2dx::Utilities::FFTCalculator::performBackwardFFT (&fftdata_new, &realdata);
		
		//sym
		SingleParticle2dx::DataStructures::Reconstruction3d::applySymmetry(realdata);
		
		if (config->getKeepAll())
		{
			SingleParticle2dx::Utilities::MRCFileIO::writeToMrc(&realdata, "REC_SP/missing_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(iteration) + ".map");
			SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("REC_SP/missing_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(iteration) + ".map", "MAP: Missing " + SingleParticle2dx::Utilities::StringFunctions::TtoString(iteration), config->getScriptName(), false, false);
		}
	}
	
	m_context->setFourierSpaceData(realdata);
	SingleParticle2dx::Utilities::UtilityFunctions::forceReload();
}
