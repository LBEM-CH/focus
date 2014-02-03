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

#include <boost/accumulators/accumulators.hpp>
#include <boost/accumulators/statistics/stats.hpp>
#include <boost/accumulators/statistics/mean.hpp>
#include <boost/accumulators/statistics/variance.hpp>
#include <boost/accumulators/statistics/min.hpp>
#include <boost/accumulators/statistics/max.hpp>

#include "Reconstruction3d.hpp"
#include "../Config.hpp"


SingleParticle2dx::DataStructures::Reconstruction3d::Reconstruction3d (size_type size_x, size_type size_y, size_type size_z, bool minimal)
 : m_angular_change_measure( value_type(0) ),
   m_number_of_particles(0)
{
	m_minimal = minimal;
	
	m_fourier_data.reset( new fft_array3d_type(boost::extents[size_x][size_y][size_z]) );	
	resetData();
	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	setProjectionMethod ( config->getProjectionMethod() );
	
//	if(!minimal)
//	{
		setRefinementMethod ( config->getRefinementMethod() );
		setBackprojectionMethod ( config->getBackprojectionMethod() );
		setMaskingMethod ( config->getReconstructionMaskingMethod() );
		setMissingConeMethod ( config->getMissingConeMethod() );
	
		m_lowpass_radius = config->getLPReconstructionRadius();
		m_lowpass_relax_param = config->getLPReconstructionSigma();
//	}
//	else
//	{
//		setRefinementMethod ( 1 );
//		setBackprojectionMethod ( 2 );
//		setMaskingMethod ( 0 );
//		setMissingConeMethod ( 0 );
//	
//		m_lowpass_radius = 100;
//		m_lowpass_relax_param = 2;
//	}

}


SingleParticle2dx::DataStructures::Reconstruction3d::Reconstruction3d ()
 : m_angular_change_measure( value_type(0) ),
   m_number_of_particles(0)
{
	m_minimal = false;
	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	size_type size_x = config->getParticleSize();
	
	m_fourier_data.reset( new fft_array3d_type(boost::extents[size_x][size_x][size_x]) );	
	resetData();
	
	
	setProjectionMethod ( config->getProjectionMethod() );
	
	setRefinementMethod ( config->getRefinementMethod() );
	setBackprojectionMethod ( config->getBackprojectionMethod() );
	setMaskingMethod ( config->getReconstructionMaskingMethod() );
	setMissingConeMethod ( config->getMissingConeMethod() );
	
	m_lowpass_radius = config->getLPReconstructionRadius();
	m_lowpass_relax_param = config->getLPReconstructionSigma();
}


SingleParticle2dx::DataStructures::Reconstruction3d::Reconstruction3d (Reconstruction3d const& rhs)
{
	m_fourier_data.reset ( new fft_array3d_type( *(rhs.m_fourier_data.get())) );
	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	setProjectionMethod ( config->getProjectionMethod() );
	setRefinementMethod ( config->getRefinementMethod() );
	setBackprojectionMethod ( config->getBackprojectionMethod() );
	setMaskingMethod ( config->getReconstructionMaskingMethod() );
	setMissingConeMethod ( config->getMissingConeMethod() );
	
	m_lowpass_radius = config->getLPReconstructionRadius();
	m_lowpass_relax_param = config->getLPReconstructionSigma();
	
	m_angular_change_measure = rhs.m_angular_change_measure;
	m_timer = rhs.m_timer;
	
	m_number_of_particles = rhs.m_number_of_particles;
	
	m_minimal = rhs.m_minimal;
}


void SingleParticle2dx::DataStructures::Reconstruction3d::resetAll()
{
	m_projection_strategy.reset(NULL);	
	m_refinement_strategy.reset(NULL);
	m_missingcone_strategy.reset(NULL);
	m_reconstruction_strategy.reset(NULL);
	m_fourier_data.reset(NULL);
}



SingleParticle2dx::DataStructures::Reconstruction3d& SingleParticle2dx::DataStructures::Reconstruction3d::operator= (Reconstruction3d rhs)
{
	if(this == &rhs)
	{
		return *this;
	}
	
	swap (*this, rhs);
	return *this;
}


SingleParticle2dx::DataStructures::Reconstruction3d::~Reconstruction3d ()
{}


void SingleParticle2dx::DataStructures::Reconstruction3d::generateInitialModel(SingleParticle2dx::DataStructures::ParticleContainer& c)
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("generating init model with  " + SingleParticle2dx::Utilities::StringFunctions::TtoString(c.getNumberOfParticles()) + " particles", 1);
	
	value_type t_start = m_timer.GetTime();
	m_reconstruction_strategy.get()->updateReconstruction(c);
	std::cout << ":time for initial reconstruction: " << m_timer.GetTime() - t_start << std::endl;
	
	
	applyMask();
	applyLowPassFilter();
	
//	boost::accumulators::accumulator_set<value_type, boost::accumulators::stats<boost::accumulators::tag::max> > acc;
//	for(size_type i=0; i<c.getNumberOfParticles(); i++)
//	{
//		acc(fabs(c(i).getNewOrientation().getTLTANG()));
//	}
//	config->setMaxConeTilt(90-boost::accumulators::extract_result<boost::accumulators::tag::max>(acc));
	
	m_number_of_particles = c.getNumberOfParticles();
	
	restoreMissingCone();
}


void SingleParticle2dx::DataStructures::Reconstruction3d::forceProjectionPreparation(SingleParticle2dx::DataStructures::ParticleContainer& c)
{
	m_projection_strategy.get()->prepareForProjections(c);
}


void SingleParticle2dx::DataStructures::Reconstruction3d::updateReconstruction (SingleParticle2dx::DataStructures::ParticleContainer& c, bool useweights, bool write_debug_output, bool skip_backproj)
{
	value_type t_start = m_timer.GetTime();
	
	m_projection_strategy.get()->clearProjections();
	m_projection_strategy.get()->prepareForProjections(c);
	
	if ( c.getNumberOfParticles() == 0)
	{
		return;
	}
	
	
	m_refinement_strategy.get()->updateReconstruction(c, useweights, write_debug_output);
	std::cout << ":time for alignment: " << m_timer.GetTime() - t_start << std::endl;
	
	t_start = m_timer.GetTime();
	
	if (useweights)
	{
		size_type number_of_diff_images = c.getNumberOfDiffImages();
		c.calcAndSetConsistency(number_of_diff_images);
		c.selectParticlesBasedOnConsistency(number_of_diff_images);
		
	//	std::cout << "::Update use for rec" << std::endl;
	//	c.updateUseForReconstruction();
		
	//	std::cout << "::Update weights" << std::endl;
	//	c.updateWeightForReconstruction();
		
	//	std::cout << "::Update use for rec II" << std::endl;
	//	c.updateUseForReconstructionBasedOnWeights();
	}
	
	
	std::cout << "::Update rec" << std::endl;
	
	if (!skip_backproj)
	{
		m_reconstruction_strategy.get()->updateReconstruction(c);
		restoreMissingCone();
		std::cout << ":time for reconstructing: " << m_timer.GetTime() - t_start << std::endl;
	}
	
	//m_projection_strategy.get()->clearProjections();
}


void SingleParticle2dx::DataStructures::Reconstruction3d::setupForBackProjection()
{
	m_reconstruction_strategy.get()->setupForBackProjection();
}


void SingleParticle2dx::DataStructures::Reconstruction3d::finishReconstruction()
{
	m_reconstruction_strategy.get()->finishReconstruction();
}


void SingleParticle2dx::DataStructures::Reconstruction3d::insertData(SingleParticle2dx::DataStructures::ParticleContainer &c)
{
	m_reconstruction_strategy.get()->insertData(c);
}


void SingleParticle2dx::DataStructures::Reconstruction3d::restoreMissingCone ()
{
	m_missingcone_strategy.get()->restoreMissingCone();
}


SingleParticle2dx::DataStructures::Reconstruction3d::value_type SingleParticle2dx::DataStructures::Reconstruction3d::getLastAngleChange ()
{
	return m_angular_change_measure;
}


void SingleParticle2dx::DataStructures::Reconstruction3d::calculateProjection(SingleParticle2dx::DataStructures::Orientation o, SingleParticle2dx::DataStructures::Projection2d& p)
{
	//std::cout << "hi_proj 1" << std::endl;
	
	m_projection_strategy.get();

	//std::cout << "hi_proj 2" << std::endl;
	
	m_projection_strategy.get()->calculateProjection(o, p);
	
	//std::cout << "hi_proj 3" << std::endl;
}


void SingleParticle2dx::DataStructures::Reconstruction3d::clearProjections()
{
	m_projection_strategy.get()->clearProjections();
}


void SingleParticle2dx::DataStructures::Reconstruction3d::resetAngularChangeMeasure()
{
	m_angular_change_measure = value_type(0);
}


void SingleParticle2dx::DataStructures::Reconstruction3d::applySymmetry()
{
	real_array3d_type real_space = real_array3d_type(boost::extents[getSizeX()][getSizeY()][getSizeZ()]);
	this->getRealSpaceData(real_space);
	SingleParticle2dx::DataStructures::Reconstruction3d::applySymmetry(real_space);
	this->setFourierSpaceData(real_space);
}


void SingleParticle2dx::DataStructures::Reconstruction3d::applySymmetry(real_array3d_type& data)
{
		std::cout << "apply sym" << std::endl;
		
		SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
		
		size_type n = data.shape()[0];
		size_type size = n * n * n;
	    float* float_data_3d;
	    float_data_3d = (float*) malloc(size * sizeof(float));
		std::copy(data.origin(), data.origin()+data.num_elements(), float_data_3d );

	    EMAN::EMData* eman_data = new EMAN::EMData;
	    eman_data->set_data(float_data_3d, n, n, n);
	    eman_data = eman_data->symvol(config->getSymmetryString().c_str());

		std::copy(eman_data->get_data(), eman_data->get_data()+size, data.origin() );
		
		delete eman_data;
}


void SingleParticle2dx::DataStructures::Reconstruction3d::addDataToReconstruction(Reconstruction3d& rhs)
{
	#pragma omp critical (add_data_to_rec)
	{
		SingleParticle2dx::Utilities::DataContainerFunctions::addData(rhs.m_fourier_data.get(), rhs.m_number_of_particles, this->m_fourier_data.get());
	}
}


void SingleParticle2dx::DataStructures::Reconstruction3d::calculateCM()
{
	real_array3d_type real_space = real_array3d_type(boost::extents[getSizeX()][getSizeY()][getSizeZ()]);
	getRealSpaceData(real_space);
	
	size_type n = getSizeX();
	value_type cm_x = 0;
	value_type cm_y = 0;
	value_type total_mass = 0;
	value_type density;
	value_type r;
	
	for (size_type i=0; i<getSizeX(); i++)
	{
		for (size_type j=0; j<getSizeY(); j++)
		{
			//for (size_type k=n/2; k<n/2+1; k++)
			for (size_type k=0; k<getSizeZ(); k++)
			{
				r = sqrt((i-n/2)*(i-n/2) + (j-n/2)*(j-n/2));
				if( r < 30 )
				{
					density = real_space[k][j][i];
					total_mass += density;
					cm_x += i * density;
					cm_y += j * density;
				}
			}
		}
	}
	
	std::cout << "cm_x: " << cm_x/(total_mass) << std::endl;
	std::cout << "cm_y: " << cm_y/(total_mass) << std::endl;
	
	size_type shift_x = floor(cm_x/(total_mass)-n/2+0.5);
	size_type shift_y = floor(cm_y/(total_mass)-n/2+0.5);
	
	std::cout << "shift_x: " << shift_x << std::endl;
	std::cout << "shift_y: " << shift_y << std::endl;
	
	SingleParticle2dx::DataStructures::Reconstruction3d::shiftVolume(shift_x, shift_y, real_space);
//	SingleParticle2dx::DataStructures::Reconstruction3d::shiftVolume(10, 10, real_space);


//	real_space[75][75][75] = 2000;
//	real_space[75-1][75][75] = 2000;
//	real_space[75][75-1][75] = 2000;
//	real_space[75][75][75-1] = 2000;
//	real_space[75-1][75-1][75] = 2000;
//	real_space[75][75-1][75-1] = 2000;
//	real_space[75-1][75][75-1] = 2000;
//	real_space[75-1][75-1][75-1] = 2000;

	this->setFourierSpaceData(real_space);
}


void SingleParticle2dx::DataStructures::Reconstruction3d::shiftVolume(size_type shift_x, size_type shift_y, real_array3d_type& data)
{
	size_type n = data.shape()[0];
	size_type nx, ny;
	
	real_array3d_type real_new = real_array3d_type(boost::extents[n][n][n]);
	
	for (size_type i=0; i<n; i++)
	{
		for (size_type j=0; j<n; j++)
		{
			for (size_type k=0; k<n; k++)
			{
				nx = i+shift_x;
				ny = j+shift_y;
				if( (nx>=0) && (nx<n) && (ny>=0) && (ny<n) )
				{
					real_new[k][nx][ny] = data[k][i][j];
				}
			}
		}
	}
	
	std::copy(real_new.origin(), real_new.origin()+(n*n*n), data.origin() );
}


void SingleParticle2dx::DataStructures::Reconstruction3d::reqularizeFSCValues(std::vector<value_type>& fsc)
{
	value_type t = 0.43;
	//value_type t = 0.143;
	//value_type t = 0.5;
	bool flag = true;
	for (size_type i=0; i<static_cast<size_type>(fsc.size()); i++)
	{
		if ( !flag )
		{
			fsc[i] = value_type(0);
		}
		
		if ( fsc[i] < t )
		{
			fsc[i] = value_type(0);
			flag = false;
		}
	}
}


void SingleParticle2dx::DataStructures::Reconstruction3d::copyData(Reconstruction3d& rhs)
{
	for (size_type i=0; i<getSizeX(); i++)
	{
		for (size_type j=0; j<getSizeY(); j++)
		{
			for (size_type k=0; k<getSizeZ(); k++)
			{
				(*this)(i,j,k) = rhs(i,j,k);
			}
		}
	}
}


void SingleParticle2dx::DataStructures::Reconstruction3d::applySqrtFSCFilter(std::vector<value_type>& fsc)
{
	value_type r;
	value_type w;
	value_type dx;
	size_type n = getSizeX();
	
	reqularizeFSCValues(fsc);
	
	for (size_type i=0; i<getSizeX(); i++)
	{
		for (size_type j=0; j<getSizeY(); j++)
		{
			for (size_type k=0; k<getSizeZ(); k++)
			{
				r = sqrt( (i-n/2)*(i-n/2) + (j-n/2)*(j-n/2) + (k-n/2)*(k-n/2) );
				
				if ( ceil(r) < n/2 )
				{
					dx = 1 - ( r - floor(r));
					//w = dx * sqrt(fsc[floor(r)]) + (1-dx) * sqrt(fsc[ceil(r)]);
					w = sqrt(fsc[floor(r)]);
					
					if( fsc[floor(r)] > 0.0 )
					{
						w = 1;
					}
					else
					{
						w = 0;
					}
					
					
				}
				else
				{
					w = 0;
				}
				
				(*this)(i,j,k) *= w;
				
			//	std::cout << w << " " << (*this)(i,j,k) << std::endl;
			}
		}
	}
}


void SingleParticle2dx::DataStructures::Reconstruction3d::applyFinalFSCFilter(std::vector<value_type>& fsc)
{
	size_type r;
	value_type w;
	value_type dx;
	size_type n = getSizeX();
	
	reqularizeFSCValues(fsc);
	
	for (size_type i=0; i<getSizeX(); i++)
	{
		for (size_type j=0; j<getSizeY(); j++)
		{
			for (size_type k=0; k<getSizeZ(); k++)
			{
				r = round(sqrt( (i-n/2)*(i-n/2) + (j-n/2)*(j-n/2) + (k-n/2)*(k-n/2) ));
				
				if ( r < (n/2-1) )
				{
					dx = 1 - ( r - floor(r));
					//w = dx * sqrt((2*fsc[floor(r)])/(fsc[floor(r)]+1)) + (1-dx) * sqrt((2*fsc[ceil(r)])/(fsc[ceil(r)]+1));
					w = sqrt((2*fsc[floor(r)])/(fsc[floor(r)]+1));
				}
				else
				{
					w = 0;
				}
				
				(*this)(i,j,k) *= w;
				
			//	std::cout << w << " " << (*this)(i,j,k) << std::endl;
			}
		}
	}
}

void SingleParticle2dx::DataStructures::Reconstruction3d::applyNegativeBFactor(std::vector<value_type>& fsc, value_type resolution, value_type factor)
{
	//real_array3d_type amp = real_array3d_type(boost::extents[getSizeX()][getSizeY()][getSizeZ()]);
	//real_array3d_type phas = real_array3d_type(boost::extents[getSizeX()][getSizeY()][getSizeZ()]);
	
	reqularizeFSCValues(fsc);
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	value_type re_tmp, im_tmp;
	value_type re_new, im_new;
	value_type amp, phas;
	value_type amp_new;
	
	value_type c;
	value_type pi = config->getPI();
	
	value_type dx;
	value_type w;
	size_type r;
	size_type n = getSizeX();
	
	for (size_type i=0; i<getSizeX()-1; i++)
	{
		for (size_type j=0; j<getSizeY()-1; j++)
		{
			for (size_type k=0; k<getSizeZ()-1; k++)
			{
				re_tmp = (*m_fourier_data.get())[i][j][k].real();
				im_tmp = (*m_fourier_data.get())[i][j][k].imag();
				amp = sqrt( re_tmp*re_tmp + im_tmp*im_tmp );
				phas = atan(re_tmp/im_tmp) * 180.0 / pi;
				
				//std::cout << re_tmp / im_tmp << " " << atan( re_tmp / im_tmp ) << std::endl;
				
				r = round(sqrt( (i-n/2)*(i-n/2) + (j-n/2)*(j-n/2) + (k-n/2)*(k-n/2) ));
				dx = 1 - ( r - floor(r));
				w = dx * sqrt((2*fsc[floor(r)])/(fsc[floor(r)]+1)) + (1-dx) * sqrt((2*fsc[ceil(r)])/(fsc[ceil(r)]+1));
				
				c = exp(-factor / ( 4 * resolution * resolution)) * w;
				
				amp_new = c * amp;
				
				re_new = sqrt( abs(amp_new*amp_new - tan(phas * pi / 180.0)) );
				im_new = re_new * tan(phas * pi / 180.0);
				
				if ( w < 0.01 || !std::isfinite(w) )
				{
					re_new = 0.0;
					im_new = 0.0;
				}
				
			//	std::cout << w << " " << c << " " << re_new << " " << im_new << std::endl;
				
			//	(*m_fourier_data.get())[i][j][k].real() = re_new;
			//	(*m_fourier_data.get())[i][j][k].imag() = im_new;
			
				(*m_fourier_data.get())[i][j][k].imag( c * (*m_fourier_data.get())[i][j][k].imag() );
				
			}
		}
	}
	
	std::cout << "bfactor done" << std::endl;

	applyMask();
}


void SingleParticle2dx::DataStructures::Reconstruction3d::applyFinalMask()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	value_type r = config->getReconstructionMaskingRadius();
	value_type dr = config->getReconstructionMaskingdR();
	value_type h = config->getReconstructionMaskingHeight();
	value_type dh = config->getReconstructionMaskingdH();

	setMaskingMethod(config->getFinalReconstructionMaskingMethod());

	config->setReconstructionMaskingRadius(config->getFinalReconstructionMaskingRadius());
	config->setReconstructionMaskingdR(config->getFinalReconstructionMaskingdR());
	config->setReconstructionMaskingHeight(config->getFinalReconstructionMaskingHeight());
	config->setReconstructionMaskingdH(config->getFinalReconstructionMaskingdH());

	applyMask();

	config->setReconstructionMaskingRadius(r);
	config->setReconstructionMaskingdR(dr);
	config->setReconstructionMaskingHeight(h);
	config->setReconstructionMaskingdH(dh);

	setMaskingMethod(config->getReconstructionMaskingMethod());
}


void SingleParticle2dx::DataStructures::Reconstruction3d::setProjectionMethod ( size_type key )
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();	

//	if(m_minimal)
//	{
//		std::cout << "::REC USING MINIMAL SETTING" << std::endl;
//		
//		m_projection_strategy.reset( new SingleParticle2dx::Methods::EMANProjectionMethod( this ) );
//		#ifdef USE_CUDA
//			m_projection_strategy.reset( new SingleParticle2dx::Methods::CUDAProjectionMethod( this ) );
//		#else
//			m_projection_strategy.reset( new SingleParticle2dx::Methods::EMANProjectionMethod( this ) );
//		#endif
//		return;
//	}

	if ( config->getCacheProjections() )
	{
		m_projection_strategy.reset( new SingleParticle2dx::Methods::CachedProjectionMethod( this ) );
		return;
	}
	
	if ( config->getParallelProjection() )
	{
		m_projection_strategy.reset( new SingleParticle2dx::Methods::ThreadSafeProjectionMethod( this ) );
		return;
	}

	switch (static_cast<int>(key))
	{
		case 0:
			m_projection_strategy.reset( new SingleParticle2dx::Methods::RealSpaceProjectionMethod( this ) );
			break;
		case 1:
			m_projection_strategy.reset( new SingleParticle2dx::Methods::FourierSpaceProjectionMethod( this ) );
			break;
		case 2:
			std::cout << "eman proj" << std::endl;
			m_projection_strategy.reset( new SingleParticle2dx::Methods::EMANProjectionMethod( this ) );
			break;
		case 3:
			m_projection_strategy.reset( new SingleParticle2dx::Methods::EMANFourierGriddingProjector( this ) ); 
			break;
		#ifdef USE_CUDA
		case 4:
			std::cout << "cuda proj" << std::endl;
			m_projection_strategy.reset( new SingleParticle2dx::Methods::CUDAProjectionMethod( this ) ); 
			break;
		#endif
		default:
			m_projection_strategy.reset( new SingleParticle2dx::Methods::EMANProjectionMethod( this ) );
	}
}


void SingleParticle2dx::DataStructures::Reconstruction3d::setRefinementMethod ( size_type key )
{
	switch (static_cast<int>(key))
	{
		case 0:
			std::cout << "dummy set :AH" << std::endl;
			m_refinement_strategy.reset( new SingleParticle2dx::Methods::DummyRefinementMethod( this ) );
			break;
		case 1:
			m_refinement_strategy.reset( new SingleParticle2dx::Methods::CCRefinementMethod( this ) );
			break;
		case 2:
			m_refinement_strategy.reset( new SingleParticle2dx::Methods::OMPCCRefinementMethod( this ) );
			break;
		default:
			std::cerr << "unknown refinement method" << std::endl;
			m_refinement_strategy.reset( new SingleParticle2dx::Methods::CCRefinementMethod( this ) );
			//throw std::runtime_error("Bad operation");
	}
}


void SingleParticle2dx::DataStructures::Reconstruction3d::setBackprojectionMethod ( size_type key )
{
	switch (static_cast<int>(key))
	{
		case 0:
			m_reconstruction_strategy.reset( new SingleParticle2dx::Methods::BackprojectionReconstructionMethod( this ) );
			break;
		case 1:
			m_reconstruction_strategy.reset( new SingleParticle2dx::Methods::EMANBackprojectionMethod( this ) );
			break;
		case 2:
			m_reconstruction_strategy.reset( new SingleParticle2dx::Methods::FFTEMANBackprojectionMethod( this ) );
			break;
		case 3:
			m_reconstruction_strategy.reset( new SingleParticle2dx::Methods::MultiThreadedEMANBackprojectionMethod( this ) );
			break;
		case 4:
			m_reconstruction_strategy.reset( new SingleParticle2dx::Methods::EMANNNReconstructor( this ) );
			break;
		default:
			m_reconstruction_strategy.reset( new SingleParticle2dx::Methods::FFTEMANBackprojectionMethod( this ) );
			std::cerr << "unknown back-projection method" << std::endl;
			//throw std::runtime_error("Bad operation");
	}
}


void SingleParticle2dx::DataStructures::Reconstruction3d::setMissingConeMethod ( size_type key )
{
	switch (static_cast<int>(key))
	{
		case 0:
			m_missingcone_strategy.reset( new SingleParticle2dx::Methods::DummyMissingConeMethod( this ) );
			break;
		case 1:
			m_missingcone_strategy.reset( new SingleParticle2dx::Methods::ErrorReductionRestoreMissingConeMethod( this ) );
			break;
		default:
			m_missingcone_strategy.reset( new SingleParticle2dx::Methods::DummyMissingConeMethod( this ) );
			//std::cerr << "unknown missing cone method for 3d reconstruction" << std::endl;
			//throw std::runtime_error("Bad operation");
	}
}


void SingleParticle2dx::DataStructures::Reconstruction3d::setMaskingMethod ( size_type key )
{
	switch (static_cast<int>(key))
	{
		case 0:
			m_3dmasking_strategy.reset( new SingleParticle2dx::Methods::Dummy3dMaskingMethod( this ) );
			break;
		case 1:
			m_3dmasking_strategy.reset( new SingleParticle2dx::Methods::Cyclic3dMaskingMethod( this ) );
			break;
		case 2:
			m_3dmasking_strategy.reset( new SingleParticle2dx::Methods::Spherical3dMaskingMethod( this ) );
			break;
		case 3:
			m_3dmasking_strategy.reset( new SingleParticle2dx::Methods::Layer3dMaskingMethod( this ) );
			break;
		case 4:
			m_3dmasking_strategy.reset( new SingleParticle2dx::Methods::Ellipse3dMaskingMethod( this ) );
			break;
		default:
			std::cerr << "unknown mask method for 3d reconstruction" << std::endl;
			m_3dmasking_strategy.reset( new SingleParticle2dx::Methods::Dummy3dMaskingMethod( this ) );
			//throw std::runtime_error("Bad operation");
	}
}


void SingleParticle2dx::DataStructures::Reconstruction3d::setMinimal(bool rhs)
{
	m_minimal = rhs;
}


void SingleParticle2dx::DataStructures::Reconstruction3d::updateAngularChangeMeasure(const value_type rhs)
{
	#pragma omp critical (increase_change)
	{
		m_angular_change_measure += rhs;
	}
}


namespace SingleParticle2dx
{
	namespace DataStructures
	{
		void swap (Reconstruction3d& r1, Reconstruction3d& r2)
		{
			boost::swap (r1.m_fourier_data, r2.m_fourier_data);
			boost::swap (r1.m_projection_strategy, r2.m_projection_strategy);
			boost::swap (r1.m_refinement_strategy, r2.m_refinement_strategy);
			boost::swap (r1.m_missingcone_strategy, r2.m_missingcone_strategy);
			boost::swap (r1.m_reconstruction_strategy, r2.m_reconstruction_strategy);
						
			std::swap (r1.m_angular_change_measure, r2.m_angular_change_measure);
			std::swap (r1.m_timer, r2.m_timer);
			std::swap (r1.m_number_of_particles, r2.m_number_of_particles);
			
			std::swap (r1.m_minimal, r2.m_minimal);
		}
		
	} /* DataStructures */

} /* SingleParticle2dx */
