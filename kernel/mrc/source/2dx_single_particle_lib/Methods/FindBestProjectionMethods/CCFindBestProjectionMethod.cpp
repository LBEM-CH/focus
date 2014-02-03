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

#include <eigen3/Eigen/Dense>


#include <boost/filesystem.hpp>

#include <boost/accumulators/accumulators.hpp>
#include <boost/accumulators/statistics/stats.hpp>
#include <boost/accumulators/statistics/mean.hpp>
#include <boost/accumulators/statistics/variance.hpp>

#include "../../Config.hpp"
#include "../../Utilities.hpp"

#include "CCFindBestProjectionMethod.hpp"



SingleParticle2dx::Methods::CCFindBestProjectionMethod::CCFindBestProjectionMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context)
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	m_context = context;
	m_peaksearch_strategy.reset( new SingleParticle2dx::Methods::MaxSearchMethod );
	m_clac_cc_strategy.reset( new SingleParticle2dx::Methods::BasicCalcCCMethod );
	
	setTrialAngleGenerator( config->getTrialAngleGenerator() );
}


SingleParticle2dx::Methods::CCFindBestProjectionMethod::~CCFindBestProjectionMethod ()
{}


void SingleParticle2dx::Methods::CCFindBestProjectionMethod::determineBestProjection(SingleParticle2dx::DataStructures::Particle& p, bool useneighbors, bool write_debug_output)
{	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	//#pragma omp critical (trial_angle_gen_setup)
	//{
		//setTrialAngleGenerator(config->getTrialAngleGenerator());
	//}
	
	SingleParticle2dx::DataStructures::Orientation proj_orientation;
	SingleParticle2dx::DataStructures::Orientation best_orientation;
	SingleParticle2dx::DataStructures::Projection2d proj(p.getSizeX(), p.getSizeY());
	
	SingleParticle2dx::DataStructures::ParticleShift current_shift;
	SingleParticle2dx::DataStructures::ParticleShift optimal_shift;
	
	value_type best_cc_value = -1000;
		
	size_type cc_size = SingleParticle2dx::ConfigContainer::Instance()->getCrossCorrelationWindowSize();
	real_array2d_type cc_data( boost::extents[cc_size][cc_size] );
	
	Eigen::Vector3f current_direction;
	value_type current_cc_value;
	
	SingleParticle2dx::DataStructures::GlobalParticleInformation info = p.getGlobalParticleInformation();
	
	std::vector<SingleParticle2dx::DataStructures::Orientation> o_vec;
	
	#pragma omp critical (eman_const_call_trial_angles)
	{
		m_angle_generator.get()->generateAngles(p.getInitialOrientation(), o_vec );
	}
	
	SingleParticle2dx::DataStructures::Orientation o_trial;
	
	boost::accumulators::accumulator_set<value_type, boost::accumulators::stats<boost::accumulators::tag::mean, boost::accumulators::tag::variance(boost::accumulators::lazy)> > acc;
	
	SingleParticle2dx::DataStructures::Particle p_sum = p;

	if ( useneighbors )
	{
		real_array2d_type part_sum( boost::extents[p.getSizeX()][p.getSizeY()] );
		real_array2d_type part_tmp( boost::extents[p.getSizeX()][p.getSizeY()] );
		p.getRealSpaceData(&part_sum);
        
		std::vector<SingleParticle2dx::DataStructures::Particle*> n_vec = p.getNeighbors();
		std::vector<value_type> dis_vec = p.getDistances();
        
		value_type sigma = SingleParticle2dx::Utilities::UtilityFunctions::calculateCorrespondingSigma(0.25, p.getMaxDistance());
		value_type weight;
        
		SingleParticle2dx::DataStructures::Particle p2add;
		//std::cout << "nn = " << static_cast<size_type>(n_vec.size()) << std::endl;
		for (size_type i=0; i<static_cast<size_type>(n_vec.size()); i++)
		{
			p2add = (*n_vec[i]);
			
			p2add.updateParticleShift();
			
			//n_vec[i]->getRealSpaceData(&part_tmp);
			p2add.getRealSpaceData(&part_tmp);
			weight = SingleParticle2dx::Utilities::UtilityFunctions::calculateNeighborWeight(dis_vec[i], sigma);
        
			if ( (weight<=0) || (weight>1) )
			{
				std::cerr << "weight = " << weight << " which can't be (sigma=" << sigma << ", r=" << dis_vec[i] << ")" << std::endl;
				throw std::runtime_error("Bad operation");
			}
        
			SingleParticle2dx::Utilities::DataContainerFunctions::addRealData(&part_tmp, weight, &part_sum);
		//	SingleParticle2dx::Utilities::DataContainerFunctions::addRealData(&part_tmp, 1, &part_sum);
		}        
		p_sum.setFourierSpaceData(&part_sum);
	}
	
	//use iterators
	for (size_type i=0; i<static_cast<size_type>(o_vec.size()); i++)
	{
		o_trial = o_vec[i];
		current_direction << o_trial.getTLTAXIS(), o_trial.getTLTANG(), o_trial.getTAXA();
		SingleParticle2dx::Utilities::DataContainerFunctions::resetData( &cc_data );
		proj_orientation.setOrientation(current_direction);
		m_context->calculateProjection (proj_orientation, proj);
		m_clac_cc_strategy.get()->calculateCrossCorrelation( p_sum, proj, cc_data );
		
		try
		{
			current_cc_value = m_peaksearch_strategy.get()->findMaximalElementAndSetShift(cc_data, current_shift);
		}
		catch(...)
		{
		//	std::cout << "::caught a CC exception" << std::endl;
			//p.setForceDontUse( true );
			//return;
			continue;
		}
		
		acc(current_cc_value);
				
		if ( best_cc_value <=  current_cc_value )
		{
			optimal_shift = current_shift;
			best_orientation = proj_orientation;
			best_cc_value = current_cc_value;
		}		
	}
	
	m_context->calculateProjection (best_orientation, proj);
	
	if(write_debug_output)
	{
		p.setParticleNumber(p.getGlobalParticleInformation().getImageNumber());
		proj.writeToFile("ref_debug_output/" + SingleParticle2dx::Utilities::StringFunctions::TtoString(p.getParticleNumber())+ "_proj.mrc");
		p.writeToFile("ref_debug_output/" +SingleParticle2dx::Utilities::StringFunctions::TtoString(p.getParticleNumber()) + "_part.mrc");
	}
		
	if ( !p_sum.checkParticle() )
	{
		std::cout << "::ilegal particle" << std::endl;
	}
	
	m_clac_cc_strategy.get()->calculateCrossCorrelation( p_sum, proj, cc_data );
	
	p.setSimMeasure(best_cc_value);
	
	//value_type cc_threshold = SingleParticle2dx::ConfigContainer::Instance()->getCCThreshold();
	//if ( best_cc_value < cc_threshold )
	//{
	//	p.setUseForReconstruction(false);
	//}
	
	value_type mean_value = boost::accumulators::extract_result<boost::accumulators::tag::mean>(acc);
	value_type sd_value = sqrt(boost::accumulators::extract_result<boost::accumulators::tag::variance>(acc));
	p.setQuality( (best_cc_value-mean_value)/sd_value );

	//#pragma omp critical (output_cc)
	//{
	//	std::cout << "\tBest direction: " << best_cc_value << ", " << std::endl << "\tAngle: " << best_orientation.getTLTAXIS() << ", " << best_orientation.getTLTANG() << ", " << best_orientation.getTAXA() << std::endl << "\tShift: " << optimal_shift.getShiftX() << " " << optimal_shift.getShiftY() << std::endl << std::endl;
	//	std::cout << "processing image " << p.getParticleNumber() << " : " << info.getImageNumber() << ", " << info.getPositionX() << ", " << info.getPositionY() << std::endl;
	//	std::cout << "\tbestcc:\t" << best_cc_value << std::endl;
	//	std::cout << "\tmean_cc:\t" << mean_value << std::endl;
	//	std::cout << "\tsd_cc:\t" << sd_value << std::endl;
	//	std::cout << "\tquality:\t" << (best_cc_value-mean_value)/sd_value << std::endl;
	//}
	
	
	
	/*
	for(int i=0; i<cc_size; i++)
	{
		for(int j=0; j<cc_size; j++)
		{
			std::cout << cc_data[i][j] << "\t";
		}
		std::cout << std::endl;
	}
	std::cout << std::endl;
	*/
	
	p.updateOrientation(best_orientation);
	p.setParticleShift(optimal_shift);
}
