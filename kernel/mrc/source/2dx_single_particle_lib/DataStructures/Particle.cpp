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

#include <boost/math/special_functions/fpclassify.hpp>

#include "Particle.hpp"
#include "../Utilities.hpp"
#include "../Config.hpp"



SingleParticle2dx::DataStructures::Particle::Particle ()
 : m_orientation_original ( SingleParticle2dx::DataStructures::Orientation() ),
   m_orientation_new ( SingleParticle2dx::DataStructures::Orientation() ),
   m_orientation_old ( SingleParticle2dx::DataStructures::Orientation() ),
   m_global_information ( SingleParticle2dx::DataStructures::GlobalParticleInformation() ),
   m_ctf_information ( SingleParticle2dx::DataStructures::CTFParticleInformation() ),
   m_class_information ( SingleParticle2dx::DataStructures::ClassInformation() ),
   m_optimal_shift ( SingleParticle2dx::DataStructures::ParticleShift() ),
   m_use_for_reconstruction ( true ),
   m_sim_measure ( value_type(0) ),
   m_qual ( value_type(0) ),
   m_consistency ( value_type(1) ),
   m_weight ( value_type(1) ),
   m_force_dont_use( false ),
   m_container_number( size_type(0) ),
   m_reject_tree_alignment(false),
   m_reject_tree_splitting(false),
   m_is_cm_particle(false)
{
	m_fourier_data.reset( new fft_array_type(boost::extents[1][1]) );
	SingleParticle2dx::DataStructures::Abstract2dData::resetData();

	setupFromConfig();
}


SingleParticle2dx::DataStructures::Particle::Particle (size_type size_x, size_type size_y, SingleParticle2dx::DataStructures::Orientation o, SingleParticle2dx::DataStructures::GlobalParticleInformation i)
 : m_orientation_original ( o ),
   m_orientation_new ( o ),
   m_orientation_old ( SingleParticle2dx::DataStructures::Orientation() ),
   m_global_information ( i ),
   m_ctf_information ( SingleParticle2dx::DataStructures::CTFParticleInformation() ),
   m_class_information ( SingleParticle2dx::DataStructures::ClassInformation() ),
   m_optimal_shift ( SingleParticle2dx::DataStructures::ParticleShift() ),
   m_use_for_reconstruction ( true ),
   m_sim_measure ( value_type(0) ),
   m_qual ( value_type(1) ),
   m_consistency ( value_type(1) ),
   m_weight ( value_type(1) ),
   m_force_dont_use( false ),
   m_container_number( size_type(0) ),
   m_reject_tree_alignment(false),
   m_reject_tree_splitting(false),
   m_is_cm_particle(false)
{
	m_fourier_data.reset ( new fft_array_type(boost::extents[size_x][size_y]) );
	SingleParticle2dx::DataStructures::Abstract2dData::resetData();
	
	setupFromConfig();
}


SingleParticle2dx::DataStructures::Particle::Particle (size_type size_x, size_type size_y)
 : m_orientation_original ( SingleParticle2dx::DataStructures::Orientation() ),
   m_orientation_new ( SingleParticle2dx::DataStructures::Orientation() ),
   m_orientation_old ( SingleParticle2dx::DataStructures::Orientation() ),
   m_global_information ( SingleParticle2dx::DataStructures::GlobalParticleInformation() ),
   m_class_information ( SingleParticle2dx::DataStructures::ClassInformation() ),
   m_ctf_information ( SingleParticle2dx::DataStructures::CTFParticleInformation() ),
   m_optimal_shift ( SingleParticle2dx::DataStructures::ParticleShift() ),
   m_use_for_reconstruction ( true ),
   m_sim_measure ( value_type(0) ),
   m_qual ( value_type(1) ),
   m_consistency ( value_type(1) ),
   m_weight ( value_type(1) ),
   m_force_dont_use( false ),
   m_container_number( size_type(0) ),
   m_reject_tree_alignment(false),
   m_reject_tree_splitting(false),
   m_is_cm_particle(false)
{
	m_fourier_data.reset ( new fft_array_type(boost::extents[size_x][size_y]) );
	SingleParticle2dx::DataStructures::Abstract2dData::resetData();
	
	setupFromConfig();
}


SingleParticle2dx::DataStructures::Particle::~Particle ()
{}


SingleParticle2dx::DataStructures::Particle::Particle (Particle const& rhs)
{
	m_fourier_data.reset ( new fft_array_type( *(rhs.m_fourier_data.get())) );
	
	m_orientation_original =rhs.m_orientation_original;
	m_orientation_new = rhs.m_orientation_new;
	m_orientation_old = rhs.m_orientation_old;
	m_global_information = rhs.m_global_information;
	m_class_information = rhs.m_class_information;
	m_ctf_information = rhs.m_ctf_information;
	m_optimal_shift = rhs.m_optimal_shift;
	m_use_for_reconstruction = rhs.m_use_for_reconstruction;
	m_sim_measure = rhs.m_sim_measure;
	m_qual = rhs.m_qual;
	m_consistency = rhs.m_consistency;
	m_weight = rhs.m_weight;
	m_neighbors = rhs.m_neighbors;
	m_distances = rhs.m_distances;
	m_force_dont_use = rhs.m_force_dont_use;
	m_container_number = rhs.m_container_number;
	m_reject_tree_alignment = rhs.m_reject_tree_alignment;
	m_reject_tree_splitting = rhs.m_reject_tree_splitting;
	m_is_cm_particle = rhs.m_is_cm_particle;
	
	setupFromConfig();
}


void SingleParticle2dx::DataStructures::Particle::setupFromConfig()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	setMaskingMethod( config->getParticleMaskingMethod());
	
	m_lowpass_radius =  config->getLPParticleRadius();
	m_lowpass_relax_param = config->getLPParticleSigma();
	
	m_highpass_radius = config->getHPParticleRadius();
	m_highpass_relax_param = config->getHPParticleSigma();
}


SingleParticle2dx::DataStructures::Particle& SingleParticle2dx::DataStructures::Particle::operator= (Particle rhs)
{
	if(this == &rhs)
	{
		return *this;
	}
	
	swap (*this, rhs);
	return *this;
}


SingleParticle2dx::DataStructures::Particle::fft_type& SingleParticle2dx::DataStructures::Particle::operator () (const size_type i, const size_type j)
{
	boost::array<fft_array_type::index,2> idx = {{i,j}};
	return m_fourier_data.get()->operator()(idx);
}


SingleParticle2dx::DataStructures::Orientation& SingleParticle2dx::DataStructures::Particle::getInitialOrientation()
{
	return m_orientation_original;
}


SingleParticle2dx::DataStructures::Orientation& SingleParticle2dx::DataStructures::Particle::getOldOrientation()
{
	return m_orientation_old;
}


SingleParticle2dx::DataStructures::Orientation& SingleParticle2dx::DataStructures::Particle::getNewOrientation()
{
	return m_orientation_new;
}


SingleParticle2dx::DataStructures::GlobalParticleInformation& SingleParticle2dx::DataStructures::Particle::getGlobalParticleInformation()
{
	return m_global_information;
}


SingleParticle2dx::DataStructures::ClassInformation& SingleParticle2dx::DataStructures::Particle::getClassInformation()
{
	return m_class_information;
}


SingleParticle2dx::DataStructures::Particle::size_type SingleParticle2dx::DataStructures::Particle::getClassNumber() const
{
	return m_class_information.getClass();
}


SingleParticle2dx::DataStructures::Particle::size_type SingleParticle2dx::DataStructures::Particle::getTrueClassNumber() const
{
	return m_class_information.getTrueClass();
}
	
	
void SingleParticle2dx::DataStructures::Particle::setClassNumber(const size_type rhs)
{
	m_class_information.setClass(rhs);
}


void SingleParticle2dx::DataStructures::Particle::setTrueClassNumber(const size_type rhs)
{
	m_class_information.setTrueClass(rhs);
}


void SingleParticle2dx::DataStructures::Particle::setClassInformation(SingleParticle2dx::DataStructures::ClassInformation rhs)
{
	m_class_information = rhs;
}


void SingleParticle2dx::DataStructures::Particle::setOrientation (SingleParticle2dx::DataStructures::Orientation o)
{
	m_orientation_original = o;
	m_orientation_old = o;
	m_orientation_new = o;
}


void SingleParticle2dx::DataStructures::Particle::setOldOrientation (SingleParticle2dx::DataStructures::Orientation o)
{
	m_orientation_old = o;
}


void SingleParticle2dx::DataStructures::Particle::setNewOrientation (SingleParticle2dx::DataStructures::Orientation o)
{
	m_orientation_new = o;
}


void SingleParticle2dx::DataStructures::Particle::updateOrientation (SingleParticle2dx::DataStructures::Orientation o)
{
	m_orientation_old = m_orientation_new;
	m_orientation_new = o;
}


SingleParticle2dx::DataStructures::Particle::value_type SingleParticle2dx::DataStructures::Particle::getWeight()
{
	return m_weight;
}


void SingleParticle2dx::DataStructures::Particle::setWeight (value_type rhs)
{
	m_weight = rhs;
}


bool SingleParticle2dx::DataStructures::Particle::getRejectTreeAlignment()
{
	return m_reject_tree_alignment;
}


void SingleParticle2dx::DataStructures::Particle::setRejectTreeAlignment(bool rhs)
{
	m_reject_tree_alignment = rhs;
}


bool SingleParticle2dx::DataStructures::Particle::getRejectTreeSplitting()
{
	return m_reject_tree_splitting;
}


void SingleParticle2dx::DataStructures::Particle::setRejectTreeSplitting(bool rhs)
{
	m_reject_tree_splitting = rhs;
}


std::vector<SingleParticle2dx::DataStructures::Particle*> SingleParticle2dx::DataStructures::Particle::getNeighbors()
{
	return m_neighbors;
}


std::vector<SingleParticle2dx::DataStructures::Particle::value_type> SingleParticle2dx::DataStructures::Particle::getDistances()
{
	return m_distances;
}


SingleParticle2dx::DataStructures::Particle::value_type SingleParticle2dx::DataStructures::Particle::getMaxDistance()
{
	if ( static_cast<size_type>(m_distances.size()) < 1)
	{
		return -1;
	}
	else
	{
		return *(std::max_element(m_distances.begin(), m_distances.end()));
	}
}


void SingleParticle2dx::DataStructures::Particle::addNeighbor(Particle* rhs)
{
	m_neighbors.push_back(rhs);
}


void SingleParticle2dx::DataStructures::Particle::resetNeighbors()
{
	m_neighbors.clear();
}


SingleParticle2dx::DataStructures::CTFParticleInformation SingleParticle2dx::DataStructures::Particle::getCTFInfo()
{
	return m_ctf_information;
}


void SingleParticle2dx::DataStructures::Particle::setCTFInformation(SingleParticle2dx::DataStructures::CTFParticleInformation rhs)
{
	m_ctf_information = rhs;
}


SingleParticle2dx::DataStructures::Particle::value_type SingleParticle2dx::DataStructures::Particle::getLastAngularChange()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	value_type result = 0;
	if (getUseForReconstruction())
	{
		Eigen::Matrix3f rot_new = SingleParticle2dx::Utilities::UtilityFunctions::determineRotation(m_orientation_new);
		Eigen::Matrix3f rot_old = SingleParticle2dx::Utilities::UtilityFunctions::determineRotation(m_orientation_old);

		Eigen::Vector3f norm;
		norm << 0, 0, 1;

		Eigen::Vector3f vec_new = rot_new * norm;
		Eigen::Vector3f vec_old = rot_old * norm;

		value_type dot_product = vec_new.dot(vec_old);
		
		// Is not optimal, but at least works. Issues with eigen3 and math::acos
		if (dot_product>0.9999)
		{
			result = 0;
		}
		else 
		{
			result = acos(dot_product) * 180.0 / config->getPI();
		}
		
		//std::cout << "sim " << result << std::endl;
		//std::cout << "dot " << vec_new.dot(vec_old) << std::endl;
		//std::cout << "acos(dot) " << acos(vec_new.dot(vec_old)) << std::endl;
		//std::cout << "new " << vec_new << std::endl;
		//std::cout << "old " << vec_old << std::endl;
		//std::cout << std::endl;
	
	}
	return result;
}


void SingleParticle2dx::DataStructures::Particle::setParticleShift( SingleParticle2dx::DataStructures::ParticleShift s)
{
	m_optimal_shift = s;
}


SingleParticle2dx::DataStructures::Particle::size_type SingleParticle2dx::DataStructures::Particle::getParticleNumber()
{
	return m_particle_number;
}


void SingleParticle2dx::DataStructures::Particle::setParticleNumber(const size_type number)
{
	m_particle_number = number;
}


void SingleParticle2dx::DataStructures::Particle::setUseForReconstruction(const bool use_for_reconstruction)
{
	m_use_for_reconstruction = use_for_reconstruction;
}


bool SingleParticle2dx::DataStructures::Particle::getUseForReconstruction() const
{
	return m_use_for_reconstruction;
}


void SingleParticle2dx::DataStructures::Particle::setSimMeasure(value_type rhs)
{
	m_sim_measure = rhs;
}


SingleParticle2dx::DataStructures::Particle::value_type SingleParticle2dx::DataStructures::Particle::getSimMeasure()
{
	return m_sim_measure;
}


SingleParticle2dx::DataStructures::Particle::value_type SingleParticle2dx::DataStructures::Particle::getConsistency()
{
	return m_consistency;
}


void SingleParticle2dx::DataStructures::Particle::setConsistency( value_type rhs )
{
	m_consistency = rhs;
}


bool SingleParticle2dx::DataStructures::Particle::getForceDontUse()
{
	return m_force_dont_use;
}


void SingleParticle2dx::DataStructures::Particle::setForceDontUse( bool rhs )
{
	m_force_dont_use = rhs;
}


void SingleParticle2dx::DataStructures::Particle::flipXAxis()
{
	real_array_type rdata( boost::extents[getSizeX()][getSizeY()] );
	getRealSpaceData(&rdata);
	SingleParticle2dx::Utilities::DataContainerFunctions::applyMirrorX(&rdata);
	setFourierSpaceData(&rdata);
}


void SingleParticle2dx::DataStructures::Particle::flipYAxis()
{
	real_array_type rdata( boost::extents[getSizeX()][getSizeY()] );
	getRealSpaceData(&rdata);
	SingleParticle2dx::Utilities::DataContainerFunctions::applyMirrorY(&rdata);
	setFourierSpaceData(&rdata);
}


void SingleParticle2dx::DataStructures::Particle::flipXYAxis()
{
	real_array_type rdata( boost::extents[getSizeX()][getSizeY()] );
	getRealSpaceData(&rdata);
	SingleParticle2dx::Utilities::DataContainerFunctions::applyMirrorXY(&rdata);
	setFourierSpaceData(&rdata);
}


void SingleParticle2dx::DataStructures::Particle::setMaskingMethod( value_type key )
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
			//std::cerr << "Unknow 2d masking method for particle class" << std::endl;
			m_2dmasking_strategy.reset ( new SingleParticle2dx::Methods::Dummy2dMaskingMethod( this ) );
			//throw std::runtime_error("Bad operation");
	}
}


void SingleParticle2dx::DataStructures::Particle::calculateDistances()
{
	m_distances.clear();
	value_type r;
	SingleParticle2dx::DataStructures::GlobalParticleInformation info_n;
	for(size_type i=0; i<static_cast<size_type>(m_neighbors.size()); i++)
	{
		info_n = m_neighbors[i]->getGlobalParticleInformation();
		r = sqrt( static_cast<value_type>((info_n.getPositionX()-m_global_information.getPositionX())*(info_n.getPositionX()-m_global_information.getPositionX()) + (info_n.getPositionY()-m_global_information.getPositionY())*(info_n.getPositionY()-m_global_information.getPositionY())) ); 
		m_distances.push_back(r);
	}
	
	if ( m_neighbors.size() != m_distances.size() )
	{
		std::cerr << "distance calculation went wrong" << std::endl;
		throw std::runtime_error("Bad operation");
	}
	
//	std::cout << "dist_size = " << m_distances.size() << std::endl;
}


void SingleParticle2dx::DataStructures::Particle::scale(value_type s)
{
	if ( fabs(s-value_type(1)) < 1e-5 )
	{
		//std::cout << "don't scale, factor to close to zero" << std::endl;
		return;
	}
	
	real_array_type rdata( boost::extents[getSizeX()][getSizeY()] );
	real_array_type rdata2set( boost::extents[getSizeX()][getSizeY()] );
	getRealSpaceData(&rdata);
	
	size_type n = getSizeX();
	
	value_type pos_x, pos_y;
	size_type x_l, x_u, y_l, y_u;
	value_type s_x, s_y;
	
	for(size_type i=0; i<n; i++)
	{
		for(size_type j=0; j<n; j++)
		{
			pos_x = ((i-n/2)*s) + n/2;
			pos_y = ((j-n/2)*s) + n/2;
			
			x_l = floor(pos_x);
			x_u = x_l+1;
			
			y_l = floor(pos_y);
			y_u = y_l+1;
			
			s_x = pos_x - x_l;
			s_y = pos_y - y_l;
			
			rdata2set[i][j] = (1-s_x) * (1-s_y) * rdata[x_l][y_l]
							+ (1-s_x) * (  s_y) * rdata[x_l][y_u]
							+ (  s_x) * (1-s_y) * rdata[x_u][y_l]
							+ (  s_x) * (  s_y) * rdata[x_u][y_u];
		}
	}	
	setFourierSpaceData(&rdata2set);
}


bool SingleParticle2dx::DataStructures::Particle::checkParticle()
{
	real_array_type rdata( boost::extents[getSizeX()][getSizeY()] );
	getRealSpaceData(&rdata);
	
	if (SingleParticle2dx::Utilities::DataContainerFunctions::checkData(&rdata))
	{
		return false;
	}

	if (SingleParticle2dx::Utilities::DataContainerFunctions::calculateSD(&rdata) < 1e-5)
	{
		return false;
	}
	return true;
}


void SingleParticle2dx::DataStructures::Particle::setToDelete(bool rhs)
{
	m_to_delete = rhs;
}


bool SingleParticle2dx::DataStructures::Particle::getToDelete()
{
	return m_to_delete;
}


void SingleParticle2dx::DataStructures::Particle::updateParticleShift()
{
	SingleParticle2dx::DataStructures::ParticleShift s = getParticleShift();
	real_array_type rdata( boost::extents[getSizeX()][getSizeY()] );
	real_array_type rdata2set( boost::extents[getSizeX()][getSizeY()] );
	getRealSpaceData(&rdata);
	
	size_type ii;
	size_type jj;
	
	for(size_type i=0; i<getSizeX(); i++)
	{
		for(size_type j=0; j<getSizeY(); j++)
		{
			ii = i + s.getShiftX();
			jj = j + s.getShiftY();
			
			if (ii<0)
			{
				ii += getSizeX();
			}
			
			if (jj<0)
			{
				jj += getSizeY();
			}
			
			if (ii >= getSizeX())
			{
				ii -= getSizeX();
			}
			
			if (jj >= getSizeY())
			{
				jj -= getSizeY();
			}
			
			rdata2set[i][j] = rdata[ii][jj];
		}
	}
	setFourierSpaceData(&rdata2set);
	m_optimal_shift.reset();
}


SingleParticle2dx::DataStructures::ParticleShift& SingleParticle2dx::DataStructures::Particle::getParticleShift()
{
	return m_optimal_shift;
}


SingleParticle2dx::DataStructures::Particle::value_type SingleParticle2dx::DataStructures::Particle::getQuality()
{
	return m_qual;
}


void SingleParticle2dx::DataStructures::Particle::setQuality (value_type rhs)
{
	m_qual = rhs;
}


SingleParticle2dx::DataStructures::Particle::size_type SingleParticle2dx::DataStructures::Particle::getContainerNumber() const
{
	return m_container_number;
}


void SingleParticle2dx::DataStructures::Particle::setContainerNumber(size_type rhs)
{
	m_container_number = rhs;
}


void SingleParticle2dx::DataStructures::Particle::setIsCMParticle(bool is_cm_particle)
{
	m_is_cm_particle = is_cm_particle;
}

bool SingleParticle2dx::DataStructures::Particle::getIsCMParticle()
{
	return m_is_cm_particle;
}


SingleParticle2dx::DataStructures::Particle::value_type SingleParticle2dx::DataStructures::Particle::calculateDensity()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	std::vector<Particle*> neighbors = getNeighbors();
	
	if(neighbors.size() < 0.5)
	{
		return -1;
	}
	
	SingleParticle2dx::DataStructures::GlobalParticleInformation part_info;
	
	value_type current_radius;
	value_type x = getGlobalParticleInformation().getPositionX();
	value_type y = getGlobalParticleInformation().getPositionY();
	
	boost::accumulators::accumulator_set<value_type, boost::accumulators::stats<boost::accumulators::tag::max> > acc;
	
	for(int i=0; i< static_cast<int>(neighbors.size()); i++)
	{
		part_info = neighbors[i]->getGlobalParticleInformation();
		current_radius = sqrt((x - part_info.getPositionX()) * (x - part_info.getPositionX()) + (y - part_info.getPositionY()) * (y - part_info.getPositionY()));
		acc(current_radius);
	}
	
	value_type max_r = boost::accumulators::extract_result<boost::accumulators::tag::max>(acc);
	
	return static_cast<value_type>(neighbors.size()) / (config->getPI() * max_r * max_r * 0.0001 * 0.0001); 
}


void SingleParticle2dx::DataStructures::Particle::applyParticleFingerPrint(SingleParticle2dx::DataStructures::ParticleFingerPrint& fp)
{
	m_orientation_original = fp.m_orientation_original;
	m_orientation_new = fp.m_orientation_new;
	m_orientation_old = fp.m_orientation_old;
	m_global_information = fp.m_global_information;
	m_optimal_shift = fp.m_optimal_shift;
	m_use_for_reconstruction = fp.m_use_for_reconstruction;
	m_sim_measure = fp.m_sim_measure;
	m_qual = fp.m_qual;
	m_consistency = fp.m_consistency;
	m_weight = fp.m_weight;
}


void SingleParticle2dx::DataStructures::Particle::calculateConsistency()
{
	size_type n = 6;
	std::vector<Eigen::VectorXf> points;
	std::vector<Particle*> neighbors = getNeighbors();
	
	for (size_type i=0; i<static_cast<size_type>(neighbors.size()); i++ )
	{
		Eigen::VectorXf new_vec(n);
		new_vec[0] = neighbors[i]->getNewOrientation().getTLTAXIS();
		new_vec[1] = neighbors[i]->getNewOrientation().getTLTANG();
		new_vec[2] = neighbors[i]->getNewOrientation().getTAXA();
		new_vec[3] = neighbors[i]->getParticleShift().getShiftX();
		new_vec[4] = neighbors[i]->getParticleShift().getShiftY();
		new_vec[5] = neighbors[i]->getSimMeasure();
		points.push_back(new_vec);
	}
	
	Eigen::VectorXf mean(n);
	
	for(size_type i=0; i<static_cast<size_type>(points.size()); i++)
	{
		mean += points[i];
	}
	mean /= static_cast<value_type>(points.size());
	
//	std::cout << ": mean = " << mean[0] << " " << mean[1] << " " << mean[2] << " " << mean[3] << " " << mean[4] << " " << mean[5] << std::endl;
	
	Eigen::MatrixXf covMat = Eigen::MatrixXf::Zero(n,n);
	
//	std::cout << mean << std::endl;
	
	for(size_type i=0; i<static_cast<size_type>(points.size()); i++)
	{
		Eigen::VectorXf diff = (points[i]-mean).conjugate();
		covMat += diff * diff.adjoint();
	}
	
//	std::cout << ":det = " << covMat.determinant() << std::endl;
	
	value_type det = covMat.determinant();
	
	if ( !std::isfinite(det) )
	{
		setConsistency(0);
		return;
	}
	
	if ( det < 0.001 )
	{
		setConsistency(0);
		return;
	}
	
	//SingleParticle2dx::Utilities::UtilityFunctions::reqularizeMatrix(covMat);
	
//	std::cout << covMat << std::endl;
	
	Eigen::VectorXf vec(n);
	vec[0] = getNewOrientation().getTLTAXIS();
	vec[1] = getNewOrientation().getTLTANG();
	vec[2] = getNewOrientation().getTAXA();
	vec[3] = getParticleShift().getShiftX();
	vec[4] = getParticleShift().getShiftY();
	vec[5] = getSimMeasure();
	
	value_type result = -0.5 * log(covMat.determinant());
	
//	#pragma omp critical (det_output)
	//{
		//std::cout << ":det = " << det << std::endl;
	//}
	
	if ( covMat.determinant() < 0.000001 )
	{
		setConsistency(0);
		return;
	}
	
//	std::cout << ":first term: " << result << std::endl;
	
	Eigen::VectorXf tmp1 = (covMat.inverse()) * (vec-mean);
	value_type tmp2 = (vec-mean).dot(tmp1);
	
	result -= 0.5 * tmp2;
	
//	std::cout << ":second term: " << -0.5 * tmp2 << std::endl;

	if ( boost::math::isnan(result) || boost::math::isnan(-result) )
	{
		std::cout << "::reset CONS realy done now" << std::endl;
		result = 0;
	}
	
	setConsistency(result);
}


std::string SingleParticle2dx::DataStructures::Particle::getDataString()
{
	std::string result = SingleParticle2dx::Utilities::StringFunctions::TtoString(getParticleNumber());
	result += "\t" + getGlobalParticleInformation().getDataString();
	result += "\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString(getUseForReconstruction());
	result += "\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString(getLastAngularChange());
	result += "\t" + getParticleShift().getDataString();
	result += "\t" + getInitialOrientation().getDataString();
	result += "\t" + getOldOrientation().getDataString();
	result += "\t" + getNewOrientation().getDataString();
	result += "\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString(getSimMeasure());
	result += "\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString(getQuality());
	result += "\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString(getConsistency());
	result += "\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString(getWeight());
	result += "\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString(getContainerNumber());
	result += "\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString(getIsCMParticle());
	result += "\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString(calculateDensity());
	result += "\t" + getClassInformation().getDataString();
	
	return result;
}


std::string SingleParticle2dx::DataStructures::Particle::getDescString()
{
	std::string result = "particle_number";
	result += "\t" + getGlobalParticleInformation().getDescString();
	result += "\tuse_for_reconstruction";
	result += "\tlast_angular_change";
	result += "\t" + getParticleShift().getDescString();
	result += "\tinit_tltaxis\tinit_tltang\tinit_taxa";
	result += "\told_tltaxis\told_tltang\told_taxa";
	result += "\tnew_tltaxis\tnew_tltang\tnew_taxa";
	result += "\tsim_measure";
	result += "\tquality_measure";
	result += "\tconst_measure";
	result += "\tweight";
	result += "\tcont_number";
	result += "\tcm_part";
	result += "\tdensity";
	result += "\t" + getClassInformation().getDescString();
	
	return result;
}


namespace SingleParticle2dx
{
	namespace DataStructures
	{
		void swap (Particle& p1, Particle& p2)
		{
			boost::swap (p1.m_fourier_data, p2.m_fourier_data);
			boost::swap (p1.m_2dmasking_strategy, p2.m_2dmasking_strategy);
			std::swap (p1.m_orientation_original, p2.m_orientation_original);
			std::swap (p1.m_orientation_new, p2.m_orientation_new);
			std::swap (p1.m_orientation_old, p2.m_orientation_old);
			std::swap (p1.m_global_information, p2.m_global_information);
			std::swap (p1.m_ctf_information, p2.m_ctf_information);
			std::swap (p1.m_class_information, p2.m_class_information);
			std::swap (p1.m_optimal_shift, p2.m_optimal_shift);
			std::swap (p1.m_use_for_reconstruction, p2.m_use_for_reconstruction);
			std::swap (p1.m_particle_number, p2.m_particle_number);
			std::swap (p1.m_sim_measure, p2.m_sim_measure);
			std::swap (p1.m_qual, p2.m_qual);
			std::swap (p1.m_weight, p2.m_weight);
			std::swap (p1.m_consistency, p2.m_consistency);
			std::swap (p1.m_neighbors, p2.m_neighbors);
			std::swap (p1.m_distances, p2.m_distances);
			std::swap (p1.m_force_dont_use, p2.m_force_dont_use);
			std::swap (p1.m_container_number, p2.m_container_number);
			std::swap (p1.m_reject_tree_alignment, p2.m_reject_tree_alignment);
			std::swap (p1.m_reject_tree_splitting, p2.m_reject_tree_splitting);
			std::swap (p1.m_is_cm_particle, p2.m_is_cm_particle);
		}
		
	} /* DataStructures */

} /* SingleParticle2dx */
