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

#include "ParticleFingerPrint.hpp"
#include "../Utilities.hpp"
#include "../Config.hpp"


SingleParticle2dx::DataStructures::ParticleFingerPrint::ParticleFingerPrint ()
 : m_orientation_original ( SingleParticle2dx::DataStructures::Orientation() ),
   m_orientation_new ( SingleParticle2dx::DataStructures::Orientation() ),
   m_orientation_old ( SingleParticle2dx::DataStructures::Orientation() ),
   m_global_information ( SingleParticle2dx::DataStructures::GlobalParticleInformation() ),
   m_optimal_shift ( SingleParticle2dx::DataStructures::ParticleShift() ),
   m_use_for_reconstruction ( true ),
   m_sim_measure ( value_type(0) ),
   m_qual ( value_type(0) ),
   m_consistency ( value_type(1) ),
   m_weight ( value_type(1) ),
   m_force_dont_use (false),
   m_reject_tree_alignment (false),
   m_reject_tree_splitting (false)
{}


SingleParticle2dx::DataStructures::ParticleFingerPrint::ParticleFingerPrint (SingleParticle2dx::DataStructures::Particle& rhs)
 : m_orientation_original ( rhs.getInitialOrientation() ),
   m_orientation_new ( rhs.getNewOrientation() ),
   m_orientation_old ( rhs.getOldOrientation() ),
   m_global_information ( rhs.getGlobalParticleInformation() ),
   m_optimal_shift ( rhs.getParticleShift() ),
   m_use_for_reconstruction ( rhs.getUseForReconstruction() ),
   m_sim_measure ( rhs.getSimMeasure() ),
   m_qual ( rhs.getQuality() ),
   m_consistency ( rhs.getConsistency() ),
   m_weight ( rhs.getWeight() ),
   m_force_dont_use ( rhs.getForceDontUse() ),
   m_reject_tree_alignment ( rhs.getRejectTreeAlignment() ),
   m_reject_tree_splitting ( rhs.getRejectTreeSplitting() )
{}


SingleParticle2dx::DataStructures::ParticleFingerPrint::~ParticleFingerPrint ()
{}


SingleParticle2dx::DataStructures::ParticleFingerPrint::ParticleFingerPrint (ParticleFingerPrint const& rhs)
 : m_orientation_original ( rhs.m_orientation_original ),
   m_orientation_new ( rhs.m_orientation_new ),
   m_orientation_old ( rhs.m_orientation_old ),
   m_global_information ( rhs.m_global_information ),
   m_optimal_shift ( rhs.m_optimal_shift ),
   m_use_for_reconstruction ( rhs.m_use_for_reconstruction ),
   m_sim_measure ( rhs.m_sim_measure ),
   m_qual ( rhs.m_qual ),
   m_consistency ( rhs.m_consistency ),
   m_weight ( rhs.m_weight ),
   m_force_dont_use ( rhs.m_force_dont_use ),
   m_reject_tree_alignment ( rhs.m_reject_tree_alignment ),
   m_reject_tree_splitting ( rhs.m_reject_tree_splitting )
{}


SingleParticle2dx::DataStructures::ParticleFingerPrint& SingleParticle2dx::DataStructures::ParticleFingerPrint::operator= (ParticleFingerPrint rhs)
{
	if(this == &rhs)
	{
		return *this;
	}
	
	swap (*this, rhs);
	return *this;
}


namespace SingleParticle2dx
{
	namespace DataStructures
	{
		void swap (ParticleFingerPrint& p1, ParticleFingerPrint& p2)
		{
			std::swap (p1.m_orientation_original, p2.m_orientation_original);
			std::swap (p1.m_orientation_new, p2.m_orientation_new);
			std::swap (p1.m_orientation_old, p2.m_orientation_old);
			std::swap (p1.m_global_information, p2.m_global_information);
			std::swap (p1.m_optimal_shift, p2.m_optimal_shift);
			std::swap (p1.m_use_for_reconstruction, p2.m_use_for_reconstruction);
			std::swap (p1.m_particle_number, p2.m_particle_number);
			std::swap (p1.m_sim_measure, p2.m_sim_measure);
			std::swap (p1.m_qual, p2.m_qual);
			std::swap (p1.m_weight, p2.m_weight);
			std::swap (p1.m_consistency, p2.m_consistency);
			std::swap (p1.m_force_dont_use , p2.m_force_dont_use );
			std::swap (p1.m_reject_tree_alignment , p2.m_reject_tree_alignment );
			std::swap (p1.m_reject_tree_splitting , p2.m_reject_tree_splitting );
		}
		
	} /* DataStructures */

} /* SingleParticle2dx */
