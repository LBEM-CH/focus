#ifndef PARTICLEFINGERPRINT_HPP_C9KPNL94
#define PARTICLEFINGERPRINT_HPP_C9KPNL94



namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class ParticleFingerPrint;
	}
}


#include <boost/scoped_ptr.hpp>
#include <boost/serialization/base_object.hpp>

#include "../Typedefs.hpp"
#include "../DataStructures.hpp"
#include "InterfaceToString.hpp"


namespace SingleParticle2dx
{
	namespace DataStructures
	{
		
		class ParticleFingerPrint
		{

		public:

			typedef SingleParticle2dx::size_type size_type;

			typedef SingleParticle2dx::value_type value_type;


			ParticleFingerPrint ();
			
			
			ParticleFingerPrint (SingleParticle2dx::DataStructures::Particle& rhs);


			virtual ~ParticleFingerPrint ();


			ParticleFingerPrint (ParticleFingerPrint const& rhs);


			ParticleFingerPrint& operator= (ParticleFingerPrint rhs);


			friend void swap (ParticleFingerPrint& p1, ParticleFingerPrint& p2);			
			

			SingleParticle2dx::DataStructures::Orientation m_orientation_original;


			SingleParticle2dx::DataStructures::Orientation m_orientation_new;


			SingleParticle2dx::DataStructures::Orientation m_orientation_old;


			SingleParticle2dx::DataStructures::GlobalParticleInformation m_global_information;


			SingleParticle2dx::DataStructures::ParticleShift m_optimal_shift;

			
			size_type m_particle_number;
			

			bool m_use_for_reconstruction;
			

			value_type m_sim_measure;
			

			value_type m_qual;
			

			value_type m_consistency;
			

			value_type m_weight;
			

			bool m_force_dont_use;


			bool m_reject_tree_alignment;


			bool m_reject_tree_splitting;

			
		private:
			
			friend class boost::serialization::access;			
   
			template<class Archive>
			void serialize(Archive & ar, const unsigned int version)
			{
				ar & m_orientation_original;
				ar & m_orientation_new;
				ar & m_global_information;
				ar & m_optimal_shift;
				ar & m_particle_number;
				ar & m_use_for_reconstruction;
				ar & m_sim_measure;
				ar & m_qual;
				ar & m_weight;
				ar & m_consistency;
				ar & m_force_dont_use;
				ar & m_reject_tree_alignment;
				ar & m_reject_tree_splitting;
			}

		};
		
	} /* DataStructures */

} /* SingleParticle2dx */

#endif /* end of include guard: PARTICLEFINGERPRINT_HPP_C9KPNL94 */ 
