#ifndef SINGLE_PARTICLE_2DX_PARTICLE_HPP
#define SINGLE_PARTICLE_2DX_PARTICLE_HPP

namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class Particle;
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
		
		/**
		 *  @brief     Representation of one single particle
		 *  @details   The central class of SingleParticle2dx. Here we store the individual particles.
		 *  @author    Sebastian Scherer
		 *  @version   0.2
		 *  @date      2012
		 *  @copyright GNU Public License
		 *  @todo      include shift
		 */
		class Particle : public SingleParticle2dx::DataStructures::Abstract2dData, public SingleParticle2dx::DataStructures::InterfaceToString
		{

		public:

			/** Type storing the size of the particle image */
			typedef SingleParticle2dx::size_type size_type;


			/** Value type of the stored image information in Fourier space */
			typedef SingleParticle2dx::value_type value_type;


			/** Type of the Fourier transformed value representing real and imaginary part */
			typedef SingleParticle2dx::fft_type fft_type;


			/** Array type of the whole Fourier transformed image */
			typedef SingleParticle2dx::fft_array2d_type fft_array_type;
			
			
			/** Array type of the whole reak space image */
			typedef SingleParticle2dx::real_array2d_type real_array_type;


			/** 
			 *  @brief      Default constructor
			 *  @details    Calls the default constructor of all internal variables and set the fourier
			 *              component to a 1x1 array with value (0,0)			
			 */
			Particle ();


			/**
			 *  @brief      Constructor
			 *  @details    Costum constructor
			 *  @param[in]  size_x image size x
			 *  @param[in]  size_y image size y
			 *  @param[in]  o Orientation
			 *  @param[in]  i Global information
			 *  @post       m_orientation_old set to (0,0,0) and m_orientation_new set to passed 
			 *              orientation.
			 */
			Particle (size_type size_x, size_type size_y, SingleParticle2dx::DataStructures::Orientation o, SingleParticle2dx::DataStructures::GlobalParticleInformation i);
			Particle (size_type size_x, size_type size_y);


			/**
			 *  @brief      Destructor
			 *  @details    All the allocated memory is freed afer calling the Destructor
			 *  @post       Memory freed
			 */
			virtual ~Particle ();


			/**
			 *  @brief      Copy-Constructor
			 *  @details    Makes a deep copy of the passed Particle
			 *  @param[in]  rhs Particle to copy
			 */
			Particle (Particle const& rhs);


			/**
			 *  @brief      Assignment operator
			 *  @details    Invokes a deep copy of the passed Particle
			 *  @param[in]  rhs Particle to assign
			 */
			Particle& operator= (Particle rhs);


			/**
			 *  @brief      Friend swap function
			 *  @details    Exchanges two Particles by exchanging the pointers to the members of each 
			 *              Particle by means of std::swap. This technique enables the copy-and-swap
			 *              trick.
			 *  @param[in]  p1 first Particle
			 *  @param[in]  p2 second Particle
			 */
			friend void swap (Particle& p1, Particle& p2);


			/**
			 *  @brief      Access operator
			 *  @details    Customized ()-operator
			 *  @return     Reference to fourier data point at position (i,j)
			 */
			fft_type& operator () (const size_type i, const size_type j);


			/**
			 *  @brief      Return the initial orientation of the particle
			 *  @details    Trivial get-Operation
			 *  @return     Reference to initial orientation of the particle
			 */
			SingleParticle2dx::DataStructures::Orientation& getInitialOrientation();


			/**
			 *  @brief      Return the latest orientation of the particle
			 *  @details    Trivial get-Operation
			 *  @return     reference to latest orientation of the particle
			 */
			SingleParticle2dx::DataStructures::Orientation& getOldOrientation();


			/**
			 *  @brief      Return the second latest orientation of the particle
			 *  @details    Trivial get-Operation
			 *  @return     reference to second latest orientation of the particle
			 */
			SingleParticle2dx::DataStructures::Orientation& getNewOrientation();


			/**
			 *  @brief      Returns the GlobalParticleInformation of the particle
			 *  @details    Trivial get-Operation
			 *  @return     Reference to GlobalParticleInformation of the particle
			 */
			SingleParticle2dx::DataStructures::GlobalParticleInformation& getGlobalParticleInformation();
			
			SingleParticle2dx::DataStructures::ClassInformation& getClassInformation();
			
			void setClassInformation(SingleParticle2dx::DataStructures::ClassInformation rhs);
			

			/**
			 *  @brief      Updates the orientation of the particle
			 *  @details    Due to the single particle refinement steps the have to update the 
			 *              orientation of the particle. After applying this function the former 
			 *              orientation is copied to m_orientation_old orientation, thus we easily can 
			 *              check for the angular change in every iteration step
			 *  @param[in]  o new orientation of the particle
			 *  @post       m_orientation_new set to passed new Orientation, m_orientation_old set to the former value of m_orientation_old.
			 */
			void updateOrientation (SingleParticle2dx::DataStructures::Orientation o);
			
			
			void setOrientation (SingleParticle2dx::DataStructures::Orientation o);
			
			
			void setOldOrientation (SingleParticle2dx::DataStructures::Orientation o);
			
			
			void setNewOrientation (SingleParticle2dx::DataStructures::Orientation o);
			

			bool checkParticle();
			
			/**
			 *  @brief      Returns the last angular change
			 *  @details    Angular change defined as norm of the difference vector between
			 *              the current and the last orientation (normal vector)
			 *  @return     Angular change of the last refinement step
			 */
			value_type getLastAngularChange();


			/**
			 *  @brief      Set the shift of the particle
			 *  @details    Trivial get-Operation
			 *  @return     s Shift to set
			 */
			void setParticleShift( SingleParticle2dx::DataStructures::ParticleShift s);
			
			
			/**
			 *  @brief      Apply particle shift to the particle
			 *  @details    Transform to realspace, apply shift, back-transform and store the update data
			 */
			void updateParticleShift();


			/**
			 *  @brief      Get Particle Number
			 *  @details    Numbers are set when picking the container by means of calling the corresponding 
			 *              function on the container. This numbering equals the position of the particle
			 *              in the container
			 *  @return     Particle number
			 */
			size_type getParticleNumber();
			
			
			/**
		 	 *  @brief      Set Particle Number
		 	 *  @details    Numbers are set when picking the container by means of calling the corresponding 
		 	 *              function on the container. This numbering equals the position of the particle
		 	 *              in the container
		 	 *  @param[in]  number Particle number to set
		 	 */
			void setParticleNumber(const size_type number);

			
			/**
			 *  @brief      Scale the image
			 *  @details    After scaling only the s-% of the middle of the particle are kept
			 *  @param[in]  s scaling parameter between 0 and 1
			 */
			void scale(value_type s);
			
			
			void setMaskingMethod( value_type key );
			
			
			/**
			 *  @brief      Return Shift of a particle
			 *  @details    Trivial get-Operation
			 *  @return     Reference to the particle shift
			 */
			SingleParticle2dx::DataStructures::ParticleShift& getParticleShift();
						
			void setUseForReconstruction(const bool use_for_reconstruction);
		
		
			bool getUseForReconstruction() const;
			
		
			void flipXAxis();
			void flipYAxis();
			void flipXYAxis();
			
			std::string getDataString();
		
		
			std::string getDescString();
			
		
			void setSimMeasure(value_type rhs);
	
	
			value_type getSimMeasure();
			
		
			virtual void setupFromConfig();
			
			value_type getQuality();
	
	
			void setQuality (value_type rhs);
			
		
			value_type getWeight();
	
	
			void setWeight (value_type rhs);
		
			
			std::vector<Particle*> getNeighbors();
	
	
			std::vector<value_type> getDistances();
	
	
			value_type getMaxDistance();
	
	
			void addNeighbor(Particle* rhs);
	
	
			void resetNeighbors();
			
		
			value_type getConsistency();
		
		
			void setConsistency( value_type rhs );
		
			
			void calculateConsistency();
		
			
			void calculateDistances();
			
			
			bool getForceDontUse();
			
			
			void setForceDontUse( bool rhs );
			
			
			size_type getContainerNumber() const;
			
			
			void setContainerNumber(size_type rhs);
			
			
			bool getRejectTreeAlignment();
			
			
			void setRejectTreeAlignment(bool rhs);
			
			
			bool getRejectTreeSplitting();
			
			
			void setRejectTreeSplitting(bool rhs);
			
			
			void applyParticleFingerPrint(SingleParticle2dx::DataStructures::ParticleFingerPrint& fp);
			
			value_type calculateDensity();
			
			void setIsCMParticle(bool is_cm_particle);
			bool getIsCMParticle();
			
			void setToDelete(bool rhs);
			bool getToDelete();
			
			size_type getClassNumber() const;
			size_type getTrueClassNumber() const;
			
			void setClassNumber(const size_type rhs);
			void setTrueClassNumber(const size_type rhs);
			
			SingleParticle2dx::DataStructures::CTFParticleInformation getCTFInfo();
			void setCTFInformation(SingleParticle2dx::DataStructures::CTFParticleInformation rhs);

		protected:

			/**
			 *  @brief      Resets whole Fourier data array to zero
			 *  @details    Fast implementation by means of std::fill
			 *  @post       Fourier data array filled with fft_type(0,0)
			 */
			void resetFourierData();

		private:

			/** @brief Initial particle orientation, gained from the original image */
			SingleParticle2dx::DataStructures::Orientation m_orientation_original;

			/** @brief Particle orientation determined by the last refinement step */
			SingleParticle2dx::DataStructures::Orientation m_orientation_new;

			/** @brief Particle orientation determined by the second last refinement step */
			SingleParticle2dx::DataStructures::Orientation m_orientation_old;

			/** @brief Global particle information */
			SingleParticle2dx::DataStructures::GlobalParticleInformation m_global_information;
			SingleParticle2dx::DataStructures::CTFParticleInformation m_ctf_information;
			
			
			SingleParticle2dx::DataStructures::ClassInformation m_class_information;

			/** @brief Optimal shift of the particle */
			SingleParticle2dx::DataStructures::ParticleShift m_optimal_shift;
			
			/** @brief Number of the particle in the container */
			size_type m_particle_number;
			
			bool m_use_for_reconstruction;
			
			value_type m_sim_measure;
			
			value_type m_qual;
			
			value_type m_consistency;
			
			value_type m_weight;
			
			std::vector<Particle*> m_neighbors;
		
			std::vector<value_type> m_distances;
			
			bool m_force_dont_use;
						
			size_type m_container_number;
			
			bool m_reject_tree_alignment;

			bool m_reject_tree_splitting;
			
			bool m_is_cm_particle;
			
			bool m_to_delete;
			
			friend class boost::serialization::access;
			   
			template<class Archive>
			void serialize(Archive & ar, const unsigned int version)
			{
				ar & boost::serialization::base_object<SingleParticle2dx::DataStructures::Abstract2dData>(*this);
				ar & m_orientation_original;
				ar & m_orientation_new;
				ar & m_global_information;
				ar & m_ctf_information;
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
				ar & m_is_cm_particle;
				
				ar & m_class_information;
			}

		};
		
		static bool compareParticles(Particle p1, Particle p2)
		{
			return (p1.getGlobalParticleInformation().getImageNumber() < p2.getGlobalParticleInformation().getImageNumber());
		}
		
		static bool compareParticlesPosDep(Particle p1, Particle p2)
		{
			if ( p1.getGlobalParticleInformation().getPositionY() < p2.getGlobalParticleInformation().getPositionY())
			{
				return true;
			}
			else if ( p1.getGlobalParticleInformation().getPositionY() > p2.getGlobalParticleInformation().getPositionY())
			{
				return false;
			}
			else
			{
				return p1.getGlobalParticleInformation().getPositionX() < p2.getGlobalParticleInformation().getPositionX();
			}
		}
		
	} /* DataStructures */

} /* SingleParticle2dx */

#endif /* end of include guard: SINGLE_PARTICLE_2DX_PARTICLE_HPP */
