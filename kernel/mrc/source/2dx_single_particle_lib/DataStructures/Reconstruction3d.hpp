#ifndef SINGLE_PARTICLE_2DX_RECONSTRUCTION3D_HPP
#define SINGLE_PARTICLE_2DX_RECONSTRUCTION3D_HPP

namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class Reconstruction3d;
	}
}

#include <boost/scoped_ptr.hpp>
#include <boost/utility.hpp>


#include "../Typedefs.hpp"
#include "../Methods.hpp"
#include "../Utilities.hpp"
#include "../DataStructures.hpp"


namespace SingleParticle2dx
{
	
	namespace DataStructures
	{
		
		/**
		 *  @brief     3d Reconstruction class
		 *  @details   This class is used operate on the 3d reconstruction of the protein
		 *  @author    Sebastian Scherer
		 *  @version   0.1
		 *  @date      2012
		 *  @copyright GNU Public License.
		 *  @todo      protect internal functions
		 */
		class Reconstruction3d : public SingleParticle2dx::DataStructures::Abstract3dData
		{
			
		public:

			/**
			 *  @brief      Constructor
			 *  @details    Costum constructor
			 *  @param[in]  size_x Reconstruction size in x-direction
			 *  @param[in]  size_y Reconstruction size in y-direction
			 *  @param[in]  size_z Reconstruction size in z-direction
			 *  @post       Data set to (0,0)
			 */
			Reconstruction3d ();
			Reconstruction3d (size_type size_x, size_type size_y, size_type size_z, bool minimal=false);


			/**
			 *  @brief      Destructor
			 *  @details    All the allocated memory is freed afer calling the Destructor
			 */
			virtual ~Reconstruction3d ();
			
			Reconstruction3d (Reconstruction3d const& rhs);
			Reconstruction3d& operator= (Reconstruction3d rhs);
			friend void swap (Reconstruction3d& r1, Reconstruction3d& r2);

			/**
			 *  @brief      Update 3d reconstruction from an optimized particle container
			 *  @details    Uses weighted backprojection
			 *  @param[in]  c containing the optimized particles
			 *  @post       Volume updated
			 *  @todo       Implement
			 */
			void updateReconstruction (SingleParticle2dx::DataStructures::ParticleContainer& c, bool useweights = true, bool write_debug_output = false, bool skip_backproj = false);
			
			
			void setProjectionMethod ( size_type key );
		
		
			void setRefinementMethod ( size_type key );
		
		
			void setBackprojectionMethod ( size_type key );
		
		
			void setMaskingMethod ( size_type key );
		
		
			void setMissingConeMethod ( size_type key );
			
			
			/**
			 *  @brief      Generate Initial model
			 *  @details    
			 *  @param[in]  c Container with the particles
			 */
			void generateInitialModel(SingleParticle2dx::DataStructures::ParticleContainer& c);


			/**
			 *  @brief      Restore the missing cone
			 *  @details    
			 *  @todo       Implement
			 */
			void restoreMissingCone ();


			/**
			 *  @brief      Calculates the amount of angular change during the last refinement
			 *  @details
			 *  @return     Angular change    
			 *  @todo       Implement
			 */
			value_type getLastAngleChange ();


			/**
			 *  @brief      Calculates the projection in direction o
			 *  @details    
			 *  @param[in]  o Direction of the projection to calculate
			 *  @param[out] p Reference where to store the calculated projection
			 */
			void calculateProjection(SingleParticle2dx::DataStructures::Orientation o, SingleParticle2dx::DataStructures::Projection2d& p);
		
		
			void forceProjectionPreparation(SingleParticle2dx::DataStructures::ParticleContainer& c);
			
			
			/**
			 *  @brief      Resets angular measurement to zero
			 *  @details    To be reset before each refinement loop
			 *  @post       m_angular_change_measure = 0
			 */
			void resetAngularChangeMeasure();
			
			
			/**
			 *  @brief      Add a certain amount to m_angular_change_measure
			 *  @details    Usually we loop over all particles and sum up their last 
			 *              angular changes
			 *  @post       m_angular_change_measure += rhs
			 */
			void updateAngularChangeMeasure(const value_type rhs);
		
			
			void reqularizeFSCValues(std::vector<value_type>& fsc);
		
		
			void applySqrtFSCFilter(std::vector<value_type>& fsc);
		
		
			void applyFinalFSCFilter(std::vector<value_type>& fsc);


			void applyNegativeBFactor(std::vector<value_type>& fsc, value_type resolution, value_type factor);


			void applyFinalMask();
			
			static void applySymmetry(real_array3d_type& data);
			void applySymmetry();
			
			void calculateCM();
			static void shiftVolume(size_type shift_x, size_type shift_y, real_array3d_type& data);
			
			
			void addDataToReconstruction(Reconstruction3d& rhs);
			
			void clearProjections();
			
			void resetAll();
			
			void copyData(Reconstruction3d& rhs);
			
			void setMinimal(bool rhs);
			
			void setupForBackProjection();
			void finishReconstruction();
			void insertData(SingleParticle2dx::DataStructures::ParticleContainer &c);

		private:
			
			/** angular change measurement */
			value_type m_angular_change_measure;
				
			/** Pointer to the projection strategy */
			boost::scoped_ptr<SingleParticle2dx::Methods::AbstractProjectionMethod> m_projection_strategy;	
			
			/** Pointer to the find best projection strategy */
			boost::scoped_ptr<SingleParticle2dx::Methods::AbstractRefinementMethod> m_refinement_strategy;
			
			/** Pointer to the missing cone strategy strategy */
			boost::scoped_ptr<SingleParticle2dx::Methods::AbstractRestoreMissingConeMethod> m_missingcone_strategy;
					
			/** Reconstruction strategy */
			boost::scoped_ptr<SingleParticle2dx::Methods::AbstractReconstructionMethod> m_reconstruction_strategy;
			
			/** Timer used for measuring the duration of certain steps */
			SingleParticle2dx::Utilities::SystemTimer m_timer;
			
			bool m_minimal;
			size_type m_number_of_particles;
			
			friend class boost::serialization::access;
			   
			template<class Archive>
			void serialize(Archive & ar, const unsigned int version)
			{
				ar & boost::serialization::base_object<SingleParticle2dx::DataStructures::Abstract3dData>(*this);
				ar & m_angular_change_measure;
			}	
			
		};
		
	} /* DataStructures */
	
} /* SingleParticle2dx */

#endif /* end of include guard: SINGLE_PARTICLE_2DX_RECONSTRUCTION3D_HPP */
