#ifndef PARTICLE_CONTAINER_HPP
#define PARTICLE_CONTAINER_HPP

namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class ParticleContainer;
		class Particle;
	}
}

#include <vector>

#include <boost/serialization/vector.hpp>
#include <boost/serialization/array.hpp>
#include <boost/serialization/nvp.hpp>
#include <boost/serialization/split_free.hpp>
#include <boost/serialization/split_member.hpp>
#include <boost/serialization/complex.hpp>

#include <boost/container/stable_vector.hpp>

#include "Particle.hpp"

#include "../DataStructures.hpp"
#include "../Typedefs.hpp"

namespace SingleParticle2dx
{
	namespace DataStructures
	{
		
		/**
		 *  @brief     Particle Container 
		 *  @details   
		 *  @author    Sebastian Scherer
		 *  @version   0.1
		 *  @date      2012
		 *  @copyright GNU Public License
		 *  @todo      Concept for MPI parallelization
		 */
		class ParticleContainer
		{
		public:

			/** Particle array type */
			//typedef std::vector<SingleParticle2dx::DataStructures::Particle> data_array_type;
			typedef boost::container::stable_vector<SingleParticle2dx::DataStructures::Particle> data_array_type;
			
			
			/** Typedef for 2d real space arrays */
			typedef SingleParticle2dx::real_array2d_type real_array_type;
			
			typedef SingleParticle2dx::fft_array2d_type fft_array_type;
			
			/** Typedef for values */
			typedef SingleParticle2dx::value_type value_type;
			
			
			/** Typedef for complex numbers */
			typedef SingleParticle2dx::fft_type fft_type;
			
			
			/** Typedef for size_type */
			typedef SingleParticle2dx::size_type size_type;


			/**
			 *  @brief      Constructor
			 *  @details    Default constructor
			 *  @post       ParticleContainer initialized
			 */
			ParticleContainer ();


			/**
			 *  @brief      Destructor
			 *  @details    All the allocated memory is freed afer calling the Destructor
			 *  @post       Memory freed
			 */
			virtual ~ParticleContainer ();


			/**
			 *  @brief      Add particle to container  
			 *  @param[in]  part Particle to add to the container
			 */
			void addParticle(SingleParticle2dx::DataStructures::Particle& part);
		
			void addParticleFast(SingleParticle2dx::DataStructures::Particle& part);


			/**
			 *  @brief      Random access to particle container
			 *  @param[in]  i Index of the accessed particle
			 *  @return     Reference to the i-th particle
			 */
			SingleParticle2dx::DataStructures::Particle& operator() (const int i);


			/**
			 *  @brief      Returns the number of stored particles
			 *  @return     Number of particles stored in the container
			 */
			int getNumberOfParticles();
			
			
			/**
			 *  @brief      Asignes a unique number to each particle
			 *  @details    Loops over the container and set the number position depenent
			 */
			void setParticleNumbers();
			
			
			/**
			 *  @brief      Write container to disk
			 *  @details    Stores each particle and the average
			 *  @param[in]  foldername Container stored in this folder
			 *  @param[in]  mode 1 means output particles aswell
			 */
			void writeContainerToDisk(std::string foldername, size_type mode);

			size_type getNumberOfClasses();

			void increaseBadParticles();
			

			size_type getBadParticles();
			
			
			void getAverage(real_array_type& ave_im);
			
			
			void clear();
			
			
			/**
			 *  @brief      Picks particles from an image and stores them in the container
			 *  @details    
			 *  @param[in]  foldername Name of the dedicated image folder name, which was created by 2dx
			 *  @param[out] cont Container where to store the particles
			 *  @param[in]  image_number Number of the image from where the particle is picked
			 *              This number is stored in the particle's global information
			 */
			static size_type pickParticlesFromCCProfile(std::string foldername, SingleParticle2dx::DataStructures::ParticleContainer& cont, SingleParticle2dx::DataStructures::PickingDiagnostics& dia, size_type image_number = 0, bool do_double_pick = false);
			
			
			/**
			 *  @brief      Calculates the FSC curve of a container
			 *  @details    
			 *  @param[in]  cont Particle Container to be measeured
			 *  @return     Vector with radius (1st comp) and FSC (2nd component)
			 */
			static std::pair<std::vector<value_type>, std::vector<value_type> > calculateFSC( SingleParticle2dx::DataStructures::ParticleContainer& cont, std::string even_name = "", std::string odd_name = "");
		
			
			static std::pair<std::vector<value_type>, std::vector<value_type> > calculateFSC( SingleParticle2dx::DataStructures::Reconstruction3d& rec1, SingleParticle2dx::DataStructures::Reconstruction3d& rec2, std::string outfile = std::string("FSC.dat"), bool dosmoothing = true );
			
			static std::pair<std::vector<value_type>, std::vector<value_type> > calculateFSC_XY( SingleParticle2dx::DataStructures::Reconstruction3d& rec1, SingleParticle2dx::DataStructures::Reconstruction3d& rec2, bool dosmoothing = true );
			static std::pair<std::vector<value_type>, std::vector<value_type> > calculateFSC_Z( SingleParticle2dx::DataStructures::Reconstruction3d& rec1, SingleParticle2dx::DataStructures::Reconstruction3d& rec2, bool dosmoothing = true );
			
			/**
			 *  @brief      Produces a list of all different initial particle orientations
			 *  @details    
			 *  @param[in]  o_vec List of all different initial particle orientations
			 */
			void getDistinctAngles(std::vector<SingleParticle2dx::DataStructures::Orientation>& o_vec);
		
			
			void writeStatToFile(std::string filename, bool append = false);
		
			
			
			void storeParticleEMAN2(std::string filename);
		
			
			void updateUseForReconstruction();
		
		
			void updateWeightForReconstruction();
		
		
			void updateUseForReconstructionBasedOnWeights();
			
		
			void generateAverageContainer(SingleParticle2dx::DataStructures::ParticleContainer& cont_out, bool applyWeight = false, bool doinparallel = true);
		
		
			void updateInitialTiltGeometryAndShift(SingleParticle2dx::DataStructures::ParticleContainer& cont_out);
			
		
			void sortContainer();
		
		
			void findNeighbors(size_type n, bool local_container_only = false);
			void findNeighborsIncludeClasses(size_type n);
		
			void eraseLastElement();
		
			
			void shuffleContainer(size_type n);
		
		
			void shakeContainer(value_type range);
			
		
			void setAllOrientations( SingleParticle2dx::DataStructures::Orientation o );
			
		
			static void splitContainer(SingleParticle2dx::DataStructures::ParticleContainer& cont_in, SingleParticle2dx::DataStructures::ParticleContainer& cont_1, SingleParticle2dx::DataStructures::ParticleContainer& cont_2);
			static void splitContainerMemSaving(SingleParticle2dx::DataStructures::ParticleContainer& cont_in, SingleParticle2dx::DataStructures::ParticleContainer& cont_1, SingleParticle2dx::DataStructures::ParticleContainer& cont_2);
		
		
			static void mergeContainers(SingleParticle2dx::DataStructures::ParticleContainer& cont_in1, SingleParticle2dx::DataStructures::ParticleContainer& cont_in2, SingleParticle2dx::DataStructures::ParticleContainer& cont_out);
			
		
			static void mergeContainers(SingleParticle2dx::DataStructures::ParticleContainer& cont_large, SingleParticle2dx::DataStructures::ParticleContainer& cont_small);

		
			value_type getTotalSim();
			
			
			data_array_type& getParticles();
		
			void resetSimForAllParticles();
		
		
			void resetWeightForAllParticles();
			
		
			void splitIntoPerImageContainer(std::vector<SingleParticle2dx::DataStructures::ParticleContainer>& cont_vec);
			
		
			void splitIntoPerImageContainer(std::vector<std::vector<SingleParticle2dx::DataStructures::Particle*> >& cont_vec);
			
		
			static void serializeContainerToDisk(SingleParticle2dx::DataStructures::ParticleContainer& cont, bool do_full_write, std::string filename);
		
		
			static void deserializeContainerFromDisk(std::string filename, SingleParticle2dx::DataStructures::ParticleContainer& cont, bool do_full_read);
			
			
			void getCenterOfMass(std::pair<value_type, value_type>& center_of_mass);
			
			size_type getNearestParticleNumber(std::pair<value_type, value_type> center_of_mass);
			
			void deleteParticle(size_type index);
			void deleteParticle(data_array_type::iterator it);
			void deleteImage(size_type image_number);
			
			SingleParticle2dx::DataStructures::Particle& findParticleWithNumber (size_type number);
			
			void deleteMarkedParticles();
			
			void resetImageNumber(size_type image_number);
			
			void calcAndSetConsistency(size_type number_of_diff_containers);
			void selectParticlesBasedOnConsistency(size_type number_of_diff_containers);
			void getParticlePointerVector(size_type cont_number, std::vector<SingleParticle2dx::DataStructures::Particle*>& res);			
			size_type getNumberOfDiffImages();
			
			void applyMaskToContainer();
			void applyMaskToContainerInParallel();
			
			static void splitContainerAccordingToClasses(SingleParticle2dx::DataStructures::ParticleContainer cont, std::vector<SingleParticle2dx::DataStructures::ParticleContainer>& cont_vec);
			void getNumberOfElementsInEachClass( std::vector<size_type>& vec);
			
			void setRandomClasses();
			void setModuloClasses();
			size_type MRAClassify(std::vector<SingleParticle2dx::DataStructures::Reconstruction3d>& rec_vec);
			
			size_type getNumberOfWrongClasses();
			
			void addRandomNoiseToContainer(value_type sd);
			
			void printClassMembers();

		private:
			
			/**
			 *  @brief      Give Fourier space lattice, get real space lattice
			 *  @details    
			 *  @param[in]  rec_lat Reciprocal lattice
			 *  @param[in]  n Size of the image
			 *  @param[out] real_lat Resulting real space lattice
			 */
			static void getRealLattice(std::vector<value_type>& rec_lat, size_type n, std::vector<value_type>& real_lat);

			/** vector storing the added particles */
			data_array_type m_particles;

			size_type m_bad_particles;
						
			friend class boost::serialization::access;
			
			template<class Archive>
			void load( Archive & ar, const unsigned int file_version)
			{
				size_type size;
				ar >> BOOST_SERIALIZATION_NVP(size);
				
				m_particles = data_array_type(size, SingleParticle2dx::DataStructures::Particle());
			
				for(int i=0; i<size; i++)
				{
					ar >> m_particles[i];
				}
			
				ar >> m_bad_particles;
			}
		
			template<class Archive>
			void save( Archive & ar, const unsigned int file_version) const
			{
				size_type size = m_particles.size();
				ar << BOOST_SERIALIZATION_NVP(size);
			
				for(size_type i=0; i<size; i++)
				{
					ar << m_particles[i];
				}
				
				ar << m_bad_particles;
			}
			
			template<class Archive>
			void serialize(Archive & ar, const unsigned int file_version)
			{
				boost::serialization::split_member(ar, *this, file_version);
			}

		};
		
	} /* DataStructures */
	
} /* SinglePartilce2dx */

#endif /* end of include guard: PARTICLE_CONTAINER_HPP */
