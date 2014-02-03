#ifndef UTILITYFUNCTIONS_HPP_HEFZ7QX2
#define UTILITYFUNCTIONS_HPP_HEFZ7QX2

namespace SingleParticle2dx
{
	namespace Utilities
	{
		class UtilityFunctions;
	}
}


#include <eigen3/Eigen/Dense>

#include "../Typedefs.hpp"
#include "../DataStructures.hpp"

namespace SingleParticle2dx
{
	namespace Utilities
	{
		class UtilityFunctions
		{
			
		public:
			
			typedef SingleParticle2dx::value_type value_type;
			typedef SingleParticle2dx::size_type size_type;
			typedef SingleParticle2dx::real_array2d_type real_array2d_type;
			
			/** Type of a generic rotation */
			typedef Eigen::AngleAxis<value_type> Rotation;
		
		
			/** Type of a 3d vector */
			typedef Eigen::Vector3f Vector3d;
			
			
			static value_type sinc(value_type x);
			
			
			/**
			 *  @brief      Calculates the rotation needed to go from (0,0,1) to the vector stored in the passed orientation 
			 *  @details    
			 *  @param[in]  o Target orientation
			 *  @return     Eigen rotation object that corresponds to the required roation
			 */
			static Eigen::Matrix3f determineRotation( SingleParticle2dx::DataStructures::Orientation& o );
			
			
			static void generateImageOutput(std::string image_name, std::string image_desc, std::string scriptname, bool important_flag = false, bool do_reload=true);
			static void generateVariableOutput(std::string variable_name, value_type value, std::string scriptname, bool do_reload=true);
			static void forceReload();
			
			
			static void writeConvergenceToFile(size_type it, value_type value, std::string filename);
			
			static void generate2dxOutput(std::string message, int verbosity = 3);
			
			static void setProgressBar(value_type value);
			
			static size_type getGoldStandardFSCRadius( std::pair<std::vector<value_type>, std::vector<value_type> >& fsc);
			
			static value_type calculateCorrespondingSigma(value_type t, value_type rmax);
			static value_type calculateNeighborWeight(value_type r, value_type sigma);
			
			static void reqularizeMatrix ( Eigen::MatrixXf& matrix );
			
			static void alignInplaneShift(std::vector<SingleParticle2dx::DataStructures::Particle*> parts, SingleParticle2dx::DataStructures::Projection2d& ref, bool useneighbors, value_type t, bool do_parallel=true);
			
			static void alignInplaneShift(SingleParticle2dx::DataStructures::ParticleContainer& cont, SingleParticle2dx::DataStructures::Projection2d& ref, bool useneighbors, value_type t, bool do_parallel=true);
			
			static void applyInplaneShift(SingleParticle2dx::DataStructures::ParticleContainer& cont);
				
			static value_type calculateDensity(std::vector<SingleParticle2dx::DataStructures::Particle*> parts, value_type width);
			
			static void calculateAverage(std::vector<SingleParticle2dx::DataStructures::Particle*> parts, real_array2d_type& ave, bool align);
			
			static void calculateAverage(SingleParticle2dx::DataStructures::ParticleContainer& cont, real_array2d_type& ave, bool align);
			
			static void disableParticles(std::vector<SingleParticle2dx::DataStructures::Particle*> parts);
			
			static void writeAveContainerStatToFile(SingleParticle2dx::DataStructures::ParticleContainer& cont, std::string filename);
			
			static void getContentOfFolder(std::string folder, std::vector<std::string>& folder_content, std::string ending = ".mrc");
			
			static void generateInitialModelForInitRef(SingleParticle2dx::DataStructures::Reconstruction3d& rec3d_ave);
			
			static void removeFileIfExists(std::string filename);
			static void removeFolderIfExists(std::string foldername);
			
			static void createContainerFolder(std::string foldername);
			
			static value_type calculateCC(SingleParticle2dx::DataStructures::Particle& part, SingleParticle2dx::DataStructures::Projection2d& proj);
			
		private:
			static void alignSingleParticle(SingleParticle2dx::DataStructures::Particle* part, SingleParticle2dx::DataStructures::Projection2d& ref, bool useneighbors, value_type t);
		};
		
	} /* Utilities */
	
} /* SingleParticle2dx */

#endif /* end of include guard: UTILITYFUNCTIONS_HPP_HEFZ7QX2 */
