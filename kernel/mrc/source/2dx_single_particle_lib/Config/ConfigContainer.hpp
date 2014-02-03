#ifndef SINGLE_PARTICLE_CONFIG_CONTAINER_HPP
#define SINGLE_PARTICLE_CONFIG_CONTAINER_HPP

#include <boost/scoped_ptr.hpp>
#include <boost/serialization/utility.hpp>
#include <boost/serialization/vector.hpp>

#include "../Typedefs.hpp"



namespace SingleParticle2dx
{
	/**
	 *  @brief     Global configuration container
	 *  @details   Global configuration data structure implemented by means of the 
	 *             singleton design pattern. The class contains a static pointer to itself
	 *             which is initialized during the get-instance call by means of the 
	 *             protected constructor. All later calls return the same pointer to the
	 *             same unique global object. 
	 *  @author    Sebastian Scherer
	 *  @version   0.2
	 *  @date      2012
	 *  @note      Thread safety is guarantied for OpenMP parallelization by means of a thread safe
	               call of the constructor
	 *  @copyright GNU Public License
	 *  @todo      Define content and write file read in from config-file
	 */
	class ConfigContainer
	{
	public:
		
		/** Type storing the size of the particle image */
		typedef SingleParticle2dx::size_type size_type;
		
		
		/** Type storing a value */
		typedef SingleParticle2dx::value_type value_type;
		
	
		/**
		 *  @brief      Get global instance function
		 *  @details    First the function checks whether the global object is already constructed. 
		                If so, the function returns a pointer to the global instance. Otherwise the 
		                object is constructed before the pointer gets returned.
		 *  @return     Pointer to the single instance
		 *  @note       thread-safe constructor call guarantied
		 */
		static ConfigContainer* Instance();
		
		
		/**
		 *  @brief      Destructor
		 *  @details    Deletes the global object
		 *  @post       All memory freed
		 */
		virtual ~ConfigContainer ();
		
		
		/**
		 *  @brief      Returns the size of a micrograph of a single particle
		 *  @details    Size of the underlying window of the original micrograph.
		 *  @note       Single particle windows are limited to squares right now.
		 *  @return     Size of a micrograph of a single particle
		 */
		size_type getParticleSize() const;
		
		
		/**
		 *  @brief      Returns the enlarging factor used for the projections 
		 *  @details    In order to avoid artefacts in the projections due to boundary effects and
		                the limited size of the 3d reconstruction the 3d reconstruction gets
		                enlarged before calculating the projections. This factor determines the size
		                of enlarged 3d reconstruction.
		 *  @return     Enlarging factor used for the projections
		 */
		size_type getProjectionFactor() const;
		
		
		/**
		 *  @brief      Returns the size of the considered CC-profile
		 *  @details    
		 *  @return     Size of the considered CC-profile
		 */
		size_type getCrossCorrelationWindowSize() const;
		
		
		void setCrossCorrelationWindowSize(const size_type rhs);
		
		
		/**
		 *  @brief      Returns used value of PI
		 *  @details    
		 *  @return     Value of PI
		 */
		value_type getPI() const;
		
		
		/**
		 *  @brief      Should the images be inverted
		 *  @details    
		 *  @return     image inverting flag
		 */	
		bool getInvertImage() const;
		
		
		/**
		 *  @brief      Gives the image directories
		 *  @details    
		 *  @return     vector of image directories name
		 */
		std::vector<std::string> getImageDirectories();
		
		
		/**
		 *  @brief      Gives the number of images selected in 2dx merge
		 *  @details    
		 *  @return     Number of images used for the calculation
		 */
		size_type getNumberOfImages();
		
	
		/**
		 *  @brief      Get the top-level project directory
		 *  @details    
		 *  @return     Full path to the top-level project directory
		 */
		std::string getProjectDirectory();
		
		
		/**
		 *  @brief      Get the minimal shell radius used for FSC calculation
		 *  @details    
		 *  @return     minimal shell radius used for FSC calculation
		 */
		value_type getFSCrmin() const;
		
		
		/**
		 *  @brief      Get the maximal shell radius used for FSC calculation
		 *  @details    
		 *  @return     maximal shell radius used for FSC calculation
		 */
		value_type getFSCrmax() const;
		
		
		/**
		 *  @brief      Get dr used for FSC calculation
		 *  @details    
		 *  @return     Get dr used for FSC calculation
		 */
		value_type getFSCdr() const;
		
		
		/**
		 *  @brief      Get highest Mag used in the dataset
		 *  @details    As not all images have to be recorded with the same Mag
		 *              one has to scale them. This parameter is used to determine
		 *              the required scaling parameter for each image
		 *  @return     highest Mag used in the dataset
		 *  @todo       Find the value automatically, not as input parameter
		 */
		value_type getMaxMag() const;
		
		
		/**
		 *  @brief      Set the paticle size
		 *  @warning    Should not be used. The function was implemented for the 
		 *              synthetic test data set and not for the refinment itself
		 */
		void setParticleSize(size_type i);
		
	 	value_type getCCThreshold() const;
	
		size_type getParticleMaskingMethod();
		
		void setParticleMaskingMethod(size_type rhs);
		
		size_type getProjectionMaskingMethod();
		
		void setProjectionMaskingMethod(size_type rhs);
		
		size_type getProjectionMethod();
		
		void setProjectionMethod(size_type rhs);
		
		size_type getRefinementMethod();
		
		void setRefinementMethod(size_type rhs);
		
		size_type getBackprojectionMethod();
		
		void setBackprojectionMethod(size_type rhs);
		
		size_type getReconstructionMaskingMethod();
		
		void setReconstructionMaskingMethod(size_type rhs);
		
		size_type getTrialAngleGenerator();
		
		void setTrialAngleGenerator(size_type rhs);
		
		bool getLocalCTFCorrection();
		
		void setLocalCTFCorrection(bool rhs);

		void setReconstructionMaskingRadius(value_type rhs);
		
		void setReconstructionMaskingdR(value_type rhs);
		
		void setReconstructionMaskingHeight(value_type rhs);
		
		void setReconstructionMaskingdH(value_type rhs);
		
		std::string getBin2dx();
		
		size_type getSymmetry() const;
		
		std::string getSymmetryString() const;
		
		size_type getMaxIteration() const;
		
		value_type getMinAngleChange() const;
		
		value_type getAmpCon() const;
		
		bool getEqualizeHistograms() const;
		
		size_type getParticleSelectionMethod() const;
		
		size_type getNumberOfParticles() const;
		
		value_type getPercentageOfParticles() const;
		
		value_type getMinAng1() const;
		
		value_type getMaxAng1() const;
		
		value_type getDAng1() const;
		
		value_type getMinAng2() const;
		
		value_type getMaxAng2() const;
		
		value_type getDAng2() const;
		
		value_type getMinAng3() const;
		
		value_type getMaxAng3() const;
		
		value_type getDAng3() const;
		
		void setMinAng3(value_type rhs);
		void setMaxAng3(value_type rhs);
		void setDAng3(value_type rhs);
		
		value_type getParticleMaskingRadius() const;
		value_type getParticleMaskingdR() const;
		
		void setParticleMaskingRadius(value_type rhs);
		void setParticleMaskingdR(value_type rhs);
		
		value_type getProjectionMaskingRadius() const;
		
		value_type getProjectionMaskingdR() const;
		
		void setProjectionMaskingRadius(value_type rhs);
		void setProjectionMaskingdR(value_type rhs);
		
		value_type getReconstructionMaskingRadius() const;
		
		value_type getReconstructionMaskingdR() const;
		
		value_type getReconstructionMaskingHeight() const;
		
		value_type getReconstructionMaskingdH() const;
		
		size_type getFinalReconstructionMaskingMethod() const;
		
		value_type getFinalReconstructionMaskingRadius() const;
		
		value_type getFinalReconstructionMaskingdR() const;
		
		value_type getFinalReconstructionMaskingHeight() const;
		
		value_type getFinalReconstructionMaskingdH() const;
		
		value_type getHPParticleRadius() const;
		
		value_type getHPParticleSigma() const;
		
		value_type getLPParticleRadius() const;
		
		value_type getLPParticleSigma() const;
		
		value_type getLPProjectionRadius() const;
		void setLPProjectionRadius(value_type rhs);
		
		value_type getLPProjectionSigma() const;
		
		value_type getLPReconstructionRadius() const;
		
		value_type getLPReconstructionSigma() const;
		
		size_type getMissingConeMethod() const;
		
		size_type getNNNumber() const;
		
		bool getSymInPlane() const;
		
		value_type getMaxInPlaneTltang() const;
		
		bool getIsGLPF() const;
		
		bool getCacheProjections() const;
		void setCacheProjections(bool rhs);
		
		bool getParallelProjection() const;
		void setParallelProjection(bool rhs);
		
		bool getOnlyStack() const;
		
		bool getSameInitModel() const;

		value_type getMinCCFreq() const;
		
		value_type getMaxCCFreq() const;
		
		value_type getCCFreqSigma() const;
		
		void setMaxCCFreq(value_type rhs);
		void setMinCCFreq(value_type rhs);
		void setCCFreqSigma(value_type rhs);
		
		std::string getScriptName() const;
		
		value_type getWeightS() const;
		
		value_type getWeightQ() const;
		
		value_type getWeightC() const;
		
		void setImageDirsManually(std::string rhs1, std::string rhs2);
		
		size_type getConsClickNumber() const;
		
		value_type getConsClickSize() const;
		
		bool getConsReuse() const;
		
		bool getConsContOnly() const;
		
		bool getConfOnly() const;
		
		value_type getEmanTrialMax() const;
		
		void setEmanTrialMax(value_type rhs);
		
		size_type getEmanTrialN() const;
		
		void setEmanTrialN(size_type rhs);
		
		std::string getContainerFilePath() const;
		std::string getContainerName() const;
		
		std::string getOrientFile() const;
		
		size_type getTreeLevel() const;
		
		bool getLastTreeLevel() const;
		
		size_type getDoublingRateTree() const;
		
		value_type getShakingRateTree() const;
		
		size_type getNumberOfLevels() const;
		
		value_type getCCDiscard() const;
		
		bool getKeepAll() const;
		
		size_type getAveWeightMode() const;
		
		value_type getMinDensity() const;
		
		value_type getMaxConeTilt() const;
		void setMaxConeTilt(value_type rhs);
		
		value_type getDConeTilt() const;
		
		bool getRestartRef() const;
		bool getDoBinning() const;
		
		size_type getRefMode() const;
		
		bool getDoDoublePick() const;
		
		bool getStoreBigCont() const;
		
		bool getDoCudaFFT() const;
		void setDoCudaFFT(bool rhs);
		
		value_type getMinStdPicking() const;
		
		value_type getConsT() const;
		value_type getModelKeep() const;
		
		size_type getInitOffsetX() const;
		size_type getInitOffsetY() const;
		size_type getInitOffsetZ() const;
		
		bool getShowSights() const;
		
		value_type getMaskEllipseA() const;
		value_type getMaskEllipseB() const;
		value_type getMaskEllipsePhi() const;
		
		value_type getNumberOfClasses() const;

		bool getMRAUseNN() const;
		void setMRAUseNN(bool rhs);
	
	protected:
	
		/**
		 *  @brief      Constructor 
		 *  @details    Reads the all the configuration values from the 2dx config file
		 *  @post       Global config container initialized
		 *  @note       Constructor is protected and thus can not be called from outside the class
		 *  @todo       Implement read in from file (2dx.config)
		 */
		ConfigContainer();
		
		void setup();
		
	private:
		
		friend class boost::serialization::access;
		   
		template<class Archive>
		void serialize(Archive & ar, const unsigned int version)
		{
			ar & m_instance;
			
			ar & m_is_valid;
			ar & m_pi;
			ar & m_image_dirs;
			ar & m_fsc_rmin;
			ar & m_fsc_rmax;
			ar & m_fsc_dr;
			ar & m_projection_factor;
			ar & m_project_dir;
			ar & m_cc_threshold;
			ar & m_bin_2dx;
			ar & m_particle_size;
			ar & m_invert_image;
			ar & m_maxmag;
			ar & m_symmetry;
			ar & m_eman_symmetry_string;
			ar & m_max_iteration;
			ar & m_min_angular_change;
			ar & m_refinement_method;
			ar & m_cc_window_size;
			ar & m_projection_method;
			ar & m_backprojection_method;
			ar & m_do_local_ctfcorrection;
			ar & m_ampcon;
			ar & m_equalize_histograms;
			ar & m_particle_selection_method;
			ar & m_number_of_particles;
			ar & m_particle_percentage;
			ar & m_trialangle_generator;
			ar & m_ang1_min;
			ar & m_ang1_max;
			ar & m_ang1_dx;
			ar & m_ang2_min;
			ar & m_ang2_max;
			ar & m_ang2_dx;
			ar & m_ang3_min;
			ar & m_ang3_max;
			ar & m_ang3_dx;
			ar & m_particle_masking;
			ar & m_particle_masking_radius;
			ar & m_particle_masking_dr;
			ar & m_projection_masking;
			ar & m_projection_masking_radius;
			ar & m_projection_masking_dr;
			ar & m_reconstruction_masking;
			ar & m_reconstruction_masking_radius;
			ar & m_reconstruction_masking_dr;
			ar & m_reconstruction_masking_height;
			ar & m_reconstruction_masking_dh;
			ar & m_final_reconstruction_masking;
			ar & m_final_reconstruction_masking_radius;
			ar & m_final_reconstruction_masking_dr;
			ar & m_final_reconstruction_masking_height;
			ar & m_final_reconstruction_masking_dh;
			ar & m_hp_particle_radius;
			ar & m_hp_particle_sigma;
			ar & m_lp_particle_radius;
			ar & m_lp_particle_sigma;
			ar & m_lp_projection_radius;
			ar & m_lp_projection_sigma;
			ar & m_lp_volume_radius;
			ar & m_lp_volume_sigma;
			ar & m_sym_inplane;
			ar & m_max_inplane_sym_tltang;
			ar & m_missing_cone_method;
			ar & m_is_glpf;
			ar & m_cache_projections;
			ar & m_parallel_projection;
			ar & m_only_stack;
			ar & m_same_init_model;
			ar & m_min_cc_freq;
			ar & m_max_cc_freq;
			ar & m_sigma_cc_freq;
			ar & m_script_name;
			ar & m_weight_s;
			ar & m_weight_q;
			ar & m_weight_c;
			ar & m_cons_click_number;
			ar & m_cons_click_size;
			ar & m_cons_reuse;
			ar & m_cons_cont_only;
			ar & m_conf_only;
			ar & m_emantrial_max;
			ar & m_emantrial_n;
			ar & m_nn_number;
			ar & m_container_folder;
			ar & m_orient_file;
			ar & m_tree_level;
			ar & m_last_tree_level;
			ar & m_doubling_rate_tree;
			ar & m_shaking_rate_tree;
			ar & m_number_of_levels;
			ar & m_cc_discard;
			ar & m_keep_all;
			ar & m_average_weight;
			ar & m_crit_density_percent;
			ar & m_max_cone_tilt;
			ar & m_d_cone_tilt;
			ar & m_restart_ref;
			ar & m_binn;
			ar & m_ref_mode;
			ar & m_double_pick;
			ar & m_store_big_cont;
			ar & m_cuda_fft;
			ar & m_minstd_pick;
			ar & m_cons_t;
			ar & m_model_keep;
			ar & m_init_offset_x;
			ar & m_init_offset_y;
			ar & m_init_offset_z;
			ar & m_show_sights;
			
			ar & m_mask_ellipse_a;
			ar & m_mask_ellipse_b;
			ar & m_mask_ellipse_phi;
			
			ar & m_num_classes;
			ar & m_mra_use_nn;
		}
		

		/** Pointer to the global instance of this class */
		static boost::scoped_ptr<ConfigContainer> m_instance;
		
		// no gui parameters
		static bool m_is_valid;
		value_type m_pi;
		std::vector<std::string> m_image_dirs;
		value_type m_fsc_rmin;
		value_type m_fsc_rmax;
		value_type m_fsc_dr;
		size_type m_projection_factor;
		std::string m_project_dir;
		value_type m_cc_threshold;
		std::string m_bin_2dx;
		
		// general parameter section
		size_type m_particle_size;
		bool m_invert_image;
		value_type m_maxmag;
		size_type m_symmetry;
		std::string m_eman_symmetry_string;
		
		// algorithmic section
		size_type m_max_iteration;
		value_type m_min_angular_change;
		size_type m_refinement_method;
		size_type m_cc_window_size;
		size_type m_projection_method;
		size_type m_backprojection_method;
		bool m_do_local_ctfcorrection;
		value_type m_ampcon;
		bool m_equalize_histograms;
		size_type m_particle_selection_method;
		size_type m_number_of_particles;
		value_type m_particle_percentage;
		size_type m_trialangle_generator;
		value_type m_ang1_min;
		value_type m_ang1_max;
		value_type m_ang1_dx;
		value_type m_ang2_min;
		value_type m_ang2_max;
		value_type m_ang2_dx;
		value_type m_ang3_min;
		value_type m_ang3_max;
		value_type m_ang3_dx;
		
		value_type m_weight_s;
		value_type m_weight_q;
		value_type m_weight_c;
		
		// masking section
		size_type m_particle_masking;
		value_type m_particle_masking_radius;
		value_type m_particle_masking_dr;
		size_type m_projection_masking;
		value_type m_projection_masking_radius;
		value_type m_projection_masking_dr;
		size_type m_reconstruction_masking;
		value_type m_reconstruction_masking_radius;
		value_type m_reconstruction_masking_dr;
		value_type m_reconstruction_masking_height;
		value_type m_reconstruction_masking_dh;
		size_type  m_final_reconstruction_masking;
		value_type m_final_reconstruction_masking_radius;
		value_type m_final_reconstruction_masking_dr;
		value_type m_final_reconstruction_masking_height;
		value_type m_final_reconstruction_masking_dh;
		size_type m_missing_cone_method;
		
		// filter section
		value_type m_hp_particle_radius;
		value_type m_hp_particle_sigma;
		value_type m_lp_particle_radius;
		value_type m_lp_particle_sigma;
		value_type m_lp_projection_radius;
		value_type m_lp_projection_sigma;
		value_type m_lp_volume_radius;
		value_type m_lp_volume_sigma;
		
		bool m_sym_inplane;
		value_type m_max_inplane_sym_tltang;
		bool m_is_glpf;
		bool m_cache_projections;		
		bool m_parallel_projection;
		
		bool m_only_stack;
		bool m_same_init_model;

		value_type m_min_cc_freq;
		value_type m_max_cc_freq;
		value_type m_sigma_cc_freq;
		
		std::string m_script_name;
		
		size_type m_cons_click_number;
		value_type m_cons_click_size;
		bool m_cons_reuse;
		bool m_cons_cont_only;
		bool m_conf_only;
		
		value_type m_emantrial_max;
		size_type m_emantrial_n;
		
		size_type m_tree_level;
		bool m_last_tree_level;
		
		size_type m_nn_number;
		
		value_type m_doubling_rate_tree;
		value_type m_shaking_rate_tree;
		value_type m_cc_discard;
		
		size_type m_number_of_levels;
		
		std::string m_container_folder;
		std::string m_orient_file;
		
		bool m_keep_all;
		size_type m_average_weight;
		value_type m_crit_density_percent;
		
		value_type m_max_cone_tilt;
		value_type m_d_cone_tilt;
		
		bool m_restart_ref;
		bool m_binn;
		
		size_type m_ref_mode;
		
		bool m_double_pick;
		
		bool m_store_big_cont;
		
		bool m_cuda_fft;
		
		value_type m_minstd_pick;
		
		value_type m_cons_t;
		value_type m_model_keep;
		
		size_type m_init_offset_x;
		size_type m_init_offset_y;
		size_type m_init_offset_z;
		
		bool m_show_sights;
		
		value_type m_mask_ellipse_a;
		value_type m_mask_ellipse_b;
		value_type m_mask_ellipse_phi;
		
		size_type m_num_classes;
		
		bool m_mra_use_nn;
		
	};
	
} /* SingleParticle2dx */

#endif /* end of include guard: SINGLE_PARTICLE_CONFIG_CONTAINER_HPP */
