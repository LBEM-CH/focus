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

#include <boost/filesystem.hpp>

#include "ConfigContainer.hpp"

#include "../Utilities/MergeVariableReader.hpp"
#include "../Utilities/StringFunctions.hpp"


boost::scoped_ptr<SingleParticle2dx::ConfigContainer> SingleParticle2dx::ConfigContainer::m_instance( NULL );


bool SingleParticle2dx::ConfigContainer::m_is_valid = false;


SingleParticle2dx::ConfigContainer* SingleParticle2dx::ConfigContainer::Instance()
{
	if (m_instance == NULL)
	{
		// thread-safe call if the constructor
		#pragma omp critical (read_config)
		{
			m_instance.reset(new SingleParticle2dx::ConfigContainer);
		}
		return m_instance.get();
	}
	if (m_is_valid)
	{
		return m_instance.get();
	}
	else
	{
		// FIXME
		return m_instance.get();
		//std::cerr << "config went wrong" << std::endl;
		//throw std::runtime_error("Bad operation");
	}
}


SingleParticle2dx::ConfigContainer::~ConfigContainer ()
{}

	
SingleParticle2dx::ConfigContainer::ConfigContainer()
{

	if (boost::filesystem::exists("mergevars_written_to_file.txt"))
	{
		std::cout << "::merge variables are there" << std::endl;
		setup();
	}
	else
	{
		std::cout << "::merge variables are NOT there" << std::endl;
		m_is_valid = false;
	}	
}


void SingleParticle2dx::ConfigContainer::setup()
{
	SingleParticle2dx::Utilities::MergeVariableReader config_merge("mergevars_written_to_file.txt");
	
	std::vector<std::string> image_dirs_in = config_merge.getStringElement("dirlist");
	for(size_type i=0; i<static_cast<int>(image_dirs_in.size()); i++)
	{
		m_image_dirs.push_back(image_dirs_in[i]);
	}

	std::vector<std::string> split_vector;
	SingleParticle2dx::Utilities::StringFunctions::splitString( split_vector, config_merge.getStringElement("dirstack")[0], std::string("/") );

	m_project_dir = "";
	for(size_type i=0; i<static_cast<int>(split_vector.size()-1); i++)
	{
		m_project_dir += split_vector[i];
		m_project_dir += "/";
	}

	m_particle_size = config_merge.getFloatElement("particle_size_sp2dx")[0];
	
	config_merge.printConfigFile();
			
	m_bin_2dx = config_merge.getStringElement("bin_2dx")[0];
	
	m_projection_factor = 3;
	m_pi = 4 * atan(1);

	m_fsc_rmin = 1;
	m_fsc_rmax = m_particle_size/2;
	m_fsc_dr = 1;

	m_cc_threshold = 2.0;
	
	m_is_valid = true;
	
	m_invert_image = static_cast<bool>(config_merge.getFloatElement("invert_sp2dx")[0]);
	m_maxmag = config_merge.getFloatElement("maximum_mag_sp2dx")[0];
	m_symmetry = config_merge.getFloatElement("symmetry_sp2dx")[0] + 1;
	m_eman_symmetry_string = "c" + SingleParticle2dx::Utilities::StringFunctions::TtoString(m_symmetry);
	m_max_iteration = config_merge.getFloatElement("max_iteration_incharge")[0];
	m_min_angular_change = config_merge.getFloatElement("min_angular_change_incharge")[0];
	m_refinement_method = config_merge.getFloatElement("refinement_method_sp2dx")[0];
	m_cc_window_size = config_merge.getFloatElement("cc_window_size_incharge")[0];
	m_projection_method = config_merge.getFloatElement("projection_method_sp2dx")[0];
	m_backprojection_method = config_merge.getFloatElement("back_projection_method_sp2dx")[0];
	m_do_local_ctfcorrection = static_cast<bool>(config_merge.getFloatElement("local_ctf_sp2dx")[0]);
	m_ampcon = config_merge.getFloatElement("local_ctf_ampcon_sp2dx")[0];
	m_equalize_histograms = static_cast<bool>(config_merge.getFloatElement("hist_equal_sp2dx")[0]);
	m_particle_selection_method = config_merge.getFloatElement("particle_selection_method_incharge")[0];
	m_number_of_particles = config_merge.getFloatElement("number_particles_incharge")[0];
	m_particle_percentage = config_merge.getFloatElement("percent_particles_incharge")[0];
	m_trialangle_generator = config_merge.getFloatElement("angle_sampling_method_sp2dx")[0];
	m_ang1_min = config_merge.getFloatElement("sampling_ang1_sp2dx")[0];
	m_ang1_max = config_merge.getFloatElement("sampling_ang1_sp2dx")[1];
	m_ang1_dx = config_merge.getFloatElement("sampling_ang1_sp2dx")[2];
	m_ang2_min = config_merge.getFloatElement("sampling_ang2_sp2dx")[0];
	m_ang2_max = config_merge.getFloatElement("sampling_ang2_sp2dx")[1];
	m_ang2_dx = config_merge.getFloatElement("sampling_ang2_sp2dx")[2];
	m_ang3_min = config_merge.getFloatElement("sampling_ang3_sp2dx")[0];
	m_ang3_max = config_merge.getFloatElement("sampling_ang3_sp2dx")[1];
	m_ang3_dx = config_merge.getFloatElement("sampling_ang3_sp2dx")[2];
	m_particle_masking = config_merge.getFloatElement("particle_masking_sp2dx")[0];
	m_particle_masking_radius = config_merge.getFloatElement("particle_masking_params_sp2dx")[0];
	m_particle_masking_dr = config_merge.getFloatElement("particle_masking_params_sp2dx")[1];
	m_projection_masking = config_merge.getFloatElement("projection_masking_sp2dx")[0];
	m_projection_masking_radius = config_merge.getFloatElement("projection_masking_params_sp2dx")[0];
	m_projection_masking_dr = config_merge.getFloatElement("projection_masking_params_sp2dx")[1];
	m_reconstruction_masking = config_merge.getFloatElement("reconstruction_masking_sp2dx")[0];
	m_reconstruction_masking_radius = config_merge.getFloatElement("reconstruction_masking_r_params_sp2dx")[0];
	m_reconstruction_masking_dr = config_merge.getFloatElement("reconstruction_masking_r_params_sp2dx")[1];
	m_reconstruction_masking_height = config_merge.getFloatElement("reconstruction_masking_h_params_sp2dx")[0];
	m_reconstruction_masking_dh = config_merge.getFloatElement("reconstruction_masking_h_params_sp2dx")[1];
	m_final_reconstruction_masking = config_merge.getFloatElement("reconstruction_final_masking_sp2dx")[0];
	m_final_reconstruction_masking_radius = config_merge.getFloatElement("reconstruction_final_masking_r_params_sp2dx")[0];
	m_final_reconstruction_masking_dr = config_merge.getFloatElement("reconstruction_final_masking_r_params_sp2dx")[1];
	m_final_reconstruction_masking_height = config_merge.getFloatElement("reconstruction_final_masking_h_params_sp2dx")[0];
	m_final_reconstruction_masking_dh = config_merge.getFloatElement("reconstruction_final_masking_h_params_sp2dx")[1];
	m_hp_particle_radius = config_merge.getFloatElement("hp_particle_filter_sp2dx")[0];
	m_hp_particle_sigma = config_merge.getFloatElement("hp_particle_filter_sp2dx")[1];
	m_lp_particle_radius = config_merge.getFloatElement("lp_particle_filter_sp2dx")[0];
	m_lp_particle_sigma = config_merge.getFloatElement("lp_particle_filter_sp2dx")[1];
	m_lp_projection_radius = config_merge.getFloatElement("lp_projection_filter_sp2dx")[0];
	m_lp_projection_sigma = config_merge.getFloatElement("lp_projection_filter_sp2dx")[1];
	m_lp_volume_radius = config_merge.getFloatElement("lp_volume_filter_incharge")[0];
	m_lp_volume_sigma = config_merge.getFloatElement("lp_volume_filter_incharge")[1];
	m_missing_cone_method = config_merge.getFloatElement("missing_cone_sp2dx")[0];
	
	m_sym_inplane = static_cast<bool>(config_merge.getFloatElement("sym_inplane_sp2dx")[0]);
	m_max_inplane_sym_tltang = config_merge.getFloatElement("max_inplane_tltang_sp2dx")[0];

	m_min_cc_freq = config_merge.getFloatElement("cc_freq_range_incharge")[0];
	m_max_cc_freq = config_merge.getFloatElement("cc_freq_range_incharge")[1];
	m_sigma_cc_freq = config_merge.getFloatElement("cc_freq_range_sp2dx_incharge")[2];

	m_is_glpf = static_cast<bool>(config_merge.getFloatElement("is_glpf_sp2dx")[0]);
	m_cache_projections = static_cast<bool>(config_merge.getFloatElement("cache_projections_sp2dx")[0]);
	m_parallel_projection = static_cast<bool>(config_merge.getFloatElement("parallel_projection_sp2dx")[0]);
	
	m_only_stack = static_cast<bool>(config_merge.getFloatElement("only_stack_sp2dx")[0]);
	m_same_init_model = static_cast<bool>(config_merge.getFloatElement("same_init_model_sp2dx")[0]);
	
	m_weight_s = config_merge.getFloatElement("weight_params_sp2dx")[0];
	m_weight_q = config_merge.getFloatElement("weight_params_sp2dx")[1];
	m_weight_c = config_merge.getFloatElement("weight_params_sp2dx")[2];
	
	m_script_name = config_merge.getStringElement("scriptname")[0];
	
	m_container_folder = config_merge.getStringElement("cont_folder_sp2dx")[0];
	m_orient_file = config_merge.getStringElement("orientation_file_sp2dx")[0];
	
	m_cons_click_number = config_merge.getFloatElement("cons_click_number_sp2dx")[0];
	m_cons_click_size = config_merge.getFloatElement("cons_click_size_sp2dx")[0];
	m_cons_reuse = static_cast<bool>(config_merge.getFloatElement("cons_reuse_sp2dx")[0]);
	m_cons_cont_only = static_cast<bool>(config_merge.getFloatElement("cons_cont_only_sp2dx")[0]);
	m_conf_only = static_cast<bool>(config_merge.getFloatElement("conf_only_sp2dx")[0]);
	
	m_tree_level = config_merge.getFloatElement("tree_level_true_sp2dx")[0];
	m_last_tree_level = static_cast<bool>(config_merge.getFloatElement("last_tree_level_sp2dx")[0]);
	
	m_nn_number = static_cast<int>(config_merge.getFloatElement("nn_number_incharge")[0]);

	m_emantrial_n = config_merge.getFloatElement("emantrial_n_incharge")[0];
	m_emantrial_max = config_merge.getFloatElement("emantrial_max_incharge")[0];
	
	m_doubling_rate_tree = config_merge.getFloatElement("doubling_rate_tree_sp2dx")[0];
	m_shaking_rate_tree = config_merge.getFloatElement("shaking_rate_tree_sp2dx")[0];
	
	m_cc_discard = config_merge.getFloatElement("cc_discard_incharge")[0];
	
	m_number_of_levels = config_merge.getFloatElement("number_of_levels_incharge")[0];
	
	m_keep_all = static_cast<bool>(config_merge.getFloatElement("keep_all_sp2dx")[0]);
	m_average_weight = static_cast<size_type>(config_merge.getFloatElement("ave_weight_sp2dx")[0]);
	m_crit_density_percent = config_merge.getFloatElement("min_density_percent_sp2dx")[0];
	
	m_max_cone_tilt = config_merge.getFloatElement("cone_parameters_sp2dx")[0];
	m_d_cone_tilt = config_merge.getFloatElement("cone_parameters_sp2dx")[1];
	
	m_restart_ref = static_cast<bool>(config_merge.getFloatElement("restart_ref_sp2dx")[0]);
	m_binn = static_cast<bool>(config_merge.getFloatElement("binn_sp2dx")[0]);
	
	m_ref_mode = static_cast<size_type>(config_merge.getFloatElement("which_ref_sp2dx")[0]);
	m_double_pick = static_cast<bool>(config_merge.getFloatElement("double_pick_sp2dx")[0]);
	
	m_store_big_cont = static_cast<bool>(config_merge.getFloatElement("store_big_cont_sp2dx")[0]);
	m_minstd_pick = config_merge.getFloatElement("minstd_pick_sp2dx")[0];
	
	m_cons_t = config_merge.getFloatElement("cons_t_sp2dx")[0];
	m_model_keep = config_merge.getFloatElement("model_keep_sp2dx")[0];
	
	m_init_offset_x = static_cast<size_type>(config_merge.getFloatElement("offsets_sp2dx")[0]);
	m_init_offset_y = static_cast<size_type>(config_merge.getFloatElement("offsets_sp2dx")[1]);
	m_init_offset_z = static_cast<size_type>(config_merge.getFloatElement("offsets_sp2dx")[2]);
	
	m_show_sights = static_cast<bool>(config_merge.getFloatElement("show_sights_sp2dx")[0]);
	
	m_mask_ellipse_a = config_merge.getFloatElement("reconstruction_masking_ellipse_params_sp2dx")[0];
	m_mask_ellipse_b = config_merge.getFloatElement("reconstruction_masking_ellipse_params_sp2dx")[1];
	m_mask_ellipse_phi = config_merge.getFloatElement("reconstruction_masking_ellipse_params_sp2dx")[2] * m_pi/180.0;
	
	m_num_classes = config_merge.getFloatElement("num_classes_sp2dx")[0];
	
	m_mra_use_nn = static_cast<bool>(config_merge.getFloatElement("mra_use_nn_sp2dx")[0]);
}


std::string SingleParticle2dx::ConfigContainer::getContainerFilePath() const
{
	return m_container_folder;
}


std::string SingleParticle2dx::ConfigContainer::getOrientFile() const
{
	return m_orient_file;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getFSCrmin() const
{
	return m_fsc_rmin;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getMaxMag() const
{
	return m_maxmag;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getFSCrmax() const
{
	return m_fsc_rmax;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getFSCdr() const
{
	return m_fsc_dr;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getParticleSize() const
{
	return m_particle_size;
}


bool SingleParticle2dx::ConfigContainer::getInvertImage() const
{
	return m_invert_image;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getProjectionFactor() const
{
	return m_projection_factor;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getCrossCorrelationWindowSize() const
{
	return m_cc_window_size;
}


void SingleParticle2dx::ConfigContainer::setCrossCorrelationWindowSize(const size_type rhs)
{
	m_cc_window_size = rhs;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getPI() const
{
	return m_pi;
}


std::vector<std::string> SingleParticle2dx::ConfigContainer::getImageDirectories()
{
	return m_image_dirs;
}


void SingleParticle2dx::ConfigContainer::setParticleSize (size_type i)
{
	m_particle_size = i;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getNumberOfImages()
{
	return m_image_dirs.size();
}


std::string SingleParticle2dx::ConfigContainer::getProjectDirectory()
{
	return m_project_dir;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getCCThreshold() const
{
	return m_cc_threshold;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getParticleMaskingMethod()
{
	return m_particle_masking;
}


void SingleParticle2dx::ConfigContainer::setParticleMaskingMethod(size_type rhs)
{
	m_particle_masking = rhs;
}


void SingleParticle2dx::ConfigContainer::setParticleMaskingRadius(value_type rhs)
{
	m_particle_masking_radius = rhs;
}


void SingleParticle2dx::ConfigContainer::setParticleMaskingdR(value_type rhs)
{
	m_particle_masking_dr = rhs;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getProjectionMaskingMethod()
{
	return m_projection_masking;
}


void SingleParticle2dx::ConfigContainer::setProjectionMaskingMethod(size_type rhs)
{
	m_projection_masking = rhs;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getProjectionMethod()
{
	return m_projection_method;
}


void SingleParticle2dx::ConfigContainer::setProjectionMethod(size_type rhs)
{
	m_projection_method = rhs;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getRefinementMethod()
{
	return m_refinement_method;
}


void SingleParticle2dx::ConfigContainer::setRefinementMethod(size_type rhs)
{
	m_refinement_method = rhs;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getBackprojectionMethod()
{
	return m_backprojection_method;
}


void SingleParticle2dx::ConfigContainer::setBackprojectionMethod(size_type rhs)
{
	m_backprojection_method = rhs;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getReconstructionMaskingMethod()
{
	return m_reconstruction_masking;
}


void SingleParticle2dx::ConfigContainer::setReconstructionMaskingMethod(size_type rhs)
{
	m_reconstruction_masking = rhs;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getTrialAngleGenerator()
{
	return m_trialangle_generator;
}


void SingleParticle2dx::ConfigContainer::setTrialAngleGenerator(size_type rhs)
{
	m_trialangle_generator = rhs;
}


bool SingleParticle2dx::ConfigContainer::getLocalCTFCorrection()
{
	return m_do_local_ctfcorrection;
}


void SingleParticle2dx::ConfigContainer::setLocalCTFCorrection(bool rhs)
{
	m_do_local_ctfcorrection = rhs;
}


std::string SingleParticle2dx::ConfigContainer::getBin2dx()
{
	return m_bin_2dx;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getSymmetry() const
{
	return m_symmetry;
}


std::string SingleParticle2dx::ConfigContainer::getSymmetryString() const
{
	return m_eman_symmetry_string;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getMaxIteration() const
{
	return m_max_iteration;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getMinAngleChange() const
{
	return m_min_angular_change;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getAmpCon() const
{
	return m_ampcon;
}


bool SingleParticle2dx::ConfigContainer::getEqualizeHistograms() const
{
	return m_equalize_histograms;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getParticleSelectionMethod() const
{
	return m_particle_selection_method;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getNumberOfParticles() const
{
	return m_number_of_particles;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getPercentageOfParticles() const
{
	return m_particle_percentage;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getMinAng1() const
{
	return m_ang1_min;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getMaxAng1() const
{
	return m_ang1_max;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getDAng1() const
{
	return m_ang1_dx;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getMinAng2() const
{
	return m_ang2_min;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getMaxAng2() const
{
	return m_ang2_max;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getDAng2() const
{
	return m_ang2_dx;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getMinAng3() const
{
	return m_ang3_min;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getMaxAng3() const
{
	return m_ang3_max;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getDAng3() const
{
	return m_ang3_dx;
}


void SingleParticle2dx::ConfigContainer::setMinAng3(value_type rhs)
{
	m_ang3_min = rhs;
}


void SingleParticle2dx::ConfigContainer::setMaxAng3(value_type rhs)
{
	m_ang3_max = rhs;
}


void SingleParticle2dx::ConfigContainer::setDAng3(value_type rhs)
{
	m_ang3_dx = rhs;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getParticleMaskingRadius() const
{
	return m_particle_masking_radius;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getParticleMaskingdR() const
{
	return m_particle_masking_dr;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getProjectionMaskingRadius() const
{
	return m_projection_masking_radius;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getProjectionMaskingdR() const
{
	return m_projection_masking_dr;
}


void SingleParticle2dx::ConfigContainer::setProjectionMaskingRadius(value_type rhs)
{
	m_projection_masking_radius = rhs;
}


void SingleParticle2dx::ConfigContainer::setProjectionMaskingdR(value_type rhs)
{
	m_projection_masking_dr = rhs;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getReconstructionMaskingRadius() const
{
	return m_reconstruction_masking_radius;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getReconstructionMaskingdR() const
{
	return m_reconstruction_masking_dr;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getReconstructionMaskingHeight() const
{
	return m_reconstruction_masking_height;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getReconstructionMaskingdH() const
{
	return m_reconstruction_masking_dh;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getFinalReconstructionMaskingMethod() const
{
	return m_final_reconstruction_masking;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getFinalReconstructionMaskingRadius() const
{
	return m_final_reconstruction_masking_radius;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getFinalReconstructionMaskingdR() const
{
	return m_final_reconstruction_masking_dr;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getFinalReconstructionMaskingHeight() const
{
	return m_final_reconstruction_masking_height;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getFinalReconstructionMaskingdH() const
{
	return m_final_reconstruction_masking_dh;
}


std::string SingleParticle2dx::ConfigContainer::getContainerName() const
{
	std::vector<std::string> split_vector;
	SingleParticle2dx::Utilities::StringFunctions::splitString( split_vector, m_container_folder, std::string("/") );
	std::string result = split_vector.back();
	return result;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getHPParticleRadius() const
{
	return m_hp_particle_radius;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getHPParticleSigma() const
{
	return m_hp_particle_sigma;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getLPParticleRadius() const
{
	return m_lp_particle_radius;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getLPParticleSigma() const
{
	return m_lp_particle_sigma;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getLPProjectionRadius() const
{
	return m_lp_projection_radius;
}


void SingleParticle2dx::ConfigContainer::setLPProjectionRadius(value_type rhs)
{
	m_lp_projection_radius = rhs;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getLPProjectionSigma() const
{
	return m_lp_projection_sigma;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getLPReconstructionRadius() const
{
	return m_lp_volume_radius;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getLPReconstructionSigma() const
{
	return m_lp_volume_sigma;
}


bool SingleParticle2dx::ConfigContainer::getSymInPlane() const
{
	return m_sym_inplane;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getMaxInPlaneTltang() const
{
	return m_max_inplane_sym_tltang;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getMissingConeMethod() const
{
	return m_missing_cone_method;
}


bool SingleParticle2dx::ConfigContainer::getIsGLPF() const
{
	return m_is_glpf;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getMinCCFreq() const
{
	return m_min_cc_freq;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getMaxCCFreq() const
{
	return m_max_cc_freq;
}


void SingleParticle2dx::ConfigContainer::setMaxCCFreq(value_type rhs)
{
	m_max_cc_freq = rhs;
}


void SingleParticle2dx::ConfigContainer::setMinCCFreq(value_type rhs)
{
	m_min_cc_freq = rhs;
}


void SingleParticle2dx::ConfigContainer::setCCFreqSigma(value_type rhs)
{
	m_sigma_cc_freq = rhs;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getCCFreqSigma() const
{
	return m_sigma_cc_freq;
}


bool SingleParticle2dx::ConfigContainer::getCacheProjections() const
{
	return m_cache_projections;
}


void SingleParticle2dx::ConfigContainer::setCacheProjections(bool rhs)
{
	m_cache_projections = rhs;
}

void SingleParticle2dx::ConfigContainer::setReconstructionMaskingRadius(value_type rhs)
{
	m_reconstruction_masking_radius = rhs;
}


void SingleParticle2dx::ConfigContainer::setReconstructionMaskingdR(value_type rhs)
{
	m_reconstruction_masking_dr = rhs;
}


void SingleParticle2dx::ConfigContainer::setReconstructionMaskingHeight(value_type rhs)
{
	m_reconstruction_masking_height = rhs;
}


void SingleParticle2dx::ConfigContainer::setReconstructionMaskingdH(value_type rhs)
{
	m_reconstruction_masking_dh = rhs;
}


bool SingleParticle2dx::ConfigContainer::getParallelProjection() const
{
	return m_parallel_projection;	
}


void SingleParticle2dx::ConfigContainer::setParallelProjection(bool rhs)
{
	m_parallel_projection = rhs;	
}


std::string SingleParticle2dx::ConfigContainer::getScriptName() const
{
	return m_script_name;
}


bool SingleParticle2dx::ConfigContainer::getOnlyStack() const
{
	return m_only_stack;
}


bool SingleParticle2dx::ConfigContainer::getSameInitModel() const
{
	return m_same_init_model;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getWeightS() const
{
	return m_weight_s;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getWeightQ() const
{
	return m_weight_q;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getWeightC() const
{
	return m_weight_c;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getConsClickNumber() const
{
	return m_cons_click_number;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getConsClickSize() const
{
	return m_cons_click_size;
}


bool SingleParticle2dx::ConfigContainer::getConsReuse() const
{
	return m_cons_reuse;
}


bool SingleParticle2dx::ConfigContainer::getConsContOnly() const
{
	return m_cons_cont_only;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getEmanTrialMax() const
{
	return m_emantrial_max;
}


void SingleParticle2dx::ConfigContainer::setEmanTrialMax(value_type rhs)
{
	m_emantrial_max = rhs;
}


bool SingleParticle2dx::ConfigContainer::getConfOnly() const
{
	return m_conf_only;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getEmanTrialN() const
{
	return m_emantrial_n;
}


void SingleParticle2dx::ConfigContainer::setEmanTrialN(size_type rhs)
{
	m_emantrial_n = rhs;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getTreeLevel() const
{
	return m_tree_level;
}


bool SingleParticle2dx::ConfigContainer::getLastTreeLevel() const
{
	return m_last_tree_level;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getNNNumber() const
{
	return m_nn_number;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getDoublingRateTree() const
{
	return m_doubling_rate_tree;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getShakingRateTree() const
{
	return m_shaking_rate_tree;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getNumberOfLevels() const
{
	return m_number_of_levels;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getCCDiscard() const
{
	return m_cc_discard;
}


bool SingleParticle2dx::ConfigContainer::getKeepAll() const
{
	return m_keep_all;
}


SingleParticle2dx::ConfigContainer::size_type  SingleParticle2dx::ConfigContainer::getAveWeightMode() const
 {
	 return m_average_weight;
 }
 
bool SingleParticle2dx::ConfigContainer::getStoreBigCont() const
{
	return m_store_big_cont;
}
 
 
SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getMinDensity() const
{
	return m_crit_density_percent;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getMaxConeTilt() const
{
	return m_max_cone_tilt;
}


void SingleParticle2dx::ConfigContainer::setMaxConeTilt(value_type rhs)
{
	m_max_cone_tilt = rhs;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getDConeTilt() const
{
	return m_d_cone_tilt;
}


bool SingleParticle2dx::ConfigContainer::getRestartRef() const
{
	return m_restart_ref;
}


bool SingleParticle2dx::ConfigContainer::getDoBinning() const
{
	return m_binn;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getRefMode() const
{
	return m_ref_mode;
}


bool SingleParticle2dx::ConfigContainer::getDoDoublePick() const
{
	return m_double_pick;	
}


bool SingleParticle2dx::ConfigContainer::getDoCudaFFT() const
{
	return m_cuda_fft;
}


void SingleParticle2dx::ConfigContainer::setDoCudaFFT(bool rhs)
{
	m_cuda_fft = rhs;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getMinStdPicking() const
{
	return m_minstd_pick;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getConsT() const
{
	return m_cons_t;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getModelKeep() const
{
	return m_model_keep;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getInitOffsetX() const
{
	return m_init_offset_x;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getInitOffsetY() const
{
	return m_init_offset_y;
}


SingleParticle2dx::ConfigContainer::size_type SingleParticle2dx::ConfigContainer::getInitOffsetZ() const
{
	return m_init_offset_z;
}


bool SingleParticle2dx::ConfigContainer::getMRAUseNN() const
{
	return m_mra_use_nn;
}


void SingleParticle2dx::ConfigContainer::setMRAUseNN(bool rhs)
{
	m_mra_use_nn = rhs;
}


bool SingleParticle2dx::ConfigContainer::getShowSights() const
{
	return m_show_sights;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getMaskEllipseA() const
{
	return m_mask_ellipse_a;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getMaskEllipseB() const
{
	return m_mask_ellipse_b;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getMaskEllipsePhi() const
{
	return m_mask_ellipse_phi;
}


SingleParticle2dx::ConfigContainer::value_type SingleParticle2dx::ConfigContainer::getNumberOfClasses() const
{
	return m_num_classes;
}


void SingleParticle2dx::ConfigContainer::setImageDirsManually(std::string rhs1, std::string rhs2)
{
	m_image_dirs.clear();
	m_image_dirs.push_back(rhs1);
	m_image_dirs.push_back(rhs2);
	
	if ( getNumberOfImages() != 2 )
	{
		std::cerr << "manually setting of image dirs went wrong" << std::endl;
		throw std::runtime_error("Bad operation");
	}
}

