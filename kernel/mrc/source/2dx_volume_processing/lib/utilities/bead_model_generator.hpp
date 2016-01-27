/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef BEAD_MODEL_GENERATOR_HPP
#define	BEAD_MODEL_GENERATOR_HPP

#include <iostream>
#include <string.h>
#include <fstream>

#include "../data_structures/volume.hpp"
#include "../data_structures/real_space_data.hpp"

namespace tdx 
{
    
    namespace utilities 
    {

        /**
         * Class to generate the bead model for any given volume.
         */
        class BeadModelGenerator 
        {
        public:
            /**
             * Constructor initializing the parameters used to generate the bead model
             * @param number_of_beads
             * @param density_threshold
             * @param noise_level
             */
            BeadModelGenerator(int number_of_beads, double density_threshold, double noise_level, double max_resolution = 2.0);

            /**
             * Generate the bead model from volume.
             * @param input volume
             * @return output real space data
             */
            tdx::data::RealSpaceData generate_bead_model_volume(tdx::data::Volume volume);
            
            /**
             * Generate the bead model from volume and write the output pdb.
             * @param volume
             * @param output_pdb_file
             */
            void generate_bead_model_coordinates(tdx::data::Volume volume, std::string output_pdb_file);

        private:
            
            /**
             * Outputs the pdb header to the file in correct format
             * NOTE: Only Symmetry record is written
             * @param out_file_stream
             * @param a - cell length a 
             * @param b - cell length b
             * @param c - cell length c
             * @param alpha - cell angle alpha in degrees
             * @param beta - cell angle beta in degrees
             * @param gamma - cell angle gamma in degrees
             * @param symmetry - symmetry string
             */
            void write_pdb_header(std::ofstream& out_file_stream, 
                                  double a, double b, double c,
                                  double alpha, double beta, double gamma,
                                  std::string symmetry);
            
            /**
             * Outputs a coordinate line in PDB format to the output stream
             * @param out_file_stream
             * @param atom_id
             * @param atom_type
             * @param x
             * @param y
             * @param z
             */
            void write_pdb_coordinate(std::ofstream& out_file_stream,
                                       int atom_id,
                                       std::string atom_type,
                                       int x, int y, int z);

            /**
             * Member to hold density threshold
             */
            double density_threshold;

            /**
             * Noise level of the beads
             */
            double noise_level;

            /**
             * Number of beads to be put up in the model
             */
            int number_of_beads;
            
            /**
             * Maximum resolution to reach
             */
            double max_resolution;

            /**
             * Average percentage of oxygen in PDB
             */
            const double PDB_CARBON_FRACTION = 0.622;

            /**
             * Average percentage of Nitrogen in PDB
             */
            const double PDB_NITROGEN_FRACTION = 0.172;

            /**
             * Average percentage of Oxygen in PDB
             */
            const double PDB_OXYGEN_FRACTION = 0.201;

            /**
             * Average percentage of Sulfur in PDB
             */
            const double PDB_SULFUR_FRACTION = 0.005;
        };

    }
}


#endif	/* BEAD_MODEL_GENERATOR_HPP */

