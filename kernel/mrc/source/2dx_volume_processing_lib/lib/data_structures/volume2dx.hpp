/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef VOLUME2DX_HPP
#define	VOLUME2DX_HPP

#include <iostream>
#include <string.h>

#include "volume_header.hpp"
#include "real_space_data.hpp"
#include "fourier_space_data.hpp"
#include "../transforms/fourier_transform_fftw.hpp"

namespace volume
{
    namespace data
    {
        /**
         * A class to represent 2D crystallography volume.
         * Holds the data in both real and imaginary formats and has
         * a header containing all necessary information about the volume.
         * Does a lazy initialization of the data whenever required.
         */
        class Volume2dx
        {
        public:
            /**
             * Default constructor. Generates an empty volume.
             */
            Volume2dx();
            
            /**
             * Generates an empty volume with given size
             * @param nx
             * @param ny
             * @param nz
             */
            Volume2dx(int nx, int ny, int nz);
            
            /**
             * Resets the volume with given size and empty data
             * @param nx
             * @param ny
             * @param nz
             */
            void reset(int nx, int ny, int nz);
            
            /**
             * Gets the real space x dimension of the volume
             * @return nx
             */
            int nx() const ;
            
            /**
             * Gets the real space y dimension of the volume
             * @return ny
             */
            int ny() const ;
            
            /**
             * Gets the real space z dimension of the volume
             * @return nz
             */
            int nz() const ;
            
            /**
             * Get the x dimension of the Fourier half volume
             * @return fx
             */
            int fx() const ;

            /**
             * Get the y dimension of the Fourier half volume
             * @return fy
             */
            int fy() const ;

            /**
             * Get the z dimension of the Fourier half volume
             * @return fz
             */
            int fz() const ;
            
            /**
             * The maximum possible h for the volume.
             * NOTE: Doesn't take into account the highest resolution
             * @return maximum possible h
             */
            int h_max() const ;
            
            /**
             * The maximum possible k for the volume.
             * NOTE: Doesn't take into account the highest resolution
             * @return maximum possible k
             */
            int k_max() const ;
            
            /**
             * The maximum possible l for the volume.
             * NOTE: Doesn't take into account the highest resolution
             * @return maximum possible l
             */
            int l_max() const ;
            
            /**
             * Returns the cell length a
             * @return xlen
             */
            double xlen() const;
            
            /**
             * Sets the cell length a 
             * @param xlen
             */
            void set_xlen(double xlen);
            
            /**
             * Returns the cell length b
             * @return ylen
             */
            double ylen() const;
            
            /**
             * Sets the cell length b
             * @param ylen
             */
            void set_ylen(double ylen);
            
            /**
             * Returns the cell length c
             * @return zlen 
             */
            double zlen() const;
            
            /**
             * Sets the cell length c
             * @param zlen
             */
            void set_zlen(double zlen);
            
            /**
             * Returns the cell angle gamma
             * @return gamma
             */
            double gamma() const;
            
            /**
             * Assigns the cell angle gamma in radians
             * @param gamma
             */
            void set_gamma(double gamma);
            
            /**
             * Gets the string of the assigned 2dx symmetry
             * @return symmetry
             */
            std::string symmetry() const;
            
            /**
             * Sets the symmetry of the map
             * @param symmetry : one of the 17 2dx symmetries of the volume
             */
            void set_symmetry(std::string symmetry);
            
            /**
             * Returns the max resolution supplied by the user
             * @return max_resolution
             */
            double max_resolution() const;
            
            /**
             * Assigns the maximum resolution of the volume
             * @param max resolution
             */
            void set_max_resolution(double resolution);
            
            /**
             * Evaluates the resolution at a miller index h, k, l
             * @param h
             * @param k
             * @param l
             * @return resolution in A
             */
            double resolution_at(int h, int k, int l) const;
            
            /**
             * Reads the volume from a file and sets the data.
             * Supported formats: hkz, mrc 
             * @param file_name
             * @param format - format of the input volume (hkz/mrc)
             */
            void read_volume(std::string file_name, std::string format);
            
            /**
             * Reads in the file and sets the data. 
             * Automatically detects the format. Supported formats: hkz/mrc
             * @param file_name
             */
            void read_volume(std::string file_name);
            
            /**
             * Writes the volume in the given file with the given format.
             * Supported writing formats: hkl, mrc
             * @param file_name
             * @param format
             */
            void write_volume(std::string file_name, std::string format);
            
            /**
             * Writes the volume in a file and automatically detects the format.
             * Supported formats: hkl, mrc
             * @param file_name
             */
            void write_volume(std::string file_name);
            
            /**
             * Symmetrizes the volume.
             */
            void symmetrize();
            
            /**
             * Fetches the real valued density at (x,y,z) from the volume
             * @param x
             * @param y
             * @param z
             * @return real valued density
             */
            double density_at(int x, int y, int z);
            
            /**
             * Generates a bead model of the current volume and writes it in a PDB file
             * @param no_of_beads
             * @param density_threshold
             * @param noise_level
             * @param pdb_file
             */
            void write_bead_model_pdb(int no_of_beads, double density_threshold, double noise_level, std::string pdb_file);
            
            /**
             * Generates the density histogram from the reference and applies 
             * it fractionally/partially to the current volume.
             * Internally, the highest value of density of the volume is partially set to 
             * the highest value from reference volume and same is done for all
             * other densities. With partial it is meant that:
             * new_density = fraction*reference_density + (1-fraction)*(old_density)
             * fraction = 1.0 will completely change the map
             * 
             * @param reference - Reference volume to be used to get density histogram
             * @param fraction - The fraction (between 0 and 1) with which the density values
             *                   from reference map is to be applied.
             */
            void apply_density_histogram(Volume2dx reference, double fraction);
            
            /**
             * Generates the density histogram from the reference and applies 
             * it the current volume.
             * Internally, the highest value of density of the volume is set to 
             * the highest value from reference volume and same is done for all
             * other densities.
             * 
             * @param reference - Reference volume to be used to get density histogram
             */
            void apply_density_histogram(Volume2dx reference);

        private:
            
            /**
             * ENUM to define data type of class
             * NONE: Nothing in memory/ Nothing initialized
             * REAL: Real data initialized
             * FOURIER: Fourier data initialized
             * BOTH: Both data are initialized and in memory
             */
            enum VolumeDataType {
                NONE=0, REAL=1, FOURIER=2, BOTH=3
            };
            
            /**
             * Initializes the data and header with the given size
             * @param nx
             * @param ny
             * @param nz
             */
            void initialize(int nx, int ny, int nz);
            
            /**
             * Checks if the current volume has Fourier data initialized
             * @return true if Fourier data initialized/ False otherwise
             */
            bool has_fourier() const;
            
            /**
             * Checks if the current volume has real data initialized
             * @return true if real data initialized/ False otherwise
             */
            bool has_real() const;
            
            /**
             * Returns the Fourier data.
             * Checks if the data is initialized, if not then does initialization
             * @return Fourier Space data
             */
            FourierSpaceData get_fourier();
            
            /**
             * Returns the Real space data.
             * Checks if the data is initialized, if not then does initialization
             * @return Real space data
             */
            RealSpaceData get_real();
            
            /**
             * Sets the Fourier data
             * @param fourier
             */
            void set_fourier(FourierSpaceData& fourier);
            
            /**
             * Sets the real data
             * @param real
             */
            void set_real(RealSpaceData& real);
            
            /**
             * Sets the Fourier data from the real data. Does FFT internally
             */
            void fourier_from_real();
            
            /**
             * Sets the real data from the Fourier data. Does FFT internally
             */
            void real_from_fourier();
            
            /**
             * Information of the volume
             */
            VolumeHeader2dx _header;
            
            /**
             * Real space data
             */
            RealSpaceData _real;
            
            /**
             * Fourier space data
             */
            FourierSpaceData _fourier;
            
            /**
             * Transforming between real and Fourier data. To be used for wisdom
             */
            volume::transforms::FourierTransformFFTW _transform;
            
            /**
             * Type of data being hold in the volume
             */
            VolumeDataType _type;
            
            
            
        };
        
    }
    
}


#endif	/* VOLUME2DX_HPP */

