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
#include "structure_factor.hpp"
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
             * Generates an empty volume with given size
             * @param nx
             * @param ny
             * @param nz
             */
            Volume2dx(int nx=0, int ny=0, int nz=0);
            
            /**
             * Generates a volume with the given header
             * @param header
             */
            Volume2dx(const VolumeHeader2dx& header);
            
            /**
             * Copy constructor
             * @param copy
             */
            Volume2dx(const Volume2dx& copy);
            
            /**
             * Default destructor
             */
            ~Volume2dx();
            
            /**
             * Definition of operator =
             */
            Volume2dx& operator=(const Volume2dx& rhs);
            
            /**
             * Operator + definition
             */
            Volume2dx& operator+(const Volume2dx& rhs);
            
            /**
             * Multiplication by a factor
             */
            Volume2dx& operator*(double factor);
            
            /**
             * Resets the volume with other volume
             */
            void reset(const Volume2dx& other);
            
            /**
             * Clears all the data present
             */
            void clear();
            
            /**
             * Returns a output-able version of the volume
             * @return string converted volume
             */
            std::string to_string() const;
            
            /**
             * Returns a string containing summary of the data
             * present in the volume
             * @return string with summary of data
             */
            std::string data_string() const;
            
            /**
             * Returns the header of the volume
             * @return header
             */
            VolumeHeader2dx header() const;
            
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
             * @param gamma in radians
             */
            void set_gamma_radians(double gamma);
            
            /**
             * Assigns the cell angle gamma in radians
             * @param gamma in degrees
             */
            void set_gamma_degrees(double gamma);
            
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
             * Evaluates the resolution at a miller index h, k, l
             * @param h
             * @param k
             * @param l
             * @return resolution in A
             */
            double resolution_at(int h, int k, int l) const;
            
            /**
             * Fetches the spot with maximum resolution
             * @return MillerIndex which has the maximum resolution in map
             */
            MillerIndex max_resolution_spot() const;
            
            /**
             * Fetches the resolution value of the spot which has maximum resolution
             * @return resolution of spot with maximum resolution (in A)
             */
            double max_resolution() const;
            
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
             * Generates random Gaussian densities in the volume
             * @param fraction_to_fill - fraction of volume to be filled with data
             */
            void generate_random_densities(double fraction_to_fill);
            
            /**
             * Scales the amplitudes such that the max amplitude is set to given value
             * @param max_amplitude
             */
            void rescale_to_max_amplitude(double max_amplitude);
            
            /**
             * Scales the amplitudes such that the energy (sum of all squared amplitudes) is set to given value
             * @param energy
             */
            void rescale_energy(double energy);
            
            /**
             * Re-scales the densities to the new range provided
             * @param min
             * @param max
             */
            void rescale_densities(double min, double max);
            
            /**
             * Scales the densities between (0, 255)
             */
            void grey_scale_densities();
            
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
            void set_fourier(const FourierSpaceData& fourier);
            
            /**
             * Sets the real data
             * @param real
             */
            void set_real(const RealSpaceData& real);
            
            /**
             * Prepares the Fourier data, and brings it to memory. Call this
             * function if you know before-hand that Fourier space data will be required.
             */
            void prepare_fourier();
            
            /**
             * Prepares the real space and brings it to memory. Call this
             * function if you know before-hand that real space data will be required.
             */
            void prepare_real();
            
            /**
             * Sets the data from a volume
             * @param volume
             */
            void set_data(const Volume2dx& volume);
            
            /**
             * Calculates the structure factors. 
             * Structure factors are given as the sum of the intensities of all
             * the spots lying in that resolution range. Internally, the Fourier
             * space is divided in the number of bins and the intensity of a
             * particular spot is added to the correct bin.
             * 
             * @param resolution_bins - number of bins.
             * @param max_resolution 
             * @return instance of class Structure Factors.
             */
            StructureFactors calculate_structure_factors(int resolution_bins, double max_resolution) const;
            
            /**
             * Apply the structure factors. The factors are applied partially with the
             * fraction provided. the highest intensity in the radial distribution 
             * is kept intact.
             * 
             * @param structure_factors
             * @param fraction
             * @see calculate_structure_factors
             */
            void apply_structure_factors(StructureFactors structure_factors, double fraction);
            
            /**
             * Generates a bead model of the current volume and writes it in a PDB file
             * @param no_of_beads
             * @param density_threshold
             * @param noise_level
             * @param pdb_file
             */
            void write_bead_model_pdb(int no_of_beads, double density_threshold, double noise_level, std::string pdb_file);
            
            /**
             * Generates a bead model of the current volume
             * @param no_of_beads
             * @param density_threshold
             * @param bead_model_resolution
             * @return volume
             */
            Volume2dx generate_bead_model(int no_of_beads, double density_threshold, double bead_model_resolution = 2.0);
            
            /**
             * Centers the density along the z axis. Internally adds PI*miller_index_l 
             * to the phase
             */
            void centerize_density_along_z();
            
            /**
             * Centers the density along the x, y, and z axis. Internally adds PI*miller_index_l 
             * to the phase
             */
            void centerize_density_along_xyz();
            
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
            
            /**
             * Applies a density slab in the vertical direction (z-axis) with the
             * fractional height. The new densities are changed only with the 
             * fraction provided. fraction = 1.0 will completely change the map
             * @param height: height in fraction of z height
             * @param fraction: fraction, by which the densities are changed
             * @param centered: Is the density centered along z-axis?
             */
            void apply_density_slab(double height, double fraction, bool centered);
            
            /**
             * Applies a density threshold. Fraction = 1.0 will completely remove 
             * the densities below limit
             * @param limit
             * @param fraction
             */
            void apply_density_threshold(double limit=0.0, double fraction=1.0);
            
            /**
             * Applies a real mask to volume. Takes as input a binary
             * mask and then applies that fractionally to the
             * volume. It will fractionally delete all densities 
             * where the mask is <=0. Fraction=0.0 will not change at all, and fraction = 1.0
             * will do a complete masking.
             * @param mask 
             * @param fraction
             */
            void apply_real_mask(const RealSpaceData& mask, double fraction=1.0);
            
            /**
             * Apply a top-hat band pass filter to the volume.
             * @param low_resolution - Resolution in A (e.g. 100.0)
             * @param high_resolution - Resolution in A (e.g. 4.0)
             */
            void band_pass(double low_resolution, double high_resolution);
            
            /**
             * Apply a top-hat low pass filter to the volume.
             * @param high_resolution - Resolution in A (e.g. 4.0)
             */
            void low_pass(double high_resolution);
            
            /**
             * Applies a Butterworth low pass filter
             * @param high_resolution - highest expected resolution (in A)
             */
            void low_pass_butterworth(double high_resolution);
            
            /**
             * Applies a Gaussian low pass filter
             * @param high_resolution - highest expected resolution (in A)
             */
            void low_pass_gaussian(double high_resolution);
            
            /**
             * Sets all phases to zero, for PSF calculation
             */
            Volume2dx zero_phases();
            
            
            /**
             * Inverts the hand
             */
            void invert_hand();
            
            /**
             * Replace the reflections from a Fourier volume.
             * @param fourier_data
             */
            void replace_reflections(const FourierSpaceData& fourier_data);
            
            /**
             * Replace the reflections from a Fourier volume.
             * @param fourier_data
             */
            void change_amplitudes(const FourierSpaceData& fourier_data);
            
            /**
             * NOT WORKING!!
             * Extends the volume to cells provided and returns it 
             * 0 means no extension, 1 means one cell extended.
             * @param x_cells
             * @param y_cells
             * @param z_cells
             */
            Volume2dx extended_volume(int x_cells, int y_cells=0, int z_cells=0);
            
            /**
             * Subsamples to factor.
             * @param factor
             * @return 
             */
            Volume2dx subsample(int factor);
            
            /**
             * Spreads the data in the Fourier space and tries to fill in the missing spots.
             * @return new volume with spreaded data
             */
            Volume2dx spread_fourier_data();
            
            /**
             * Expands the data to include negative h as well, so that full
             * fourier space is in the memory!
             * @return 
             */
            void extend_to_full_fourier();
            
            /**
             * Applies a negative b-factor sharpening to the volume. 
             * Mathematically multiplies all the amplitudes by 
             * exp(B/4/resolution**2) 
             * @param negative_temp_factor
             */
            Volume2dx apply_bfactor(double negative_temp_factor);

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

