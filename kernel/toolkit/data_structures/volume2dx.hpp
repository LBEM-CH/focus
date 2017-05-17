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
#include "reflection_data.hpp"
#include "binned_data.hpp"
#include "mesh_binned_data.hpp"
#include "../transforms/fourier_transform_fftw.hpp"

namespace tdx
{
    namespace data
    {
        /**
         * A class to represent 2D crystallography volume.
         * Holds the data in both real and imaginary formats and has
         * a header containing all necessary information about the volume.
         * Does a lazy initialization of the data whenever required.
         */
        class Volume2DX
        {
        public:
            
            /**
             * Generates an empty volume with given size
             * @param nx
             * @param ny
             * @param nz
             */
            Volume2DX(int nx=0, int ny=0, int nz=0);
            
            /**
             * Generates a volume with the given header
             * @param header
             */
            Volume2DX(const VolumeHeader& header);
            
            /**
             * Copy constructor
             * @param copy
             */
            Volume2DX(const Volume2DX& copy);
            
            /**
             * Default destructor
             */
            ~Volume2DX();
            
            /**
             * Definition of operator =
             */
            Volume2DX& operator=(const Volume2DX& rhs);
            
            /**
             * Operator + definition
             */
            Volume2DX operator+(const Volume2DX& rhs);
            
            /**
             * Multiplication by a factor
             */
            Volume2DX operator*(double factor);
            
            /**
             * Resets the volume with other volume
             */
            void reset(const Volume2DX& other);
            
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
            VolumeHeader header() const;
            
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
             * Cuts the specified XY plane from Fourier space
             * @param xy_plane: Write out the xy_plane in this volume
             * @param missing_plane: Write the volume with missing plane
             * @param plane_number: which plane to cut (int)
             */
            void cut_xy_plane(Volume2DX& xy_plane, Volume2DX& missing_plane, int plane_number=0);
            
            /**
             * Cuts a cone in Fourier space with specified degree
             * @param cone: Write out the cut cone in this volume
             * @param missing_cone: Write the result in this volume
             * @param cone_angle: Angle in degrees of cone to cut
             */
            void cut_cone(Volume2DX& cone, Volume2DX& missing_cone, double cone_angle=30);
            
            /**
             * Generates random Gaussian densities in the volume
             * @param fraction_to_fill - fraction of volume to be filled with data
             */
            void generate_random_densities(double fraction_to_fill);
            
            /**
             * Changes phases randomly in Fourier space
             * @param fraction_to_change - fraction of phases to be changed
             */
            void generate_fourier_noise(double fraction_to_change);
            
            /**
             * Generates random Gaussian densities in the volume
             * @param expected_density_sum - The expected value of density sum
             */
            void generate_poisson_densities(double expected_density_sum);
            
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
             * Returns a projection in the direction of axis
             * @param axis: x/y/z
             */
            Volume2DX projection2D(char axis);
            
             /**
             * Calculates the average of all voxels along axis
             * in real space.
             * @param axis: x/y/z
             */
            Volume2DX average2D(char axis);
            
            /**
             * Get the slice from the real space
             * @param slice_no
             * @return 
             */
            Volume2DX get_slice(int slice_no);
            
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
            ReflectionData get_fourier();
            
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
            void set_fourier(const ReflectionData& fourier);
            
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
            void set_data(const Volume2DX& volume);
            
            /**
             * Calculates the structure factors. 
             * Structure factors are given as the sum of the intensities of all
             * the spots lying in that resolution range. Internally, the Fourier
             * space is divided in the number of bins and the intensity of a
             * particular spot is added to the correct bin.
             * 
             * @param minimum frequency (in 1/A)
             * @param maximum frequency (in 1/A)
             * @param resolution_bins - number of bins.
             * @return instance of class Resolution binned data.
             */
            BinnedData calculate_structure_factors(double min_freq, double max_freq, int resolution_bins);
            
            /**
             * Calculates the Fourier shell correlation with the reference. The
             * min and max frequency range and the number of resolution bins to
             * be used can also be controlled. Outputs the Binned data class
             * instance which contains the sum of the FSC in each bins and 
             * can be used to output the final FSC. 
             * @param reference
             * @param min_freq
             * @param max_freq
             * @param resolution_bins
             * @return instance of class BinnedData
             */
            BinnedData fourier_shell_correlation(Volume2DX reference, double min_freq=0, double max_freq=0.5, int resolution_bins=50);
            
            /**
             * Calculates the Fourier conic correlation with the reference. The
             * min and max cone angle and the number of bins to
             * be used can also be controlled. Outputs the Binned data class
             * instance which contains the sum of the FCC in each bins and 
             * can be used to output the final FCC. 
             * @param reference
             * @param min_cone_angle (in degrees)
             * @param max_cone_angle (in degrees)
             * @param bins
             * @return instance of class BinnedData
             */
            BinnedData fourier_conic_correlation(Volume2DX reference, double min_cone_angle=0, double max_cone_angle=90, int bins=90);
            
            /**
             * Calculates the Fourier conic mesh correlation with the reference.
             * This correlation measures correlation on a mesh grid over frequency
             * and cone angle resulting in a two dimensional plot.
             * The min and max cone angle, frequency and the number of bins to
             * be used can also be controlled. Outputs the Binned data class
             * instance which contains the sum of the FCMC in each bin and 
             * can be used to output the final FCMC. 
             * @param reference
             * @param min frequency (in 1/A)
             * @param max frequency (in 1/A)
             * @param min_cone_angle (in degrees)
             * @param max_cone_angle (in degrees)
             * @param resolution bins
             * @param conical bins
             * @return instance of class MeshedBinnedData
             */
            MeshBinnedData fourier_conic_mesh_correlation(Volume2DX reference, double min_freq=0, double max_freq=0.5, double min_cone=0, double max_cone=90, int resolution_bins=50, int cone_bins=36);
            
            /**
             * Calculates the CRC between current volume and it's reference and 
             * returns a MeshBinnedData which can be used to output the final
             * mesh plot.
             * @param (Volume2DX) reference
             * @param (int) bins in x and in y direction
             * @return (MeshBinnedData)
             * @see MeshBinnedData
             */
            MeshBinnedData cylindrical_ring_correlation(Volume2DX reference, int bins=50);
            
            /**
             * Apply the structure factors. The factors are applied partially with the
             * fraction provided. the highest intensity in the radial distribution 
             * is kept intact.
             * 
             * @param structure_factors
             * @param fraction
             * @see calculate_structure_factors
             */
            void apply_structure_factors(BinnedData structure_factors, double fraction);
            
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
            Volume2DX generate_bead_model(int no_of_beads, double density_threshold, double bead_model_resolution = 2.0);
            
            /*
             * Shifts the volume to given values. Internally first gets fourier
             * transform and then changes the phases.
             */
            void shift_volume(double x_shift, double y_shift, double z_shift);
            
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
            void apply_density_histogram(Volume2DX reference, double fraction);
            
            /**
             * Generates the density histogram from the reference and applies 
             * it the current volume.
             * Internally, the highest value of density of the volume is set to 
             * the highest value from reference volume and same is done for all
             * other densities.
             * 
             * @param reference - Reference volume to be used to get density histogram
             */
            void apply_density_histogram(Volume2DX reference);
            
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
            Volume2DX zero_phases();
            
            
            /**
             * Inverts the hand (Inverts Fourier data in a specific direction)
             * @param direction = 0 for all axis, 1 for x axis invert, 2 for y axis invert, 3 for z axis invert, 
             */
            void invert_hand(int direction);
            
            /**
             * Replace the reflections from a Fourier volume.
             * @param fourier_data
             * @param cone_angle (in degrees) - (Default 90 degrees), if provided would keep the input reflections and would
             *                                   change other reflections in the conical region provided
             * @param replacement_amplitude_cutoff - Reflections in input with this amplitude value or more will only be changed
             */
            void replace_reflections(const ReflectionData& fourier_data, double cone_angle = 90, double replacement_amplitude_cutoff = 0.0);
            
            /**
             * Replace the reflections from a Fourier volume.
             * @param fourier_data
             */
            void change_amplitudes(const ReflectionData& fourier_data);
            
            /**
             * NOT WORKING!!
             * Extends the volume to cells provided and returns it 
             * 0 means no extension, 1 means one cell extended.
             * @param x_cells
             * @param y_cells
             * @param z_cells
             */
            Volume2DX extended_volume(int x_cells, int y_cells=0, int z_cells=0);
            
            /**
             * Subsamples to factor.
             * @param factor
             * @return 
             */
            Volume2DX subsample(int factor);
            
            /**
             * Spreads the data in the Fourier space and tries to fill in the missing spots.
             * @return new volume with spreaded data
             */
            Volume2DX spread_fourier_data();
            
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
            Volume2DX apply_bfactor(double negative_temp_factor);

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
            VolumeHeader _header;
            
            /**
             * Real space data
             */
            RealSpaceData _real;
            
            /**
             * Fourier space data
             */
            ReflectionData _fourier;
            
            /**
             * Transforming between real and Fourier data. To be used for wisdom
             */
            tdx::transforms::FourierTransformFFTW _transform;
            
            /**
             * Type of data being hold in the volume
             */
            VolumeDataType _type;
            
            
            
        };
        
    }
    
}


#endif	/* VOLUME2DX_HPP */

