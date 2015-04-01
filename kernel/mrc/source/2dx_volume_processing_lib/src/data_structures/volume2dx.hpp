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

namespace volume_processing_2dx
{
    namespace data_structures
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
             * Returns the cell length b
             * @return ylen
             */
            double ylen() const;
            
            /**
             * Returns the cell length c
             * @return zlen 
             */
            double zlen() const;
            
            /**
             * Returns the cell angle gamma
             * @return gamma
             */
            double gamma() const;
            
            /**
             * Returns the max resolution supplied by the user
             * @return max_resolution
             */
            double max_resolution() const;
            
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
            volume_processing_2dx::transforms::FourierTransformFFTW _transform;
            
            /**
             * Type of data being hold in the volume
             */
            VolumeDataType _type;
            
            
            
        };
        
    }
    
}


#endif	/* VOLUME2DX_HPP */

