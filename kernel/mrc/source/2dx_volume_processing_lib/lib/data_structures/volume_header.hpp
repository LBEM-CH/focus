/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef VOLUME_HEADER_HPP
#define	VOLUME_HEADER_HPP

#include <iostream>
#include <string>
#include <stdexcept>
#include <math.h>

#include "../symmetrization/symmetry2dx.hpp"

namespace volume
{
    namespace data
    {
        
        /**
         * A class to store all header information of a 2dx volume.
         * To be used in conjunction with some kind of data (Real/Fourier)
         */
        class VolumeHeader2dx
        {
            
        public:
            /**
             * Default constructor initializing size to 0
             */
            VolumeHeader2dx();
            
            /**
             * Constructor setting the size
             * @param nx : size of x-dimension
             * @param ny : size of y-dimension
             * @param nz : size of z-dimension
             */
            VolumeHeader2dx(int nx, int ny, int nz);
            
            /**
             * Resets the size of the header. Also, changes the cell lengths if they are zero. 
             * Changes rows, columns, sections, (and optionally xlen, ylen, zlen, mx, my, mz)
             * @param nx
             * @param ny
             * @param nz
             */
            void reset_size(int nx, int ny, int nz);
            
            /**
             * Returns an output-able string of the header
             * @return 
             */
            std::string to_string() const;
            
            /**
             * Returns the size of rows
             * @return rows
             */
            int rows() const ;
            
            /**
             * Returns the size of columns
             * @return columns
             */
            int columns() const ;
            
            /**
             * Returns the size of sections
             * @return sections
             */
            int sections() const ;
            
            /**
             * Returns the length in x-direction
             * @return mx
             */
            int mx() const;
            
            /**
             * Returns the length in y-direction
             * @return my
             */
            int my() const;
            
            /**
             * Returns the length in z-direction
             * @return mz
             */
            int mz() const;
            
            /**
             * Returns the x-length of the cell (a)
             * @return xlen
             */
            double xlen() const ;
            
            /**
             * Retruns the y-length of the cell (b)
             * @return ylen
             */
            double ylen() const ;
            
            /**
             * Returns the z-length of the cell (c)
             * @return zlen
             */
            double zlen() const ;
            
            /**
             * Returns the nxstart of the volume
             * @return nxstart
             */
            int nxstart() const;
            
            /**
             * Returns the nystart of the volume
             * @return nystart
             */
            int nystart() const;
            
            /**
             * Returns the nzstart of the volume
             * @return nzstart
             */
            int nzstart() const;
            
            /**
             * Returns the cell angle gamma
             * @return gamma
             */
            double gamma() const ;
            
            /**
             * Returns the string of the symmetry
             * @return symmetry string
             */
            std::string symmetry() const ;
            
            /**
             * Returns the 2dx code of the symmetry
             * @return 2dx code (0 to 16)
             */
            int symmetry_2dx_code() const ;
            
            /**
             * Returns the ccp4 code of the symmetry
             * @return ccp4 code
             */
            int symmetry_ccp4_code() const ;
            
            /**
             * Returns the maximum resolution set
             * @return max_resolution
             */
            double max_resolution() const ;
                      
            /**
             * Assigner function of rows
             * @param rows
             */
            void set_rows(int rows);
            
            /**
             * Assigner function of columns
             * @param columns
             */
            void set_columns(int columns);
            
            /**
             * Assigner function of sections
             * @param sections
             */
            void set_sections(int sections);
            
            /**
             * Assigner function of mx
             * @param mx
             */
            void set_mx(int mx);
            
            /**
             * Assigner function of my
             * @param my
             */
            void set_my(int my);
            
            /**
             * Assigner function of mz
             * @param mz
             */
            void set_mz(int mz);
            
            /**
             * Assigner function of nxstart
             * @param nxstart
             */
            void set_nxstart(int nxstart);
            
            /**
             * Assigner function of nystart
             * @param nystart
             */
            void set_nystart(int nystart);
            
            /**
             * Assigner function of nzstart
             * @param nzstart
             */
            void set_nzstart(int nzstart);
            
            /**
             * Assigner function of xlen
             * @param xlen
             */
            void set_xlen(double xlen);
            
            /**
             * Assigner function of ylen
             * @param ylen
             */
            void set_ylen(double ylen);
            
            /**
             * Assigner function of zlen
             * @param zlen
             */
            void set_zlen(double zlen);
            
            /**
             * Sets the gamma of the cell
             * @param gamma in radians
             */
            void set_gamma(double gamma);
            
            /**
             * Sets the symmetry of the crystal
             * @param symmetry
             */
            void set_symmetry(std::string symmetry);
            
            /**
             * Sets the maximum resolution
             * @param resolution in Angstroems
             */
            void set_max_resolution(double resolution);
           
            
        private:
            
            /**
             * Member initializer function with nx, ny, nz. Sets rest to default.
             * @param nx : x grid size
             * @param ny : y grid size
             * @param nz : z grid size
             */
            void initialize(int nx, int ny, int nz);
            
           /**
            * Number of rows, columns and sections of the volume
            * Example: 108, 108, 401
            * Default: nx, ny, nz
            */
           int _rows, _columns, _sections;
           
           /**
            * The grid size
            * Example: 131, 131, 400
            * Default: nx, ny, nz
            */
           int _mx, _my, _mz;

           /**
            * Cell size
            * Example: 131.0, 131.0, 400.0
            * Default: nx, ny, nz
            */
           double _xlen, _ylen, _zlen;
           
           /**
            * Start of volume in MRC files
            * (Important when reading from MRC files)
            * Example: -10, -10, 0
            * Default: 0, 0, 0
            */
           int _nxstart, _nystart, _nzstart;

           /**
            * Cell angle beta in degrees;
            * NOTE: alpha, beta are = 90 for 2d crystals
            * Example: 30
            * Default: 90
            */
           double _gamma;

           /**
            * Symmetry present in the protein.
            * Possible values for the symmetry in 2d crystals are:
            * P1, P2, P12, P121, C12, P222, P2221, P22121, C222, P4, P422, P4212, P3, P312, P321, P6, P622
            * Default: P1
            */
           volume::symmetrization::Symmetry2dx _symmetry;

           /**
            * Maximum resolution of the volume in Angstroem
            * Example: 3.0
            * Default: 2.0
            */
           double _max_resolution;

           

        };
        
    }
    
}

#endif	/* VOLUME_HEADER_HPP */

