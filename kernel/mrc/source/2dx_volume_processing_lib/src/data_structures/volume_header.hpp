/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef VOLUME_HEADER_HPP
#define	VOLUME_HEADER_HPP

#include <string>
#include <stdexcept>
#include <math.h>

#include "../symmetization/symmetry2dx.hpp"

namespace volume_processing_2dx
{
    namespace data_structures
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
             * @param nz : size of z-dimention
             */
            VolumeHeader2dx(int nx, int ny, int nz);
            
            /**
             * Returns the size of the x-dimension
             * @return nx
             */
            int nx() const ;
            
            /**
             * Returns the size of the y-dimension
             * @return ny
             */
            int ny() const ;
            
            /**
             * Returns the size of the z-dimension
             * @return nz
             */
            int nz() const ;
            
            /**
             * Returns the x-size of the input data
             * @return mx
             */
            int mx() const ;
            
            /**
             * Returns the y-size of the input data
             * @return my
             */
            int my() const ;
            
            /**
             * Returns the z-size of the input data
             * @return 
             */
            int mz() const ;
            
            /**
             * Returns the x-position of the subimage 
             * @return nxstart
             */
            int nxstart() const ;
            
            
            /**
             * Returns the y-position of the subimage
             * @return nystart
             */
            int nystart() const ;
            
            
            /**
             * Retruns the z-position of the subimage
             * @return nzstart
             */
            int nzstart() const ;
            
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
             * Returns the cell angle alpha
             * @return alpha
             */
            double alpha() const ;
            
            /**
             * Returns the cell angle beta
             * @return beta
             */
            double beta() const ;
            
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
             * Returns the membrane height in fraction of nz
             * @return membrane_height 
             * @see nz
             */
            double membrane_height() const ;
            
            /**
             * Assigner function of nx
             * @param nx
             */
            void set_nx(int nx);
            
            /**
             * Assigner function of ny
             * @param ny
             */
            void set_ny(int ny);
            
            /**
             * Assigner function of nz
             * @param nz
             */
            void set_nz(int nz);
            
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
            
            /**
             * Sets the membrane height
             * @param membrane_height as a fraction of nz
             */
            void set_membrane_height(double membrane_height);
            
        private:
            
            /**
             * Member initializer function with nx, ny, nz. Sets rest to default.
             * @param nx
             * @param ny
             * @param nz
             */
            void initialize(int nx, int ny, int nz);
            
           /**
            * Number of rows, columns and sections of the volume
            * Example: 108, 108, 401
            * **Compulsory
            */
           int _nx, _ny, _nz;

           /**
            * Size of the data present in the volume.
            * Example: 131, 131, 400
            * Default: nx, ny, nz
            */
           int _mx, _my, _mz;

           /**
            * Starting point of sub/super image. The rest of image is constructed using periodicity
            * Example: -131, -131, 5
            * Example: 5, 5, 0
            * Default: 0, 0, 0
            */
           int _nxstart, _nystart, _nzstart;

           /**
            * Cell size
            * Example: 131.0, 131.0, 400.0
            * Default: nx, ny, nz
            */
           double _xlen, _ylen, _zlen;

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
           volume_processing_2dx::symmetrization::Symmetry2dx _symmetry;

           /**
            * Maximum resolution of the volume in Angstroem
            * Example: 3.0
            * Default: 2.0
            */
           double _max_resolution;

           /**
            * Expected Membrane height in fraction of the Z-spacing
            * Example: 0.7
            * Default: 1.0
            */
           double _membrane_height;

        };
        
    }
    
}

#endif	/* VOLUME_HEADER_HPP */

