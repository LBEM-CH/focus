/* 
 * File:   VolumeHeader2dx.hpp
 * Author: Nikhil Biyani (nikhilbiyani@gmail.com)
 *
 * Created on March 2, 2015, 4:10 PM
 */

#ifndef VOLUMEHEADER2DX_HPP
#define	VOLUMEHEADER2DX_HPP

#include <string>
#include <math.h>
#include <iostream>

#include "Symmetry2dx.hpp"
#include "NumericalUtils.hpp"

/*
 * A class to store all header information of a 2dx volume.
 * To be used in conjunction with some kind of data 
 */
class VolumeHeader2dx{

public:
    /*============================
     * Constructors
     *===========================*/
    
    /*
     * Default constructor setting nx, ny, nz to 0 and rest to default values
     */
    VolumeHeader2dx(){
        initialize(0, 0, 0);
    };
    
    /*
     * constructor setting nx, ny, nz and other default values
     */
    VolumeHeader2dx(int nx, int ny, int nz){
        initialize(nx, ny, nz);
    };
    
    /*
     * copy constructor
     */
    VolumeHeader2dx(const VolumeHeader2dx& copy){
        initialize(copy._nx, copy._ny, copy._nz,
                   copy._mx, copy._my, copy._mz,
                   copy._nxstart, copy._nystart, copy._nzstart,
                   copy._xlen, copy._ylen, copy._zlen, copy._gamma, 
                   copy._symmetry, copy._apix, copy._max_resolution, copy._membrane_height);
    };
    
    
    /*============================
     * Getters
     *===========================*/
    
    /*
     * Returns the x dimension of the volume
     * @return: integer nx
     */
    int nx() const {return _nx;};
    
    /*
     * Returns the y dimension of the volume
     * @return: integer ny
     */
    int ny() const {return _ny;};
    
    /*
     * Returns the z dimension of the volume
     * @return: integer nz
     */
    int nz() const {return _nz;};
    
    /*
     * Returns the x dimension of the input data 
     * @return mx : Input data's x dimension
     */
    int mx() const {return _mx;};
    
    /*
     * Returns the y dimension of the input data 
     * @return my : Input data's y dimension
     */
    int my() const {return _my;};
    
    /*
     * Returns the z dimension of the input data
     * @return mz : Input data's z dimension
     */
    int mz() const {return _mz;};
    
    /*
     * Returns the x position of the sub-image
     */
    int nxstart() const {return _nxstart;};
    
    /*
     * Returns the y position of the sub-image
     */
    int nystart() const {return _nystart;};
    
    /*
     * Returns the z position of the sub-image
     */
    int nzstart() const {return _nzstart;};
    
    /*
     * Returns the cell size in x direction
     */
    double xlen() const {return _xlen;};
    
    /*
     * Returns the cell size in y direction
     */
    double ylen() const {return _ylen;};
    
    /*
     * Returns the cell size in z direction
     */
    double zlen() const {return _zlen;};
    
    /*
     * Returns the gamma of cell in degrees
     * @return double gamma in degrees
     */
    double gamma() const {return _gamma;};
    
    /*
     * Returns the gamma of cell in radians
     * @return double gamma in radians
     */
    double gamma_radian() const {return degree_to_radian(_gamma);};
    
    /*
     * Returns the alpha of cell in degrees
     * @return double alpha in degrees
     */
    double alpha() const {double alpha = 90; return alpha;};
    
    /*
     * Returns the alpha of cell in radians
     * @return double alpha in radians
     */
    double alpha_radian() const {return degree_to_radian(90.0);};
    
    /*
     * Returns the beta of cell in degrees
     * @return double beta in degrees
     */
    double beta() const {double beta = 90; return beta;};
    
    /*
     * Returns the beta of cell in radians
     * @return double beta in radians
     */
    double beta_radian() const {return degree_to_radian(90.0);};
    
    /*
     * Returns the string of the symmetry
     */
    std::string symmetry() const {return _symmetry.getSymmetryString();};
    
    /*
     * Returns the 2dx-code (0-16) of the symmetry
     */
    int symmetry_code() const {return _symmetry.getSymmetryIndex();};
    
    /*
     * Returns the CCP4 index of the symmetry
     */
    int symmetry_ccp4() const {return _symmetry.getCCP4Index();};
    
    /*
     * Returns the apix (Angstroem/Pixel)
     */
    double apix() const {return _apix;};
    
    /*
     * Returns the maximum resolution of the map
     */
    double max_resolution() const {return _max_resolution;};
    
    /*
     * Returns the membrane_height of the map
     */
    double membrane_height() const {return _membrane_height;};
    
    /*============================
     * Setters
     *===========================*/
    
    /*
     * Sets the nx value
     */
    void nx(int nx){
        _nx = nx;
    };
    
    void ny(int ny){
        _ny = ny;
    };
    
    void nz(int nz){
        _nz = nz;
    };
    
    void mx(int mx){
        _mx = mx;
    };
    
    void my(int my){
        _my = my;
    };
    
    void mz(int mz){
        _mz = mz;
    };
    
    void nxstart(int nxstart){
        _nxstart = nxstart;
    };
    
    void nystart(int nystart){
        _nystart = nystart;
    };
    
    void nzstart(int nzstart){
        _nzstart = nzstart;
    };
    
    void xlen(double xlen){
        _xlen = xlen;
    };
    
    void ylen(double ylen){
        _ylen = ylen;
    };
    
    void zlen(double zlen){
        _zlen = zlen;
    };
    
    void gamma(double gamma_in_degrees){
        _gamma = gamma_in_degrees;
    };
    
    void symmetry(std::string symmetry){
        _symmetry = Symmetry2dx(symmetry);
    };
    
    void apix(double apix){
        _apix = apix;
    };
    
    void max_resolution(double max_resolution){
        _max_resolution = max_resolution;
    };
    
    void membrane_height(double membrane_height){
        _membrane_height = membrane_height;
    };
    
private:
    /*============================
     * MRC File header information
     *===========================*/
    
    /*
     * Number of rows, columns and sections of the volume
     * Example: 108, 108, 401
     * **Compulsory
     */
    int _nx, _ny, _nz;
    
    /*
     * Size of the data present in the volume.
     * Example: 131, 131, 400
     * Default: nx, ny, nz
     */
    int _mx, _my, _mz;
    
    /*
     * Starting point of sub/super image. The rest of image is constructed using periodicity
     * Example: -131, -131, 5
     * Example: 5, 5, 0
     * Default: 0, 0, 0
     */
    int _nxstart, _nystart, _nzstart;
    
    /*
     * Cell size
     * Example: 131.0, 131.0, 400.0
     * Default: nx, ny, nz
     */
    double _xlen, _ylen, _zlen;
    
    /*
     * Cell angle beta in degrees;
     * NOTE: alpha, beta are = 90 for 2d crystals
     * Example: 30
     * Default: 90
     */
    double _gamma;
    
    /*============================
     * Additional fields for 2dx
     *===========================*/
    
    /*
     * Symmetry present in the protein.
     * Possible values for the symmetry in 2d crystals are:
     * P1, P2, P12, P121, C12, P222, P2221, P22121, C222, P4, P422, P4212, P3, P312, P321, P6, P622
     * Default: P1
     */
    Symmetry2dx _symmetry;
    
    /*
     * Pixel size in Angstroem/Pixel
     * Example: 1.34
     * Default: 1.0
     */
    double _apix;
    
    /*
     * Maximum resolution of the volume in Angstroem
     * Example: 3.0
     * Default: 2.0
     */
    double _max_resolution;
    
    /*
     * Expected Membrane height in fraction of the Z-spacing
     * Example: 0.7
     * Default: 1.0
     */
    double _membrane_height;

private:    
    /*============================
     * Initializer functions
     *===========================*/
    
    /*
     * A member initializer function with nx, ny, nz
     * Initializes nx, ny, nz and sets rest to default values
     */
    void initialize(int nx, int ny, int nz){
        _nx = nx;
        _ny = ny;
        _nz = nz;
        _mx = nx;
        _my = ny;
        _mz = nz;
        _nxstart = 0;
        _nystart = 0;
        _nzstart = 0;
        _xlen = (double) nx;
        _ylen = (double) ny;
        _zlen = (double) nz;
        _gamma = 90;
        _symmetry = Symmetry2dx("P1");
        _apix = 1.0;
        _max_resolution = 2.0;
        _membrane_height = 1.0;
    };
    
    /*
     * A member initializer function with all the members included
     */
    void initialize(int nx, int ny, int nz, 
                    int mx, int my, int mz,
                    int nxstart, int nystart, int nzstart,
                    double xlen, double ylen, double zlen, double gamma, 
                    Symmetry2dx symmetry, double apix, double max_resolution, double membrane_height){
        _nx = nx;
        _ny = ny;
        _nz = nz;
        _mx = mx;
        _my = my;
        _mz = mz;
        _nxstart = nxstart;
        _nystart = nystart;
        _nzstart = nzstart;
        _xlen = xlen;
        _ylen = ylen;
        _zlen = zlen;
        _gamma = gamma;
        _symmetry = symmetry;
        _apix = apix;
        _max_resolution = max_resolution;
        _membrane_height = membrane_height;
    };

};

#endif	/* VOLUMEHEADER2DX_HPP */

