/* 
 * File:   VolumeHeader.hpp
 * Author: Nikhil Biyani (nikhilbiyani@gmail.com)
 *
 * Created on March 2, 2015, 4:10 PM
 */

#ifndef VOLUMEHEADER_HPP
#define	VOLUMEHEADER_HPP

#include <string>
#include <math.h>

/*
 * A class to store all header information of a 2dx volume.
 */
class VolumeHeader{
public:
    //Members
    int nx, ny, nz;             // Number of rows, columns and sections
    double gamma;                // Cell angle beta; alpha, beta are = 90 for 2d crystals
    std::string symmetry;       // symmetry present in the protein
    double apix;                // Pixel size in Angstroem/Pixel
    double max_resolution;      // Maximum resolution of the volume 
    
    //Constructors
    VolumeHeader(){
        initialize(0, 0, 0, M_PI/2, "P1", 1.0, 2.0);
    };
    
    VolumeHeader(const VolumeHeader& copy){
        initialize(copy.nx, copy.ny, copy.nz, copy.gamma, copy.symmetry, copy.apix, copy.max_resolution);
    };
    
private:
    void initialize(int nx, int ny, int nz, double gamma, std::string symmetry, double apix, double max_resolution){
        this->nx = nx;
        this->ny = ny;
        this->nz = nz;
        this->gamma = gamma;
        this->symmetry = symmetry;
        this->apix = apix;
        this->max_resolution = max_resolution;
    };

};

#endif	/* VOLUMEHEADER_HPP */

