/* 
 * File:   VolumeReal.hpp
 * Author: Nikhil Biyani (nikhilbiyani@gmail.com)
 *
 * Created on March 4, 2015, 5:01 PM
 */

#ifndef VOLUMEREAL_HPP
#define	VOLUMEREAL_HPP

#include "Volume2dx.hpp"


typedef WeightedValue<double> WeightedDouble;

/*
 * Class to define functions for Real Volume
 * Stores data in voxel format
 */

class VolumeReal : public Volume2dx<double>{
public:    
    /*================
     * Constructors
     =================*/
    /*
     * Default Constructor
     */
    VolumeReal(){};
    
    /*
     * Constructor with a header
     */
    VolumeReal(const VolumeHeader& header) : Volume2dx<double>(header) {};   

    ~VolumeReal(){};
    
    void write_volume(const char* outFileName);
    
    /*================
     * Additional methods
     =================*/
    /*
     * Gets the data in the format of a double*
     */
    double* array_data() const;
    
    /*
     * Sets the data from double* array
     */
    void array_data(const double*);
    
    
    /*=======================
     * Constraints
     ========================*/
    
    /*
     * Assuming density we are interested is in the top and bottom of z-axis
     */
    VolumeReal membrane_slab(int membrane_height);
    
};

#endif	/* VOLUMEREAL_HPP */

