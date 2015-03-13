/* 
 * File:   Volume2dx.hpp
 * Author: Nikhil Biyani (nikhilbiyani@gmail.com)
 *
 * Created on March 2, 2015, 4:04 PM
 */

#ifndef VOLUME2DX_HPP
#define	VOLUME2DX_HPP

#include <map>
#include <iostream>
#include <fstream>
#include <math.h>
#include <string.h>

#include "VolumeHeader.hpp"
#include "WeightedValue.hpp"
#include "Triplet.hpp"
#include "NumericalUtils.hpp"

/* 
 * An abstract class to provide all the required functionalities
 * of a 2D-Electron Crystallography volume.
 * Use this class for any instance of a Volume to be created in 2dx
 */
template<class T>
class Volume2dx{
public:
    /*================
     * Constructors
     =================*/
    /*
     * Default Constructor
     */
    Volume2dx(){};
    
    /*
     * Constructor with a header
     */
    Volume2dx(const VolumeHeader& header){
        this->_header = header;
    };
    
    Volume2dx(const Volume2dx& copy){
        VolumeHeader header(copy.get_header());
        std::map<Triplet2dx, WeightedValue<T> >data(copy._data);
        this->_header = header;
        this->_data = data;
    }
    
    virtual ~Volume2dx(){};
    
    /*================
     * Operators
     =================*/
    
    /*
     * Operator Overloading of =
     */
    Volume2dx<T>& operator=(const Volume2dx<T>& rhs){
        this->_data = rhs._data;
        this->_header = rhs._header;
    };
    
    /*=====================
     * Getters and Setters
     ======================*/
    
    /*
     * Returns the WeightedValue at the index x, y, z
     */
    WeightedValue<T> get_value_at(int x, int y, int z) const{
        WeightedValue<T> wtdValue;
        if(exists(x,y,z)){
            wtdValue = _data.at(Triplet2dx(x,y,z));
        }
            
        return wtdValue;
    };
    
    /*
     * Sets the value at index x, y, z
     */
    void set_value_at(int x, int y, int z, WeightedValue<T> weightedValue){
        Triplet2dx index = Triplet2dx(x, y, z);
        if(exists(x,y,z)) _data.erase(index);
        _data.insert(std::pair<Triplet2dx, WeightedValue<T> >(index, weightedValue));
    };
    
    /*
     * Returns the header
     */
    VolumeHeader get_header() const {
        return _header;
    };
    
    /*
     * Set the header
     */
    void set_header(VolumeHeader header){
        this->_header = header;
    };
    
    /*
     * Get the x dimension of the volume
     */
    int nx() const{
        return this->_header.nx;
    };
    
    /*
     * Get the y dimension of the volume
     */
    int ny() const{
        return this->_header.ny;
    };
    
    /*
     * Get the z dimension of the volume
     */
    int nz() const{
        return this->_header.nz;
    };
    
    /*=============================
     * Reading and Writing to Files
     ===============================*/
    
    /*
     * Function to read in the volume from a file
     */
    virtual void read_volume(std::string inFileName){};
    
    /*
     * Function to write the volume to a file
     */
    virtual void write_volume(const char* outFileName){};
    
    
    /*=======================
     * Mathematical Operations
     ========================*/
    
    /*
     * Masks the volume with another mask
     */
    virtual void mask(Volume2dx<T>& mask){};
    
    /*
     * Scale the values of the components by a factor
     */
    void scale(double factor){
        typename std::map<Triplet2dx, WeightedValue<T> >::iterator ii;
        for(ii=_data.begin(); ii!=_data.end(); ++ii){ 
                (*ii).second.setValue((*ii).second.getValue()*factor);
        }
    };
    
    
    /*=======================
     * Statistical Functions
     ========================*/
    
    /*
     * Returns the maximum of the volume
     */
    WeightedValue<T> get_maximum() const{
        WeightedValue<T> max = (*_data.begin()).second;
        for(typename std::map<Triplet2dx, WeightedValue<T> >::const_iterator ii=_data.begin(); ii!=_data.end(); ++ii){
            WeightedValue<T> current = (*ii).second;
            if(max < current) max = current;
        }
        return max;
    };
    
    /*
     * Returns the minimum of the volume
     */
    WeightedValue<T> get_miminum() const{
        WeightedValue<T> minval = (*_data.begin()).second;
        for(typename std::map<Triplet2dx, WeightedValue<T> >::const_iterator ii=_data.begin(); ii!=_data.end(); ++ii){
            WeightedValue<T> current = (*ii).second;
            if(current < minval) minval = current;
        }
        return minval;
    };
    
    /*=======================
     * Utility Functions
     ========================*/
    
    /*
     * A function to return the resolution of the spot at (x, y, z)
     */
    virtual double get_resolution(int x, int y, int z) const{
        double resolution = sqrt(x*x + y*y + z*z);
        return resolution;
    };
  
private:
    bool exists(int x, int y, int z){
        if ( _data.find(Triplet2dx(x, y, z)) == _data.end() ) {
            return false;
        } else {
            return true;
        }       
    }    
    
protected:
    //Members
    VolumeHeader _header;
    std::map<Triplet2dx, WeightedValue<T> > _data;
    
    
};

#endif	/* VOLUME2DX_HPP */
