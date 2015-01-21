/* 
 * File:   VolumeHKL.hpp
 * Author: biyanin
 *
 * Created on January 14, 2015, 3:25 PM
 */

#ifndef VOLUMEHKL_HPP
#define	VOLUMEHKL_HPP

#include <string>
#include <iostream>
#include <fstream>
#include <map>

#include "CommonDefinitions.hpp"
#include "Symmetry2dx.hpp"
#include "SymmetryOperations.hpp"

class VolumeHKL{
    
public:
    ReflectionMap reflections;
    int nx;     //The x-dimension of the volume
    int ny;     //The y-dimension of the volume
    int nz;     //The z-dimension of the volume
    Symmetry2dx* symmetry;
    double apix;
    double max_resolution;
    double amp_epsilon;
    bool symmetrized = false;
    
    VolumeHKL(int nx, int ny, int nz, std::string symmetry, double apix, double max_resolutuion, double amp_epsilon);
    VolumeHKL(const VolumeHKL& other);
    ~VolumeHKL(){};
    
    VolumeHKL& operator=(const VolumeHKL& rhs);
    
    friend std::ostream& operator<<(std::ostream& os, const VolumeHKL& volume);
    
    void setMillerIndex(MillerIndex index, Complex2dx value, double weight);
    ReflectionMap averageOutReflections(ReflectionMultiMap refMultiMap);
    
    Reflection getAveragefromList(ReflectionList reflections);
    
    double getResolution(const MillerIndex&);
    
    void addHKZData(std::ifstream& hkzFile);
    void writeHKL(const char* fileName);
    
    void symmetrize();
    
};

#endif	/* VOLUMEHKL_HPP */

