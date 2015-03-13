/* 
 * File:   VolumeFourier.hpp
 * Author: Nikhil Biyani(nikhilbiyani@gmail.com)
 *
 * Created on March 4, 2015, 12:03 PM
 */

#ifndef VOLUMEFOURIER_HPP
#define	VOLUMEFOURIER_HPP

#include <list>
#include <fftw3.h>

#include "Volume2dx.hpp"
#include "Complex2dx.hpp"
#include "MillerIndex.hpp"
#include "Symmetry2dx.hpp"
#include "SymmetryOperations.hpp"

typedef WeightedValue<Complex2dx> WeightedComplex;

/*
 * Class to define functions for Fourier Volume
 * Stores data in HKZ format
 */

class VolumeFourier : public Volume2dx<Complex2dx>{
    
public:
    
    /*================
     * Constructors
     =================*/
    /*
     * Default Constructor
     */
    VolumeFourier(){};
    
    /*
     * Constructor with a header
     */
    VolumeFourier(const VolumeHeader& header){
        this->_header = header;
    };
        
    ~VolumeFourier(){};
    
    /*
     * Overrides the method from Volume2dx
     * Reads in an HKL file and initializes the maps
     * HKL file is expected to be in the following format:
     * h    k   l   amplitude   phase   sig_amp sig_phase   iq_value
     */
    void read_volume(std::string hkzFileName);
    
    /*
     * Overrides the method from Volume2dx
     * Writes in an HKL file
     * HKL file is written in the following format:
     * h    k   l   amplitude   phase   FOM
     */
    void write_volume(const char* hklFileName);
    
    /*
     * Returns the resolution of a Miller Index h, k, l
     */
    double get_resolution(int x, int y, int z) const;
    
    /*===============================
     * Additional Methods
     ================================*/
    /*
     * Symmetrizes with 2d crystallographic symmetry
     */
    void symmetrize();
    
    /*
     * Converts the data into an array of fftw_complex to be used for Fourier transforms
     */
    fftw_complex* to_fftw_complex() const;
    
    void from_fftw_complex(const fftw_complex*);
    
private:
    
    /*
     * Averages out the reflections assigned to one index
     */
    std::map<Triplet2dx, WeightedComplex> averageOutReflections(std::multimap<Triplet2dx, WeightedComplex> refMultiMap);
    
    /*
     * Averages from a list of Weighted values
     */
    WeightedValue<Complex2dx> averageFromList(std::list<WeightedValue<Complex2dx> > values);
        
};


#endif	/* VOLUMEFOURIER_HPP */

