/* 
 * File:   FourierTransform.hpp
 * Author: biyanin
 *
 * Created on March 4, 2015, 4:57 PM
 */

#ifndef FOURIERTRANSFORM_HPP
#define	FOURIERTRANSFORM_HPP

#include <fftw3.h>

#include "VolumeFourier.hpp"
#include "VolumeReal.hpp"

/*
 * Class used to perform Fourier Transforms
 */
class FourierTransform{
private:
    
    void real_to_complex(double *real_data, fftw_complex *complex_data, int nx, int ny, int nz);   
    void complex_to_real(fftw_complex *complex_data, double *real_data, int nx, int ny, int nz);
    
    
    
public:
    VolumeFourier fft(VolumeReal realVolume);
    VolumeReal fft(VolumeFourier fourierVolume);
    
};

#endif	/* FOURIERTRANSFORM_HPP */

