/* 
 * File:   FourierTransform.hpp
 * Author: biyanin
 *
 * Created on March 4, 2015, 4:57 PM
 */

#ifndef FOURIERTRANSFORM_HPP
#define	FOURIERTRANSFORM_HPP

#include <fftw3.h>
/*
 * Class used to perform Fourier Transforms
 */
class FourierTransform{
    
public:
    
    /*===================
     * Constructors
     ====================*/
    /*
     * Default constructor
     */
    FourierTransform()
    {
        _nx = _ny = _nz = 0;
    };
    
    /*
     * Default Destructor
     */
    ~FourierTransform(){
        fftw_destroy_plan(_plan_r2c);
        fftw_destroy_plan(_plan_c2r);
    };
    
    void real_to_complex(double* real_data, fftw_complex* complex_data, int nx, int ny, int nz){
        
        //Re-plan if required
        if(_nx != nx || _ny != ny || _nz != nz) replan(real_data, complex_data, nx, ny, nz);

        //Execute the plan
        fftw_execute_dft_r2c(_plan_r2c, real_data, complex_data );
        
        //Normalize
        for(int i=0; i<fourier_size(); i++) 
        { 
          ((fftw_complex*)complex_data)[i][0]*=normalize_factor(); 
          ((fftw_complex*)complex_data)[i][1]*=-1*normalize_factor();
        }
        
    };  
    
    void complex_to_real(fftw_complex* complex_data, double* real_data, int nx, int ny, int nz){
        //Re-plan if required
        if(_nx != nx || _ny != ny || _nz != nz) replan(real_data, complex_data, nx, ny, nz);
        
        //Normalize
        for(int i=0; i<fourier_size(); i++) 
        { 
          ((fftw_complex*)complex_data)[i][0]*=normalize_factor(); 
          ((fftw_complex*)complex_data)[i][1]*=-1*normalize_factor();
        }
        
        //Execute the plan
        fftw_execute_dft_c2r(_plan_c2r, complex_data, real_data);
        
    };
    
private:
    /*
     * If the size changes creates the plans again
     */
    void replan(double *real_data, fftw_complex *complex_data, int nx, int ny, int nz){
        fftw_destroy_plan(_plan_r2c);
        fftw_destroy_plan(_plan_c2r);
        
        _nx = nx;
        _ny = ny;
        _nz = nz;
        _plan_r2c = fftw_plan_dft_r2c_3d(nz, ny, nx, real_data, complex_data, FFTW_ESTIMATE);
        _plan_c2r = fftw_plan_dft_c2r_3d(nz, ny, nx, complex_data, real_data, FFTW_ESTIMATE);
    }
    
    /*
     * Returns the normalization factor
     */
    double normalize_factor(){
        if(_nx ==0 || _ny ==0 || _nz ==0 ) return 0;
        else return 1/sqrt(_nx*_ny*_nz);
    };
    
    /*
     * Returns the corresponding Fourier size
     */
    int fourier_size(){
        return (_nx/2+1)*_ny*_nz;
    };
    
    /*===================
     * Members
     ====================*/
    
    /*
     * Holds the plan for real to complex conversion
     */
    fftw_plan _plan_r2c;
    
    /*
     * Holds the plan for complex to real conversion
     */
    fftw_plan _plan_c2r;
    
    /*
     * Holds the size of the transform
     */
    int _nx, _ny, _nz;
    
};

#endif	/* FOURIERTRANSFORM_HPP */

