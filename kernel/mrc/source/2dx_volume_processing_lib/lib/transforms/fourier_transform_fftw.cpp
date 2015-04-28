/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "fourier_transform_fftw.hpp"

namespace ft = volume::transforms;

ft::FourierTransformFFTW::FourierTransformFFTW()
{
    _nx = _ny = _nz = 0;
}

ft::FourierTransformFFTW::~FourierTransformFFTW()
{
    fftw_destroy_plan(_plan_r2c);
    fftw_destroy_plan(_plan_c2r);
}

int ft::FourierTransformFFTW::FourierSize()
{
    return (_nx/2+1)*_ny*_nz;
}

double ft::FourierTransformFFTW::NormalizationFactor()
{
    if(_nx ==0 || _ny ==0 || _nz ==0 ) return 1;
    else return 1/sqrt(_nx*_ny*_nz);
}

void ft::FourierTransformFFTW::Replan(double* real_data, fftw_complex* complex_data, int nx, int ny, int nz)
{
    std::cout << "Re-planning.. ";
    _nx = nx;
    _ny = ny;
    _nz = nz;
    _plan_r2c = fftw_plan_dft_r2c_3d(nz, ny, nx, real_data, complex_data, FFTW_ESTIMATE);
    _plan_c2r = fftw_plan_dft_c2r_3d(nz, ny, nx, complex_data, real_data, FFTW_ESTIMATE);
    std::cout << "Created new plans\n";
}

void ft::FourierTransformFFTW::RealToComplex(int nx, int ny, int nz, double* real_data, fftw_complex* complex_data)
{   
    //Re-plan if required
    if(_nx != nx || _ny != ny || _nz != nz){
        this->Replan(real_data, complex_data, nx, ny, nz);
    }

    //Execute the plan
    fftw_execute_dft_r2c(_plan_r2c, real_data, complex_data );

    //Normalize
    double factor = this->NormalizationFactor();
    for(int i=0; i<this->FourierSize(); i++) 
    { 
      ((fftw_complex*)complex_data)[i][0] *= factor; 
      ((fftw_complex*)complex_data)[i][1] *= -1*factor;
    }

}

void ft::FourierTransformFFTW::ComplexToReal(int nx, int ny, int nz, fftw_complex* complex_data, double* real_data)
{
    //Re-plan if required
    if(_nx != nx || _ny != ny || _nz != nz) this->Replan(real_data, complex_data, nx, ny, nz);

    double factor = this->NormalizationFactor();
    //Normalize
    for(int i=0; i < this->FourierSize(); i++) 
    { 
      ((fftw_complex*)complex_data)[i][0] *= factor; 
      ((fftw_complex*)complex_data)[i][1] *= -1*factor;
    }

    //Execute the plan
    fftw_execute_dft_c2r(_plan_c2r, complex_data, real_data);
}