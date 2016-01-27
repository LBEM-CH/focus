/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "fourier_transform_fftw.hpp"

namespace ft = tdx::transforms;

ft::FourierTransformFFTW::FourierTransformFFTW()
{
    _nx = _ny = _nz = 0;
    _plan_r2c = NULL;
    _plan_c2r = NULL;
    _plans_initialized = false;
}

ft::FourierTransformFFTW::FourierTransformFFTW(const FourierTransformFFTW& copy)
{
    _nx = copy._nx;
    _ny = copy._ny;
    _nz = copy._nz;
    _plans_initialized = copy._plans_initialized;
    if(_plans_initialized)
    {   
        _plan_r2c = new fftw_plan(*(copy._plan_r2c));
        _plan_c2r = new fftw_plan(*(copy._plan_c2r));
    }
    else
    {
        _plan_r2c = NULL;
        _plan_c2r = NULL;
    }
}

ft::FourierTransformFFTW::~FourierTransformFFTW()
{
    if(_plans_initialized)
    {
        //fftw_destroy_plan(*_plan_r2c);
        //fftw_destroy_plan(*_plan_c2r);
        delete _plan_r2c;
        delete _plan_c2r;
    }
}

void ft::FourierTransformFFTW::reset(const FourierTransformFFTW& copy)
{
    _nx = copy._nx;
    _ny = copy._ny;
    _nz = copy._nz;
    if(_plans_initialized)
    {
        //fftw_destroy_plan(*_plan_r2c);
        //fftw_destroy_plan(*_plan_c2r);
        delete _plan_r2c;
        delete _plan_c2r;
    }
    
    _plans_initialized = copy._plans_initialized;
    if(_plans_initialized)
    {   
        _plan_r2c = new fftw_plan(*(copy._plan_r2c));
        _plan_c2r = new fftw_plan(*(copy._plan_c2r));
    }
    else
    {
        _plan_r2c = NULL;
        _plan_c2r = NULL;
    }
}

ft::FourierTransformFFTW& ft::FourierTransformFFTW::operator=(const FourierTransformFFTW& rhs)
{
    reset(rhs);
    return *this;
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
    _plan_r2c = new fftw_plan(fftw_plan_dft_r2c_3d(nz, ny, nx, real_data, complex_data, FFTW_ESTIMATE));
    _plan_c2r = new fftw_plan(fftw_plan_dft_c2r_3d(nz, ny, nx, complex_data, real_data, FFTW_ESTIMATE));
    _plans_initialized = true;
    std::cout << "Created new plans\n";
}

void ft::FourierTransformFFTW::RealToComplex(int nx, int ny, int nz, double* real_data, fftw_complex* complex_data)
{   
    //Re-plan if required
    if(_nx != nx || _ny != ny || _nz != nz){
        this->Replan(real_data, complex_data, nx, ny, nz);
    }

    //Execute the plan
    fftw_execute_dft_r2c(*_plan_r2c, real_data, complex_data );

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
    
    if(_plan_r2c == NULL || _plan_c2r == NULL) Replan(real_data, complex_data, nx, ny, nz);

    double factor = this->NormalizationFactor();
    //Normalize
    for(int i=0; i < this->FourierSize(); i++) 
    { 
      ((fftw_complex*)complex_data)[i][0] *= factor; 
      ((fftw_complex*)complex_data)[i][1] *= -1*factor;
    }

    //Execute the plan
    fftw_execute_dft_c2r(*_plan_c2r, complex_data, real_data);
}