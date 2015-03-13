#include <iomanip>

#include "../../include/FourierTransform.hpp"

void FourierTransform::real_to_complex(double *real_data, fftw_complex *complex_data, int nx, int ny, int nz){
    //Get the plan
    fftw_plan plan = fftw_plan_dft_r2c_3d(nz, ny, nx, real_data, complex_data, FFTW_ESTIMATE);
    
    //Execute the plan
    fftw_execute_dft_r2c(plan, real_data, complex_data );
    
    //Destroy the plan
    fftw_destroy_plan(plan);
}

void FourierTransform::complex_to_real(fftw_complex *complex_data, double *real_data, int nx, int ny, int nz){
    //Get the plan
    fftw_plan plan = fftw_plan_dft_c2r_3d(nz, ny, nx, complex_data, real_data, FFTW_ESTIMATE);
    
    //Execute the plan
    fftw_execute_dft_c2r(plan, complex_data, real_data);
    
    //Destroy the plan
    fftw_destroy_plan(plan);
}

VolumeReal FourierTransform::fft(VolumeFourier fourierVolume) {
    //Get the complex array
    std::cout <<"Transforming Fourier to real\n";
    fftw_complex* complex_data = fourierVolume.to_fftw_complex();
    
    double* real_data;
    int nx = fourierVolume.nx();
    int ny = fourierVolume.ny();
    int nz = fourierVolume.nz();
    int real_size = nx * ny * nz;
    int fourier_size = (nx/2+1) * ny * nz;
    
    //Normalize
    double factor = 1/sqrt(real_size);
    //std::cout <<"Read in complex data\n";
    for(int i=0;i<fourier_size;i++) 
    { 
      ((fftw_complex*)complex_data)[i][0]*=factor; 
      ((fftw_complex*)complex_data)[i][1]*=-1*factor; 
      //std::cout << "(" << std::setw(10) << ((fftw_complex*)complex_data)[i][0] << "," << std::setw(10) << ((fftw_complex*)complex_data)[i][1] <<") ";
      //if((i+1)%10 == 0) std::cout << std::endl;
    }
    std::cout << std::endl;
    //Generate the double  real array
    real_data = (double*) malloc(real_size*sizeof(double));
    
    // Do the Fourier Transform
    complex_to_real(complex_data, real_data, nx, ny, nz);
    
    /*
    std::cout << "Transformed to following real data:\n";
    for(int i=0;i<real_size;i++) 
    {
        std::cout << std::setw(12)<< real_data[i] << "\t";
        if((i+1)%10 == 0) std::cout << std::endl;
    }
    std::cout << std::endl;
    */
    
    //generate the Real Volume using the real_data
    VolumeHeader header(fourierVolume.get_header());
    VolumeReal real_volume = VolumeReal(header);
    real_volume.array_data(real_data);
    
    fftw_free(real_data); 
    fftw_free(complex_data);
    
    return real_volume;
    
}


VolumeFourier FourierTransform::fft(VolumeReal realVolume){
    std::cout << "Transforming real to Fourier\n";
    //Get the real array
    double* real_data = realVolume.array_data();
    
    //Generate the complex array
    fftw_complex* complex_data;
    int nx = realVolume.nx();
    int ny = realVolume.ny();
    int nz = realVolume.nz();
    int real_size = nx*ny*nz;
    int fourier_size = (nx/2+1) * ny * nz;
    complex_data = (fftw_complex*) malloc(fourier_size*sizeof(fftw_complex));
    
    /*
    std::cout << "Read in real data\n";
    for(int i=0;i<real_size;i++) 
    {
        std::cout << std::setw(12) << real_data[i] << "\t";
        if((i+1)%10 == 0) std::cout << std::endl;
    }
    */
    
    std::cout << std::endl;
    // Do the Fourier Transform
    real_to_complex(real_data, complex_data, nx, ny, nz);
    
    //Normalize
    //std::cout <<"Transformed to following complex data\n";
    double factor = 1/sqrt(real_size);
    for(int i=0;i<fourier_size;i++) 
    { 
      ((fftw_complex*)complex_data)[i][0]*=factor; 
      ((fftw_complex*)complex_data)[i][1]*=-1*factor;
      //std::cout << "(" << std::setw(10) << ((fftw_complex*)complex_data)[i][0] << "," << std::setw(10) << ((fftw_complex*)complex_data)[i][1] <<") ";
      //if((i+1)%10 == 0) std::cout << std::endl;
    }
    std::cout << std::endl;
    //Generate the Fourier volume using the complex_data
    VolumeHeader header(realVolume.get_header());
    VolumeFourier fourier_volume = VolumeFourier(header);
    fourier_volume.from_fftw_complex(complex_data);
    
    fftw_free(real_data); 
    fftw_free(complex_data);
    
    return fourier_volume;
}

