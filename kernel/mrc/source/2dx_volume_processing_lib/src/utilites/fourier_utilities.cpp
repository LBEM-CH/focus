/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "fourier_utilities.hpp"


double volume_processing_2dx::utilities::fourier_utilities::GetResolution
    (const volume_processing_2dx::data_structures::MillerIndex& index, double gamma, double a, double b, double c)
{
    
    if(a==0 || b==0 || c==0 || gamma==0)
    {
        std::cerr << "ERROR! Encountered 0 in cell dimensions while calculating resolution.";
        return 0;
    }
    
    //Calculate the reciprocal lattice
    double factor = 1;
    //double factor = 2*M_PI;
    double ux = factor/a;
    double vy = factor/b;
    double vz = factor*cos(gamma)/(sin(gamma)*b);
    double wz = factor/(c*sin(gamma));
    
    double resolution_x = index.h() * ux;
    double resolution_y = index.k() * vy;
    double resolution_z = index.k() * vz + index.l() * wz;
     
    double final_resolution = sqrt(resolution_x*resolution_x + resolution_y*resolution_y + resolution_z*resolution_z);
    
    //Default resolution
    double result = 1000;
    if(final_resolution != 0)
    {
        result = 1/final_resolution;
    }
    //std::cout << "Resolution of spot (" << index.h() << ", " << index.k() << ", " << index.l() << ") = " << result << "\n";
    
    return result;
}
