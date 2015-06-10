/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "fourier_utilities.hpp"


double volume::utilities::fourier_utilities::GetResolution
    (const volume::data::MillerIndex& index, double gamma, double a, double b, double c)
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
    
    double resolution_x_f = index.h() * ux;
    double resolution_y_f = index.k() * vy;
    double resolution_z_f = index.k() * vz + index.l() * wz;
    
    //Infinite resolution
    double resolution_x = 10000;
    double resolution_y = 10000;
    double resolution_z = 10000;
    
    if(resolution_x_f > 0.00001 || resolution_x_f < -0.00001 ) resolution_x = 1/resolution_x_f;
    if(resolution_y_f > 0.00001 || resolution_y_f < -0.00001 ) resolution_y = 1/resolution_y_f;
    if(resolution_z_f > 0.00001 || resolution_z_f < -0.00001 ) resolution_z = 1/resolution_z_f;
    
    double final_resolution = sqrt(resolution_x*resolution_x + resolution_y*resolution_y + resolution_z*resolution_z);
    
    //Default resolution
    double result = final_resolution;
    
    
    //Sanity Check
    if(result > 100000 || result < 2)
    {
        std::cerr << "WARNING: Resolution of spot (" << index.h() << ", " << index.k() << ", " << index.l() << ") = " << result << " exceeds limits\n";
        //std::cout << resolution_x << " " << resolution_y << " " << resolution_z << "\n";
    }
    
    return result;
}
