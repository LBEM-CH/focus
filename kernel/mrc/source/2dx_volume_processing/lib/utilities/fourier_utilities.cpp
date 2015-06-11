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
    
    double resolution_f = sqrt(resolution_x_f*resolution_x_f + resolution_y_f*resolution_y_f + resolution_z_f*resolution_z_f);
    
    //Infinite resolution
    double final_resolution = 100000;
    if(resolution_f != 0)
    {
        final_resolution = 1/resolution_f;
    }
    
    //Sanity Check
    //if(final_resolution < 12.0 || final_resolution > 100000)
    //{
    //    std::cerr << "WARNING: Resolution of spot (" << index.h() << ", " << index.k() << ", " << index.l() << ") = " << final_resolution << " exceeds limits\n";
    //    std::cout << resolution_x_f << " " << resolution_y_f << " " << resolution_z_f << "\n";
    //}
    
    return final_resolution;
}
