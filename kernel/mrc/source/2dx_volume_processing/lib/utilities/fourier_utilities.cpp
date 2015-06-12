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
     
    //std::cerr << "::GetResolution: a,b,c,gamma = "<<a<<","<<b<<","<<c<<",  "<<gamma<<"\n";
    
    double PI = 3.14159265437;
    
    double astar = 1.0 / ( a * sin(gamma));
    double bstar = 1.0 / ( b * sin(gamma));
    double cstar = 1.0 / c;
    
    double recgamma = PI - gamma;
    
    double dstar_sq = 
          ( pow(index.h()*astar,2) 
          + 2*index.h()*index.k()*astar*bstar*cos(recgamma)
          + pow(index.k()*bstar,2)
          + pow(index.l()*cstar,2) );
        
    double final_resolution = 1.0 / sqrt(dstar_sq);
        
    //std::cout << ":: h,k,l = " << index.h() <<","<< index.k()<<","<<index.l()<<", RES=" << final_resolution << "\n";
    
    return final_resolution;
}
