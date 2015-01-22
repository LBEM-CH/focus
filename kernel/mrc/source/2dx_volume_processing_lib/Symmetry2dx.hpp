/* 
 * File:   Symmetry.hpp
 * Author: biyanin
 *
 * Created on January 14, 2015, 3:31 PM
 */

#ifndef SYMMETRY_HPP
#define	SYMMETRY_HPP

#include <string>

#include "CommonDefinitions.hpp"

class Symmetry2dx{
private:
    enum SymmetryName{ 
        P1=0, 
        P2, 
        P12, 
        P121, 
        C12, 
        P222, 
        P2221, 
        P22121, 
        C222, 
        P4, 
        P422, 
        P4212, 
        P3, 
        P312, 
        P321, 
        P6, 
        P622};
        
public:
    SymmetryName name;

    Symmetry2dx(std::string symmetry);
    ~Symmetry2dx(){};
    
    
    int getCCP4Index();
    
    friend std::istream& operator<<(std::istream& is, Symmetry2dx& symmetry);
    friend std::ostream& operator>>(std::ostream& os, Symmetry2dx& symmetry);
    

};
#endif	/* SYMMETRY_HPP */

