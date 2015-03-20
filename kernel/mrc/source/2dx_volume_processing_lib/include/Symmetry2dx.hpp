/* 
 * File:   Symmetry.hpp
 * Author: biyanin
 *
 * Created on January 14, 2015, 3:31 PM
 */

#ifndef SYMMETRY_HPP
#define	SYMMETRY_HPP

#include <string>

/*
 * Class to hold in the data and perform basic operations on 2D crystallographic symmetries
 */
class Symmetry2dx{
public:
    /*============================
     * Constructors
     *===========================*/
    
    /*
     * Default constructor initializing symmetry to P1
     */
    Symmetry2dx();
    
    /*
     * Constructor with string symmetry
     */
    Symmetry2dx(std::string symmetry);
    
    /*============================
     * Methods
     *===========================*/
    
    /*
     * Sets the symmetry with input string
     */
    void setSymmetry(std::string symmetry);
    
    /*
     * Returns the string of the corresponding symmetry
     */
    std::string getSymmetryString() const;
    
    /*
     * Returns the CCP4 symmetry index of the corresponding symmetry
     */
    int getCCP4Index() const;
    
    /*
     * Returns the index of the enum SymmetryName
     */
    int getSymmetryIndex() const;
    
    /*============================
     * Operators
     *===========================*/
    friend std::istream& operator<<(std::istream& is, Symmetry2dx& symmetry);
    
private:
    /*
     * Possible 2D crystallographic symmetries
     */
    enum SymmetryName
    { 
        P1=0, P2, P12, P121, C12, P222, P2221, P22121, C222, P4, P422, P4212, P3, P312, P321, P6, P622
    };
        
    SymmetryName name;
    
    /*
     * Initializer with a string
     */
    void initialize(std::string symmetry);
    
};
#endif	/* SYMMETRY_HPP */

