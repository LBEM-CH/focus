/* 
 * File:   Triplet.hpp
 * Author: Nikhil Biyani (nikhilbiyani@gmail.com)
 *
 * Created on March 2, 2015, 5:31 PM
 */

#ifndef TRIPLET2dX_HPP
#define	TRIPLET2dX_HPP

#include <iostream>

/*
 * An abstract class to define triplet of integers.
 */
class Triplet2dx{

public:    
    
    /*===================
     * Constructors
     ====================*/
    /*
     * Default constructor
     */
    Triplet2dx(){
        //Initialize the indices by 0
        initialize(0, 0, 0);
    };
    
    /*
     * Constructor with three integers
     */
    Triplet2dx(int x, int y, int z){
        initialize(x, y, z);
    };
    
    /*
     * Copy Constructor
     */
    Triplet2dx(const Triplet2dx& copy){
        initialize(copy._x, copy._y, copy._z);
    };
    
    /*===================
     * Operators
     ====================*/
    /*
     * Initializes the members with those of rhs
     */
    Triplet2dx& operator=(const Triplet2dx& rhs){
        initialize(rhs._x, rhs._y, rhs._z);
        return *this;
    };
    
    /*
     * Checks an equality of members with that of rhs
     */
    bool operator==(const Triplet2dx& rhs) const{
        return (this->_x == rhs._x) && (this->_y == rhs._y) && (this->_z == rhs._z);
    };
    
    /*
     * First sorts x followed by y and z respectively
     */
    bool operator<(const Triplet2dx& rhs) const{
        return (this->_x == rhs._x && this->_y == rhs._y && this->_z < rhs._z) || (this->_x == rhs._x && this->_y < rhs._y) || (this->_x < rhs._x);
    };
    
    /*
     * Outputs in the format:
     * x y z
     */
    std::ostream& operator<<(std::ostream& output) const{
        return output << x() << " " << y() << " " << z();
    };
    
    /*===================
     * Getters and Setters
     ====================*/
    int x() const {return _x;};
    int y() const {return _y;};
    int z() const {return _z;};
    
    void x(int x){_x = x;};
    void y(int y){_y = y;};
    void z(int z){_z = z;};
    
    /*===================
     * Additional methods
     ====================*/
    /*
     * Given a continuous array of a size nx, ny, nz, returns the index where this triplet should be placed
     * Formula: z + (y*nz) + (x*ny*nz)
     */
    int get_array_id(Triplet2dx size) const{
        return (x() + (y()*size.x()) + (z()*size.y()*size.x()) );
    };
    
    int get_self_muliplication() const{
        return x()*y()*z();
    };
    
private:
    void initialize(int x, int y, int z){
        this->_x = x;
        this->_y = y;
        this->_z = z;
    };
    
protected: 
    /*===================
     * Members
     ====================*/
    int _x, _y, _z;
    
};


#endif	/* TRIPLET2dx_HPP */