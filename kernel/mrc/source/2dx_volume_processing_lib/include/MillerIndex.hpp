/* 
 * File:   MillerIndex.hpp
 * Author: Nikhil Biyani (nikhilbiyani@gmail.com)
 *
 * Created on January 13, 2015, 2:58 PM
 */

#ifndef MILLERINDEX_HPP
#define	MILLERINDEX_HPP

#include "Triplet.hpp"

/*
 * Subclass of Triplet.
 * Has additional methods to use it as Miller Index
 */
class MillerIndex : public Triplet2dx{
        
public:
    /*===================
     * Constructors
     ====================*/
    /*
     * Default constructor
     */
    MillerIndex();
    
    /*
     * Constructor with three integers
     */
    MillerIndex(int h, int k, int l);
    
    /*
     * Constructor for Triplet to MillerIndex conversion
     */
    MillerIndex(const Triplet2dx& triplet);
    
    /*
     * Constructor from an ArrayIndex and Max Miller index
     */
    MillerIndex(const Triplet2dx& arrayIndex, const Triplet2dx size, const MillerIndex& maxMiller);
    
    
    /*===================
     * Additional Methods
     ====================*/
    /*
     * Method to get the Friedel spot of index h, k, l
     */
    MillerIndex getFriedelSpot() const;
    
    /*
     * Returns the Miller Index H
     */
    int h() const;
    
    /*
     * Returns the Miller Index K
     */
    int k() const;
    
    /*
     * Returns the Miller Index L
     */
    int l() const;
    
    /*
     * Sets the miller index h, k, l
     */
    void set_values(int h, int k, int l);
    
    /*
     * Returns the array index corresponding to the present miller index
     */
    Triplet2dx to_array_index(const Triplet2dx size) const;
    
};

#endif	/* MILLERINDEX_HPP */

