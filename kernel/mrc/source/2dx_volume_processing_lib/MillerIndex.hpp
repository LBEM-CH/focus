/* 
 * File:   MillerIndex.hpp
 * Author: biyanin
 *
 * Created on January 13, 2015, 2:58 PM
 */

#ifndef MILLERINDEX_HPP
#define	MILLERINDEX_HPP

class MillerIndex{
        
public:
    int h, k, l;
    
public:
    
    //Constructors
    MillerIndex(){};
    MillerIndex(int, int, int);
    MillerIndex(const MillerIndex&);
    ~MillerIndex(){};
    
    //Operators
    MillerIndex& operator=(const MillerIndex& rhs);
    int operator==(const MillerIndex& rhs) const;
    int operator<(const MillerIndex& rhs) const;
    
    //Getters
    int getH() const;
    int getK() const;
    int getL() const;
    
    //Setters
    void setH(int);
    void setK(int);
    void setL(int);

    friend std::ostream& operator<<(std::ostream&, const MillerIndex&);

    MillerIndex* getFriedelSpot();
};

#endif	/* MILLERINDEX_HPP */

