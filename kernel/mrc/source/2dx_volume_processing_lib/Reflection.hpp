/* 
 * File:   Reflection.hpp
 * Author: biyanin
 *
 * Created on January 13, 2015, 3:04 PM
 */

#ifndef REFLECTION_HPP
#define	REFLECTION_HPP

#include <complex>

/*
 * Complex numbers used in the software
 */
typedef std::complex<double> Complex2dx;

class Reflection{
 
public:
    Complex2dx value;
    double weight;
    
public:
    //Constructors and Destructor
    Reflection();
    Reflection(std::complex<double>, double);
    Reflection(const Reflection&);
    ~Reflection(){};
    
    //Getters
    std::complex<double> getValue() const;
    double getWeight() const;
    double getAmplitude() const;
    double getPhase() const;

    //Setters
    void setAmplitude(double);
    void setPhase(double);
    void setValue(std::complex<double>);
    void setWeight(double);
    
    //Operator overloading
    Reflection operator=(const Reflection& rhs);
    Reflection operator+(const Reflection& rhs);
    Reflection operator/(const double factor);
    int operator==(const Reflection&) const;
    int operator<(const Reflection&) const;
    friend std::ostream& operator<<(std::ostream&, const Reflection&);
    
};

#endif	/* REFLECTION_HPP */

