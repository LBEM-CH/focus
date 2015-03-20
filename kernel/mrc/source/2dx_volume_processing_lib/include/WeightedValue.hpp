/* 
 * File:   WeightedValue.hpp
 * Author: Nikhil Biyani (nikhilbiyani@gmail.com)
 *
 * Created on March 2, 2015, 5:15 PM
 */

#ifndef WEIGHTEDVALUE_HPP
#define	WEIGHTEDVALUE_HPP

#include <iostream>

#include "Complex2dx.hpp"

/*
 * A class to store a complex value with a weight.
 */
class WeightedComplex{
    
public:
    /*===================
     * Constructors
     ====================*/
    /*
     * Default constructor
     */
    WeightedComplex(){
        //std::cout << "In default initializer\n";
        initialize(Complex2dx(), 1.0);
    };
    
    /*
     * Constructor initializing a value and a weight
     */
    WeightedComplex(Complex2dx value, double weight){
        //std::cout << "In initializer\n";
        initialize(value, weight);
    };
    
    
    /*===================
     * Operators
     ====================*/
    /*
     * Operator overloading of = 
     * Equates the members of the class with those of rhs
     */
    WeightedComplex& operator=(const WeightedComplex& rhs){
        this->setValue(rhs.getValue());
        this->setWeight(rhs.getWeight());
        return *this;
    };
    
    /*
     * Operator overloading of +
     * Sums the values and averages the weights
     */
    WeightedComplex operator+(const WeightedComplex& rhs){
        return WeightedComplex(this->getValue()+rhs.getValue(), (this->getWeight()+rhs.getWeight())/2);
    };
    
    /*
     * Operator overloading of / with a double factor
     * Divides the value with factor and does nothing for weight
     */
    WeightedComplex operator*(const double factor){
        return WeightedComplex(this->getValue()*(1/factor), this->getWeight());
    };
    
    /*
     * Operator overloading of ==
     * Checks for an equal value with equal weight
     */
    bool operator==(const WeightedComplex& rhs) const{
        return ( (this->getValue() == rhs.getValue()) && (this->getWeight() == rhs.getWeight()) );
    };
    
    /*
     * Operator overloading of <
     * Solves the sorting purpose.
     * First compares the values followed by weights
     */
    bool operator<(const WeightedComplex& rhs) const{
        return (this->getValue() < rhs.getValue());
    };
    
    /*
     * Operator overloading of <<
     * Outputs the members in following style:
     * value (weight) 
     
    std::ostream& operator<<(std::ostream& os){
        os << this->getValue() << " (" << this->getWeight() << ")";
        return os;
    };
    */
    
    /*===================
     * Getters and setters
     ====================*/
    /*
     * Default setter for the value
     */
    void setValue(Complex2dx value){this->_value = value;};
    
    /*
     * Default setter for the weight
     */
    void setWeight(double weight){this->_weight=weight;};
    
    /*
     * Default getter of the value
     */
    Complex2dx getValue() const {return _value;};
    
    /*
     * Default getter of weight
     */
    double getWeight() const {return _weight;};
    
private:
            
    void initialize(Complex2dx value, double weight){
        this->_value = value;
        this->_weight = weight;
    };
            
    /*===================
     * Members
     ====================*/
    Complex2dx _value;
    double _weight;
        
};

#endif	/* WEIGHTEDVALUE_HPP */

