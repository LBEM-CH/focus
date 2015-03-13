/* 
 * File:   WeightedValue.hpp
 * Author: Nikhil Biyani (nikhilbiyani@gmail.com)
 *
 * Created on March 2, 2015, 5:15 PM
 */

#ifndef WEIGHTEDVALUE_HPP
#define	WEIGHTEDVALUE_HPP

#include <iostream>

/*
 * A class to store a value with a weight.
 */
template<class T>
class WeightedValue{
    
public:
    /*===================
     * Constructors
     ====================*/
    /*
     * Default constructor
     */
    WeightedValue(){
        //std::cout << "In default initializer\n";
        initialize(T(), 1.0);
    };
    
    /*
     * Constructor initializing a value and a weight
     */
    WeightedValue(T value, double weight){
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
    WeightedValue<T>& operator=(const WeightedValue<T>& rhs){
        this->setValue(rhs.getValue());
        this->setWeight(rhs.getWeight());
        return *this;
    };
    
    /*
     * Operator overloading of +
     * Sums the values and averages the weights
     */
    WeightedValue<T> operator+(const WeightedValue<T>& rhs){
        return WeightedValue<T>(this->getValue()+rhs.getValue(), (this->getWeight()+rhs.getWeight())/2);
    };
    
    /*
     * Operator overloading of / with a double factor
     * Divides the value with factor and does nothing for weight
     */
    WeightedValue<T> operator*(const double factor){
        return WeightedValue<T>(this->getValue()/factor, this->getWeight());
    };
    
    /*
     * Operator overloading of ==
     * Checks for an equal value with equal weight
     */
    bool operator==(const WeightedValue<T>& rhs) const{
        return ( (this.getValue() == rhs.getValue()) && (this->getWeight() == rhs.getWeight()) );
    };
    
    /*
     * Operator overloading of <
     * Solves the sorting purpose.
     * First compares the values followed by weights
     */
    bool operator<(const WeightedValue<T>& rhs) const{
        return (this->getValue() < rhs.getValue());
    };
    
    /*
     * Operator overloading of <<
     * Outputs the members in following style:
     * value (weight) 
     */
    std::ostream& operator<<(std::ostream& os){
        os << this->getValue() << " (" << this->getWeight() << ")";
        return os;
    };
    
    
    /*===================
     * Getters and setters
     ====================*/
    /*
     * Default setter for the value
     */
    void setValue(T value){this->_value = value;};
    
    /*
     * Default setter for the weight
     */
    void setWeight(double weight){this->_weight=weight;};
    
    /*
     * Default getter of the value
     */
    T getValue() const {return _value;};
    
    /*
     * Default getter of weight
     */
    double getWeight() const {return _weight;};
    
private:
            
    void initialize(T value, double weight){
        this->_value = value;
        this->_weight = weight;
    };
            
    /*===================
     * Members
     ====================*/
    T _value;
    double _weight;
        
};

#endif	/* WEIGHTEDVALUE_HPP */

