#include <iostream>
#include <math.h>

#include "Reflection.hpp"

Reflection::Reflection(){
    this->value = std::complex<double>(0.0, 0.0);
    this->weight = 0.0;
}

Reflection::Reflection(std::complex<double> value, double weight) {
    this->value = value;
    this->weight = weight;
}

Reflection::Reflection(const Reflection& other) {
    this->value = other.value;
    this->weight = other.weight;
}

Reflection Reflection::operator=(const Reflection& rhs) {
    this->value = rhs.value;
    this->weight = rhs.weight;  
    return *this;
}

Reflection Reflection::operator+(const Reflection& rhs) {
    Reflection newRef = Reflection(this->value+rhs.value, this->weight+rhs.weight);
    return newRef;
}

Reflection Reflection::operator/(const double factor) {
    Reflection newRef = Reflection(Complex2dx(this->value.real()/factor, this->value.imag()/factor), this->weight/factor);
    return newRef;
}


int Reflection::operator==(const Reflection& rhs) const{
    if(this->value != rhs.value || this->weight != rhs.weight) return 0;
    return 1;
}

int Reflection::operator<(const Reflection& rhs) const{
    if(this->getAmplitude() == rhs.getAmplitude() && this->weight < rhs.weight) return 1;
    if(this->getAmplitude() < rhs.getAmplitude()) return 1;
    return 0;
}

std::ostream& operator<<(std::ostream& os, const Reflection& ref) {
    os << std::abs(ref.value) << " " << std::arg(ref.value)*180/M_PI << " " << ref.weight*100;
    
    return os;
}


std::complex<double> Reflection::getValue() const{
    return this->value;
}

void Reflection::setValue(std::complex<double> value) {
    this->value = value;
}

double Reflection::getAmplitude() const{
    return std::abs(this->value);
}

void Reflection::setAmplitude(double amplitude) {
    double current_amplitude = this->getAmplitude();
    this->value = (amplitude/current_amplitude)*this->value;
}

double Reflection::getPhase() const{
    return std::arg(this->value);
}

void Reflection::setPhase(double phase) {
    double current_amplitude = this->getAmplitude();
    double real = current_amplitude*cos(phase);
    double imag = current_amplitude*sin(phase);
    this->value = std::complex<double>(real, imag);
}

double Reflection::getWeight() const{
    return this->weight;
}

void Reflection::setWeight(double weight) {
    this->weight = weight;
}
