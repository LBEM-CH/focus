/*
 * Implementation of the library:
 * <Complex2dx.hpp>
 * 
 * Author: Nikhil Biyani (nikhilbiyani@gmail.com)
 */

#include "../../include/Complex2dx.hpp"

Complex2dx::Complex2dx() {
    initialize(0, 0);
}

Complex2dx::Complex2dx(double real, double imag) {
    initialize(real, imag);
}

Complex2dx& Complex2dx::operator=(const Complex2dx& rhs) {
    this->real(rhs.real());
    this->imag(rhs.imag());
    return *this;
}

Complex2dx Complex2dx::operator+(const Complex2dx& rhs) {
    //std::cout << "(" <<  this->real() << "," << this->imag() << ")+";
    //std::cout << "(" <<  rhs.real() << "," << rhs.imag() << ")=";
    Complex2dx result = Complex2dx(this->real()+rhs.real(), this->imag()+rhs.imag());
    //std::cout << "(" <<  result.real() << "," << result.imag() << ")\n";
    return result;
}

Complex2dx Complex2dx::operator*(double factor) {
    return Complex2dx(this->real()*factor, this->imag()*factor);
}


bool Complex2dx::operator<(const Complex2dx& rhs) const {
    bool result = (this->getAmplitude() < rhs.getAmplitude());
    return result;
}

bool Complex2dx::operator==(const Complex2dx& rhs) const {
    return(real()==rhs.real() && imag()==rhs.imag());
}


std::ostream& Complex2dx::operator<<(std::ostream& os) {
    os << "(" << real() << ", " << imag() << ")";
    return os;
}

double Complex2dx::getAmplitude() const{
    return std::abs(std::complex<double>(real(), imag()));
}

double Complex2dx::getPhase() const{
    return std::arg(std::complex<double>(real(), imag()));
}

void Complex2dx::setAmplitude(double amplitude) {
    double current_amplitude = this->getAmplitude();
    this->real(amplitude/current_amplitude);
    this->imag(amplitude/current_amplitude);
}

void Complex2dx::setPhase(double phase_in_radians) {
    double current_amplitude = this->getAmplitude();
    this->real(current_amplitude*cos(phase_in_radians));
    this->imag(current_amplitude*sin(phase_in_radians));
    //std::cout << "Requested " << correctPhase(phase_in_radians) << " turned to " << getPhase() << "\n";
}

void Complex2dx::initialize(double real, double imag){
    _real = real;
    _imag = imag;
}
