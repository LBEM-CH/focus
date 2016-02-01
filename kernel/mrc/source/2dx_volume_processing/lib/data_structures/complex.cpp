/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "complex.hpp"


namespace ds = tdx::data;

ds::Complex::Complex()
{
    initialize(0, 0);
}

ds::Complex::Complex(double real, double imag)
{
    initialize(real, imag);
}

void ds::Complex::initialize(double real, double imag)
{
    set_real(real);
    set_imag(imag);
}

ds::Complex& ds::Complex::operator =(const ds::Complex& rhs)
{
    initialize(rhs.real(), rhs.imag());
    return *this;
}

ds::Complex ds::Complex::operator +(const ds::Complex& rhs)
{
    return ds::Complex(this->real()+rhs.real(), this->imag()+rhs.imag());
}

ds::Complex ds::Complex::operator *(double factor)
{
    return ds::Complex(this->real()*factor, this->imag()*factor);
}

ds::Complex ds::Complex::operator *(const Complex& other)
{
    double a1 = this->real();
    double b1 = this->imag();
    double a2 = other.real();
    double b2 = other.imag();
    return ds::Complex(a1*a2 - b1*b2, a1*b2 + b1*a2);
}

bool ds::Complex::operator <(const ds::Complex& rhs) const
{
    return (this->amplitude() < rhs.amplitude());
}

bool ds::Complex::operator ==(const Complex& rhs) const
{
    return (real()==rhs.real() && imag()==rhs.imag());
}

double ds::Complex::real() const
{
    return _real;
}

double ds::Complex::imag() const
{
    return _imag;
}

double ds::Complex::amplitude() const
{
    return std::abs(std::complex<double>(real(), imag()));
}

double ds::Complex::phase() const
{
    return std::arg(std::complex<double>(real(), imag()));
}

double ds::Complex::intensity() const
{
    return amplitude()*amplitude();
}

void ds::Complex::set_real(double real)
{
    this->_real = real;
}

void ds::Complex::set_imag(double imag)
{
    this->_imag = imag;
}

void ds::Complex::set_amplitude(double amplitude)
{
    double current_amplitude = this->amplitude();
    double current_real = this->real();
    double current_imag = this->imag();
    double scale = 0.0;
    if(current_amplitude != 0.0) scale = amplitude/current_amplitude;
    this->set_real(current_real*(scale));
    this->set_imag(current_imag*(scale));
}

void ds::Complex::set_phase(double phase)
{
    double current_amplitude = this->amplitude();
    this->set_real(current_amplitude*cos(phase));
    this->set_imag(current_amplitude*sin(phase));
}

ds::Complex ds::Complex::conjugate()
{
    return Complex(real(), -1*imag());
}