/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "complex.hpp"

using namespace tdx;

Complex::Complex()
{
    initialize(0, 0);
}

Complex::Complex(double real, double imag)
{
    initialize(real, imag);
}

void Complex::initialize(double real, double imag)
{
    set_real(real);
    set_imag(imag);
}

Complex& Complex::operator =(const Complex& rhs)
{
    initialize(rhs.real(), rhs.imag());
    return *this;
}

Complex Complex::operator +(const Complex& rhs)
{
    return Complex(this->real()+rhs.real(), this->imag()+rhs.imag());
}

Complex Complex::operator *(double factor)
{
    return Complex(this->real()*factor, this->imag()*factor);
}

Complex Complex::operator *(const Complex& other)
{
    double a1 = this->real();
    double b1 = this->imag();
    double a2 = other.real();
    double b2 = other.imag();
    return Complex(a1*a2 - b1*b2, a1*b2 + b1*a2);
}

bool Complex::operator <(const Complex& rhs) const
{
    return (this->amplitude() < rhs.amplitude());
}

bool Complex::operator ==(const Complex& rhs) const
{
    return (real()==rhs.real() && imag()==rhs.imag());
}

double Complex::real() const
{
    return _real;
}

double Complex::imag() const
{
    return _imag;
}

double Complex::amplitude() const
{
    return std::abs(std::complex<double>(real(), imag()));
}

double Complex::phase() const
{
    return std::arg(std::complex<double>(real(), imag()));
}

double Complex::intensity() const
{
    return amplitude()*amplitude();
}

void Complex::set_real(double real)
{
    this->_real = real;
}

void Complex::set_imag(double imag)
{
    this->_imag = imag;
}

void Complex::set_amplitude(double amplitude)
{
    double current_amplitude = this->amplitude();
    double current_real = this->real();
    double current_imag = this->imag();
    double scale = 0.0;
    if(current_amplitude != 0.0) scale = amplitude/current_amplitude;
    this->set_real(current_real*(scale));
    this->set_imag(current_imag*(scale));
}

void Complex::set_phase(double phase)
{
    double current_amplitude = this->amplitude();
    this->set_real(current_amplitude*cos(phase));
    this->set_imag(current_amplitude*sin(phase));
}

Complex Complex::conjugate()
{
    return Complex(real(), -1*imag());
}