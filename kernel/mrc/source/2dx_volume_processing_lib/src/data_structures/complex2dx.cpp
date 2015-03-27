/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "complex2dx.hpp"


namespace ds = volume_processing_2dx::data_structures;

ds::Complex2dx::Complex2dx()
{
    this->initialize(0, 0);
}

ds::Complex2dx::Complex2dx(double real, double imag)
{
    this->initialize(real, imag);
}

ds::Complex2dx::Complex2dx(const Complex2dx& copy)
{
    this->initialize(copy.real(), copy.imag());
}

void ds::Complex2dx::initialize(double real, double imag)
{
    this->set_real(real);
    this->set_imag(imag);
}

ds::Complex2dx& ds::Complex2dx::operator =(const ds::Complex2dx& rhs)
{
    this->initialize(rhs.real(), rhs.imag());
    return *this;
}

ds::Complex2dx ds::Complex2dx::operator +(const ds::Complex2dx& rhs)
{
    return ds::Complex2dx(this->real()+rhs.real(), this->imag()+rhs.imag());
}

ds::Complex2dx ds::Complex2dx::operator *(double factor)
{
    return ds::Complex2dx(this->real()*factor, this->imag()*factor);
}

bool ds::Complex2dx::operator <(const ds::Complex2dx& rhs) const
{
    return (this->amplitude() < rhs.amplitude());
}

bool ds::Complex2dx::operator ==(const Complex2dx& rhs) const
{
    return (real()==rhs.real() && imag()==rhs.imag());
}

double ds::Complex2dx::real() const
{
    return _real;
}

double ds::Complex2dx::imag() const
{
    return _imag;
}

double ds::Complex2dx::amplitude() const
{
    return std::abs(std::complex<double>(real(), imag()));
}

double ds::Complex2dx::phase() const
{
    return std::arg(std::complex<double>(real(), imag()));
}

void ds::Complex2dx::set_real(double real)
{
    this->_real = real;
}

void ds::Complex2dx::set_imag(double imag)
{
    this->_imag = imag;
}

void ds::Complex2dx::set_amplitude(double amplitude)
{
    double current_amplitude = this->amplitude();
    this->set_real(amplitude/current_amplitude);
    this->set_imag(amplitude/current_amplitude);
}

void ds::Complex2dx::set_phase(double phase)
{
    double current_amplitude = this->amplitude();
    this->set_real(current_amplitude*cos(phase));
    this->set_imag(current_amplitude*sin(phase));
}