/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "miller_index.hpp"


namespace ds = volume::data;

ds::MillerIndex::MillerIndex()
{
    initialize(0, 0, 0);
}

ds::MillerIndex::MillerIndex(int h, int k, int l)
{
    initialize(h, k, l);
}

ds::MillerIndex::MillerIndex(const MillerIndex& copy)
{
    initialize(copy.h(), copy.k(), copy.l());
}

void ds::MillerIndex::initialize(int h, int k, int l)
{
    _h = h;
    _k = k;
    _l = l;
}

ds::MillerIndex& ds::MillerIndex::operator =(const MillerIndex& rhs)
{
    this->initialize(rhs.h(), rhs.k(), rhs.l());
    return *this;
}

bool ds::MillerIndex::operator ==(const MillerIndex& rhs) const
{
    return (this->h() == rhs.h() && this->k()==rhs.k() && this->l() == rhs.l());
}

bool ds::MillerIndex::operator <(const MillerIndex& rhs) const
{
    return( (this->h() == rhs.h() && this->k() == rhs.k() && this->l() < rhs.l()) || 
            (this->h() == rhs.h() && this->k() < rhs.k()) || 
            (this->h() < rhs.h())
          );
}

std::ostream& ds::MillerIndex::operator <<(std::ostream& os) const
{
    return os << h() << " " << k() << " " << l();
}

void ds::MillerIndex::set_values(int h, int k, int l)
{
    this->initialize(h, k, l);
}

int ds::MillerIndex::h() const
{
    return _h;
}

int ds::MillerIndex::k() const
{
    return _k;
}

int ds::MillerIndex::l() const
{
    return _l;
}

std::string ds::MillerIndex::to_string() const
{
    std::string result = "(" + std::to_string(h()) + ", " + std::to_string(k()) + ", " + std::to_string(l()) +")";
    return result;
}

ds::MillerIndex ds::MillerIndex::FriedelSpot() const
{
    return MillerIndex(-1*h(), -1*k(), -1*l());
}