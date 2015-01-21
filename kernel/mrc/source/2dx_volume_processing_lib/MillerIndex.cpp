#include <iostream>
#include "MillerIndex.hpp"

MillerIndex::MillerIndex(int h, int k, int l) {
    this->h = h;
    this->k = k;
    this->l = l;
}

MillerIndex::MillerIndex(const MillerIndex& other) {
    this->h = other.h;
    this->k = other.k;
    this->l = other.l;
}

MillerIndex& MillerIndex::operator=(const MillerIndex& rhs) {
    this->h = rhs.h;
    this->k = rhs.k;
    this->l = rhs.l;
    return *this;
}

int MillerIndex::operator==(const MillerIndex& rhs) const{
    if( this->h != rhs.h) return 0;
    if( this->k != rhs.k) return 0;
    if( this->l != rhs.l) return 0;
    return 1;
}

int MillerIndex::operator<(const MillerIndex& rhs) const{
    if( this->h == rhs.h && this->k == rhs.k && this->l < rhs.l) return 1;
    if( this->h == rhs.h && this->k < rhs.k) return 1;
    if( this->h < rhs.h ) return 1;
    return 0;
}


std::ostream &operator<<(std::ostream& output, const MillerIndex& index){
    output << index.h << " " << index.k << " " << index.l;
   return output;
}


int MillerIndex::getH() const{
    return this->h;
}

int MillerIndex::getK() const{
    return this->k;
}

int MillerIndex::getL() const{
    return this->l;
}

void MillerIndex::setH(int h) {
    this->h = h;
}

void MillerIndex::setK(int k) {
    this->k = k;
}

void MillerIndex::setL(int l) {
    this->l = l;
}

MillerIndex* MillerIndex::getFriedelSpot() {
    return new MillerIndex(-1*this->h, -1*this->k, -1*this->l);
}
