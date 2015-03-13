/*
 * Implementation of the library:
 * <MillerIndex.hpp>
 * 
 * Author: Nikhil Biyani (nikhilbiyani@gmail.com)
 */

#include "../../include/MillerIndex.hpp"

MillerIndex::MillerIndex() : Triplet2dx(){}

MillerIndex::MillerIndex(int h, int k, int l) : Triplet2dx(h, k, l){}

MillerIndex::MillerIndex(const Triplet2dx& triplet) : Triplet2dx(triplet){}

MillerIndex::MillerIndex(const Triplet2dx& arrayIndex, const Triplet2dx size, const MillerIndex& maxMiller) {
    Triplet2dx newTriplet(arrayIndex);
    if(newTriplet.y()>maxMiller.k()) {
        int temp_y = newTriplet.y();
        newTriplet.y(temp_y-size.y());
    }
    if(newTriplet.z()>maxMiller.l()){
        int temp_z = newTriplet.z();
        newTriplet.z(temp_z-size.z());
    }
    set_values(newTriplet.x(), newTriplet.y(), newTriplet.z());
    //std::cout << "Input Memory: " << arrayIndex.x() << ", " << arrayIndex.y() << ", " << arrayIndex.z()
    //        <<" Output Miller: " << h() << ", " << k() << ", " << l() << "\n";
}

int MillerIndex::h() const{
    return _x;
}

int MillerIndex::k() const{
    return _y;
}

int MillerIndex::l() const{
    return _z;
}

void MillerIndex::set_values(int h, int k, int l) {
    _x = h;
    _y = k;
    _z = l;
}

MillerIndex MillerIndex::getFriedelSpot() const{
    return MillerIndex(-1*x(), -1*y(), -1*z());
}

Triplet2dx MillerIndex::to_array_index(const Triplet2dx size) const{
    Triplet2dx newTriplet(h(), k(), l());
    if(newTriplet.z() < 0) newTriplet.z(newTriplet.z()+size.z());
    if(newTriplet.y() < 0) newTriplet.y(newTriplet.y()+size.y());
    
    //std::cout << "Input Miller: " << h() << ", " << k() << ", " << l()
    //        <<" Output Memory: " << newTriplet.x() << ", " << newTriplet.y() << ", " << newTriplet.z() << "\n";
    
    return newTriplet;
}
