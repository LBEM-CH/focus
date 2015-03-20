#include <iostream>
#include <stdexcept>
#include <string>

#include "../../include/Symmetry2dx.hpp"

Symmetry2dx::Symmetry2dx() {
    initialize("P1");
}

Symmetry2dx::Symmetry2dx(std::string symmetry) {
    initialize(symmetry);
}

std::ostream& operator<<(std::ostream& os, Symmetry2dx& symmetry) {
    os << symmetry.getSymmetryString();
    return os;
}

void Symmetry2dx::setSymmetry(std::string symmetry) {
    initialize(symmetry);
}

int Symmetry2dx::getSymmetryIndex() const {
    return name;
}

std::string Symmetry2dx::getSymmetryString() const {
    std::string symName;
    
    switch(name){
        case 0: 
            symName = "P1";
            break;
        case 1: 
            symName = "P2";
            break;
        case 2: 
            symName = "P12";
            break;
        case 3: 
            symName = "P121";
            break;
        case 4: 
            symName = "C12";
            break;
        case 5: 
            symName =  "P222";
            break;
        case 6: 
            symName =  "P2221";
            break;
        case 7: 
            symName =  "P22121";
            break;
        case 8: 
            symName = "C222";
            break;
        case 9: 
            symName = "P4";
            break;
        case 10: 
            symName = "P422";
            break;
        case 11: 
            symName = "P4212";
            break;
        case 12: 
            symName = "P3";
            break;
        case 13: 
            symName = "P312";
            break;
        case 14: 
            symName = "P321";
            break;
        case 15: 
            symName = "P6";
            break;
        case 16: 
            symName = "P622";
            break;
    }
    
    return symName;
}


int Symmetry2dx::getCCP4Index() const{
    int index;
    
    switch(this->name){
        case 0: 
            index = 1;
            break;
        case 1: 
            index = 3;
            break;
        case 2: 
            index = 3;
            break;
        case 3: 
            index = 4;
            break;
        case 4: 
            index = 5;
            break;
        case 5: 
            index =  16;
            break;
        case 6: 
            index =  17;
            break;
        case 7: 
            index =  18;
            break;
        case 8: 
            index = 21;
            break;
        case 9: 
            index = 75;
            break;
        case 10: 
            index = 89;
            break;
        case 11: 
            index = 90;
            break;
        case 12: 
            index = 143;
            break;
        case 13: 
            index = 149;
            break;
        case 14: 
            index = 150;
            break;
        case 15: 
            index = 168;
            break;
        case 16: 
            index = 177;
            break;
    }

    return index;
}

void Symmetry2dx::initialize(std::string symmetry) {
    symmetry[0] = std::toupper(symmetry[0]);
     
    if( symmetry == "P1")  name = P1;
    else if( symmetry == "P2") name = P2;
    else if( symmetry == "P12") name = P12;
    else if( symmetry == "P121") name = P121;
    else if( symmetry == "C12") name = C12;
    else if( symmetry == "P222") name =  P222;
    else if( symmetry == "P2221") name =  P2221;
    else if( symmetry == "P22121") name =  P22121;
    else if( symmetry == "C222") name = C222;
    else if( symmetry == "P4") name = P4;
    else if( symmetry == "P422") name = P422;
    else if( symmetry == "P4212") name = P4212;
    else if( symmetry == "P3") name = P3;
    else if( symmetry == "P312") name = P312;
    else if( symmetry == "P321") name = P321;
    else if( symmetry == "P6") name = P6;
    else if( symmetry == "P622") name = P622;
    else throw std::out_of_range ( "Invalid value for symmetry: "+symmetry );
    
}
