/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "symmetry2dx.hpp"

namespace sym = volume_processing_2dx::symmetrization;

sym::Symmetry2dx::Symmetry2dx()
{
    this->initialize("P1");
}

sym::Symmetry2dx::Symmetry2dx(std::string symmetry) {
    this->initialize(symmetry);
}

std::ostream& sym::Symmetry2dx::operator<<(std::ostream& os) {
    os << this->symmetry_string();
    return os;
}

void sym::Symmetry2dx::set_symmetry(std::string symmetry) {
    initialize(symmetry);
}

int sym::Symmetry2dx::symmetry_code() const {
    return this->_name;
}

std::string sym::Symmetry2dx::symmetry_string() const{
    std::string symmetry_string;
    
    switch(this->_name){
        case 0: 
            symmetry_string = "P1";
            break;
        case 1: 
            symmetry_string = "P2";
            break;
        case 2: 
            symmetry_string = "P12";
            break;
        case 3: 
            symmetry_string = "P121";
            break;
        case 4: 
            symmetry_string = "C12";
            break;
        case 5: 
            symmetry_string =  "P222";
            break;
        case 6: 
            symmetry_string =  "P2221";
            break;
        case 7: 
            symmetry_string =  "P22121";
            break;
        case 8: 
            symmetry_string = "C222";
            break;
        case 9: 
            symmetry_string = "P4";
            break;
        case 10: 
            symmetry_string = "P422";
            break;
        case 11: 
            symmetry_string = "P4212";
            break;
        case 12: 
            symmetry_string = "P3";
            break;
        case 13: 
            symmetry_string = "P312";
            break;
        case 14: 
            symmetry_string = "P321";
            break;
        case 15: 
            symmetry_string = "P6";
            break;
        case 16: 
            symmetry_string = "P622";
            break;
    }
    
    return symmetry_string;
}


int sym::Symmetry2dx::ccp4_index() const{
    int index;
    
    switch(this->_name){
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

void sym::Symmetry2dx::initialize(std::string symmetry) {
    symmetry[0] = std::toupper(symmetry[0]);
     
    if( symmetry == "P1")  _name = P1;
    else if( symmetry == "P2") _name = P2;
    else if( symmetry == "P12") _name = P12;
    else if( symmetry == "P121") _name = P121;
    else if( symmetry == "C12") _name = C12;
    else if( symmetry == "P222") _name =  P222;
    else if( symmetry == "P2221") _name =  P2221;
    else if( symmetry == "P22121") _name =  P22121;
    else if( symmetry == "C222") _name = C222;
    else if( symmetry == "P4") _name = P4;
    else if( symmetry == "P422") _name = P422;
    else if( symmetry == "P4212") _name = P4212;
    else if( symmetry == "P3") _name = P3;
    else if( symmetry == "P312") _name = P312;
    else if( symmetry == "P321") _name = P321;
    else if( symmetry == "P6") _name = P6;
    else if( symmetry == "P622") _name = P622;
    else throw std::out_of_range ( "Invalid value for symmetry: " + symmetry );
    
}

