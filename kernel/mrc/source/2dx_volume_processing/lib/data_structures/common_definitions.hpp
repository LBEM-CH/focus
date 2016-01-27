/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef COMMON_DEFINITIONS_HPP
#define	COMMON_DEFINITIONS_HPP

#include <map>
#include <list>

#include "miller_index.hpp"
#include "diffraction_spot.hpp"

namespace tdx
{
    namespace data
    {
        /**
         * A map of Miller Index and diffraction spot. Stores sorted values 
         * of a miller index mapped to diffraction spots.
         */
        typedef std::map<MillerIndex, DiffractionSpot> DiffractionSpotMap;
        
        /**
         * A multi-map of Miller Index and Diffraction spot. 
         * Stores multiple diffraction spots at one miller index. 
         */
        typedef std::multimap<MillerIndex, DiffractionSpot> DiffractionSpotMultiMap;
        
        /**
         * Defines a pair of Miller Index and Diffraction Spot.
         */
        typedef std::pair<MillerIndex, DiffractionSpot> MillerIndexDiffSpotPair;
        
        /**
         * Defines a list of diffraction spots
         */
        typedef std::list<DiffractionSpot> DiffractionSpotList;
        
        /**
         * Defines a list of complex
         */
        typedef std::list<Complex> ComplexList;
        
        /**
         * Defines a list of doubles
         */
        typedef std::list<double> DoubleList;
        
    }
    
}
        


#endif	/* COMMON_DEFINITIONS_HPP */

