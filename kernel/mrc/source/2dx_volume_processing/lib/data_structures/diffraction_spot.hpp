/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef DIFFRACTION_SPOT_HPP
#define	DIFFRACTION_SPOT_HPP

#include <iostream>
#include <stdexcept>
#include <list>
#include <string.h>

#include "complex2dx.hpp"

namespace volume
{
    namespace data
    {
        /**
         * A class used to store the data of a diffraction spot. 
         * A diffraction spot has a complex and it's weight associated to it.
         */
        class DiffractionSpot
        {
            
        public:
            
            /**
             * Default constructor with value assigned to (0 + 0i) 
             * and weight assigned to 0.0
             */
            DiffractionSpot();
            
            /**
             * Constructor with a value and weight
             * @param value
             * @param weight in fractions
             */
            DiffractionSpot(Complex2dx value, double weight);
            
            /**
             * Constructor from a list of Diffraction spots.
             * Averages the spots from a list of diffraction spots.
             * @param spots: list of diffraction spots
             */
            DiffractionSpot(const std::list<DiffractionSpot> spots);
            
            /**
             * Define = oeprator. Equates the weights and the values
             * @param rhs
             * @return this instance equated to rhs
             */
            DiffractionSpot& operator=(const DiffractionSpot& rhs);
            
            /**
             * Definition of + operator. 
             * Adds the complex values and does special averaging of the weights
             * using the xargs
             * @param rhs
             * @return added diffraction spot
             */
            DiffractionSpot operator+(const DiffractionSpot& rhs);
            
            /**
             * Definition of == operator.
             * Checks for the equality of both value and weight
             * @param rhs
             * @return True/false
             */
            bool operator==(const DiffractionSpot& rhs) const;
            
            /**
             * Definition of < operator.
             * Checks if the amplitudes of the values are small, 
             * if equal goes for the higher weight.
             * @param rhs
             * @return 
             */
            bool operator<(const DiffractionSpot& rhs) const;
            
            /**
             * Gets the complex value of the spot
             * @return value
             */
            Complex2dx value() const ;
            
            /**
             * Gets the weight of the spot
             * @return FOM weight in fractions
             */
            double weight() const ;
            
            /**
             * Gets the amplitude of the spot
             * @return amplitude
             */
            double amplitude() const;
            
            /**
             * Gets the intensity of the spot
             * @return intensity
             */
            double intensity() const;
            
            /**
             * Gets the phase of the spot
             * @return phase
             */
            double phase() const;
            
            /**
             * Assigner function of the value
             * @param value
             */
            void set_value(Complex2dx value);
            
            /**
             * Assigner function of the weight
             * @param weight in fractions
             */
            void set_weight(double weight);
            
            
        private:
            
            /**
             * Initializer function of the members
             * @param value
             * @param weight
             */
            void initialize(Complex2dx value, double weight);
            
            /**
             * Value of the diffraction spot
             */
            Complex2dx _value;
            
            /**
             * weight of the diffraction spot
             */
            double _weight;
            
        };//class DiffractionSpots
        
    }//namespace data_structures
    
}//namespace volume_processing_2dx


#endif	/* DIFFRACTION_SPOT_HPP */

