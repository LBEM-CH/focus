/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef SYMMETRY2DX_HPP
#define	SYMMETRY2DX_HPP

#include <iostream>
#include <stdexcept>

#include <string.h>

namespace tdx
{
    namespace symmetrization
    {
        /**
         * A class to use and process 17 different crystallographic symmetries 
         * which can occur in 2D crystals.
         * The following symmetries can occur in 2D crystals:
         * P1, P2, P12, P121, C12, P222, P2221, P22121, C222, P4, P422, P4212, P3, P312, P321, P6, P622
         * P1 has the code 0 and other follows till 16.
         */
        class Symmetry2dx
        {
            
        public:
            
            /**
             * Default constructor initializing symmetry to P1
             */
            Symmetry2dx();
    
            /**
             * Constructor with string symmetry
             */
            Symmetry2dx(std::string symmetry);
            
            /**
             * Operator overloading of <<
             * Can be used for output purposes
             * @param os : input output stream
             * @return modified output stream
             */
            std::ostream& operator<<(std::ostream& os);
            
            /**
             * Sets the symmetry using a string
             * @param symmetry: input symmetry string
             */
            void set_symmetry(std::string symmetry);
            
            /**
             * Sets the symmetry using int code
             * @param code: int possible values from 0 to 16
             */
            void set_symmetry(int code);
            
            /**
             * Returns the string of the corresponding symmetry
             * @return The string of the corresponding symmetry 
             */
            std::string symmetry_string() const;
            
            /**
             * Returns the code of the symmetry (value between 0 to 16)  
             * @return : code
             */
            int symmetry_code() const;
            
            /**
             * Returns the CCP4 index of the symmetry
             * @return CCP4 index
             */
            int ccp4_index() const;
            
        private:
            /**
             * Possible 2D crystallographic symmetries.
             * Each symmetry is represented as an enum with P1 being set to 0.
             * The rest are just incrementally followed. One can exchange the
             * names with integers at any time.
             */
            enum SymmetryName
            {
                P1=0, P2, P12, P121, C12, P222, P2221, P22121, C222, P4, P422, P4212, P3, P312, P321, P6, P622
            };
            
            /**
             * Class members initializer with input string
             * @param symmetry
             */
            void initialize(std::string symmetry);
            
            /**
             * Symmetry Name of the symmetry.
             * Internally is an int code
             */
            SymmetryName _name;
            
            
            
        };//class Symmetry2dx
        
    }//namespace symmetrization   
    
}//namespace volume_processing_2dx

#endif	/* SYMMETRY2DX_HPP */

