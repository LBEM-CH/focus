/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef ANGLE_UTILITIES_HPP
#define	ANGLE_UTILITIES_HPP

#include <math.h>

namespace volume
{
    namespace utilities
    {
        namespace angle_utilities
        {
            /**
             * Converts the input angle in degrees to radians.
             * Uses math.h for the precision of PI.
             * @param angle_in_degrees
             * @return angle in radians
             */
            double DegreeToRadian(double angle_in_degrees);
            
            /**
             * Converts the input angle in radians to degrees.
             * Uses math.h for the precision of PI.
             * @param angle_in_radians
             * @return angle_in_degrees
             */
            double RadianToDegree(double angle_in_radians);
            
            /**
             * Brings the value of input phase to (-PI, PI]
             * @param phase_in_radians
             * @return corrected phase in radians
             */
            double  CorrectRadianPhase(double phase_in_radians);
            
        } // namespace angle_utilities
    
    } // namespace utilities
    
} // namespace volume_processing_2dx

#endif	/* ANGLE_UTILITIES_HPP */

