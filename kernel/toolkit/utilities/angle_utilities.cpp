/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "angle_utilities.hpp"


double tdx::utilities::angle_utilities::CorrectRadianPhase(double phase_in_radians)
{   
    double new_phase = phase_in_radians;
    
    if (new_phase <= -1 * M_PI) {
        new_phase = CorrectRadianPhase(phase_in_radians + 2*M_PI);
    }
    else if (phase_in_radians > M_PI) {
        new_phase = CorrectRadianPhase(phase_in_radians - 2*M_PI);
    }
    
    return new_phase;
}

double tdx::utilities::angle_utilities::DegreeToRadian(double angle_in_degrees)
{
    return angle_in_degrees * M_PI/180;
}

double tdx::utilities::angle_utilities::RadianToDegree(double angle_in_radians)
{
    return angle_in_radians * 180/M_PI;
}
