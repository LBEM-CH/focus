/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "number_utilities.hpp"


int volume_processing_2dx::utilities::number_utilities::Sign(int value)
{
    if (value > 0) return 1;
    if (value < 0) return -1;
    return 0;
}
