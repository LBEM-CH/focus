/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef FOMMANIPULATOR_HPP
#define	FOMMANIPULATOR_HPP

#include <math.h>
#include <list>

namespace tdx
{
    namespace utilities
    {
        namespace fom_utilities
        {
            /**
             * Converts the FOM to XARG.
             * Gets the XARG value from the pre-calculated reference table and 
             * interpolates (linear) it according to the input value. 
             * @param fom in fraction
             * @return XARG value
             */
            double FomToXarg(double fom);
            
            /**
             * Converts the XARG value to FOM value
             * FOM values are defined as the ratio of the 0th and 1st order 
             * of modified Bessel functions of the first kind.
             * @param input XARG value
             * @return FOM value in fraction
             */
            double XargToFom(double xarg);
            
            /**
             * Averages FOMs from a list. Averaging weight is not straight 
             * forward. First XARG values are calculated using the FOM values.
             * This sum of XARG is converted back to FOM to get the averaged
             * weight. NOTE, that sum of XARG is converted back to FOM and not
             * the average of XARG. 
             * @param foms: list of FOM values in fractions
             * @return averaged weight
             */
            double AverageFOMs(const std::list<double> foms);
            
            /**
             * Lookup table of FOM values and XARG values.
             * The data was generated using a function which increased  XARG
             * from 0 to 54.8 in 100 steps and calculated the corresponding FOM
             * values. Now, these FOM values can be used to get corresponding
             * XARG values. 
             * NOTE: the FOM values are in percentage
             */
            const double LOOKUP_FOM_TO_XARG[101][2] = {
                {  0.000000,  0.000000}, 
		{  1.049942,  0.021000}, 
		{  2.152001,  0.043050}, 
		{  3.308313,  0.066202}, 
		{  4.521002,  0.090513}, 
		{  5.792169,  0.116038}, 
		{  7.123854,  0.142840}, 
		{  8.518017,  0.170982}, 
		{  9.976499,  0.200531}, 
		{ 11.500977,  0.231558},
		{ 13.092929,  0.264136}, 
		{ 14.753577,  0.298342}, 
		{ 16.483826,  0.334260}, 
		{ 18.284201,  0.371973}, 
		{ 20.154779,  0.411571}, 
		{ 22.095135,  0.453150}, 
		{ 24.104219,  0.496807}, 
		{ 26.180330,  0.542647}, 
		{ 28.320995,  0.590780}, 
		{ 30.522934,  0.641319}, 
		{ 32.781960,  0.694385}, 
		{ 35.092937,  0.750104}, 
		{ 37.449760,  0.808609}, 
		{ 39.845322,  0.870039}, 
		{ 42.271519,  0.934541}, 
		{ 44.719318,  1.002268}, 
		{ 47.178829,  1.073382}, 
		{ 49.639431,  1.148051}, 
		{ 52.089916,  1.226453},
		{ 54.518742,  1.308776}, 
		{ 56.914192,  1.395215}, 
		{ 59.264709,  1.485975}, 
		{ 61.559139,  1.581274}, 
		{ 63.787025,  1.681338}, 
		{ 65.938858,  1.786404}, 
		{ 68.006363,  1.896724}, 
		{ 69.982666,  2.012561}, 
		{ 71.862434,  2.134189}, 
		{ 73.641998,  2.261898}, 
		{ 75.319344,  2.395993}, 
		{ 76.894043,  2.536792}, 
		{ 78.367195,  2.684632}, 
		{ 79.741211,  2.839863}, 
		{ 81.019623,  3.002856}, 
		{ 82.206879,  3.173999}, 
		{ 83.308060,  3.353699}, 
		{ 84.328720,  3.542383}, 
		{ 85.274574,  3.740502}, 
		{ 86.151398,  3.948527}, 
		{ 86.964813,  4.166953}, 
		{ 87.720207,  4.396301}, 
		{ 88.422630,  4.637116}, 
		{ 89.076744,  4.889971}, 
		{ 89.686783,  5.155470}, 
		{ 90.256584,  5.434243}, 
		{ 90.789612,  5.726955}, 
		{ 91.288910,  6.034302},
		{ 91.757240,  6.357017}, 
		{ 92.197037,  6.695868}, 
		{ 92.610481,  7.051661}, 
		{ 92.999527,  7.425243}, 
		{ 93.365936,  7.817505}, 
		{ 93.711273,  8.229380}, 
		{ 94.037003,  8.661849}, 
		{ 94.344406,  9.115941}, 
		{ 94.634712,  9.592737}, 
		{ 94.908974, 10.093374}, 
		{ 95.168236, 10.619042}, 
		{ 95.413422, 11.170993}, 
		{ 95.645378, 11.750542}, 
		{ 95.864914, 12.359069},
		{ 96.072754, 12.998022}, 
		{ 96.269608, 13.668922}, 
		{ 96.456085, 14.373368}, 
		{ 96.632813, 15.113036}, 
		{ 96.800323, 15.889687}, 
		{ 96.959152, 16.705170}, 
		{ 97.109779, 17.561428}, 
		{ 97.252663, 18.460498}, 
		{ 97.388214, 19.404522}, 
		{ 97.516861, 20.395748}, 
		{ 97.638962, 21.436534}, 
		{ 97.754875, 22.529360}, 
		{ 97.864922, 23.676827},
		{ 97.969421, 24.881667}, 
		{ 98.068687, 26.146749}, 
		{ 98.162964, 27.475085}, 
		{ 98.252541, 28.869838}, 
		{ 98.337639, 30.334329}, 
		{ 98.418518, 31.872044}, 
		{ 98.495361, 33.486644}, 
		{ 98.568420, 35.181975}, 
		{ 98.637848, 36.962072},
		{ 98.703857, 38.831174}, 
		{ 98.766617, 40.793731}, 
		{ 98.826294, 42.854415}, 
		{ 98.883026, 45.018134}, 
		{ 98.936981, 47.290039}, 
		{ 98.988304, 49.675538}, 
		{ 99.037102, 52.180313}, 
		{ 99.083527, 54.810326}

                };
            
        } //namespace fom_manipulator
        
    } //namespace utilities
    
} //namespace volume_processing_2dx

#endif	/* FOMMANIPULATOR_HPP */

