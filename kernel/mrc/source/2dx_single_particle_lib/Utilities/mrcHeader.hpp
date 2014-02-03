#ifndef SINGLE_PARTICLE_2DX_MRCHEADER_HPP
#define SINGLE_PARTICLE_2DX_MRCHEADER_HPP

namespace SingleParticle2dx
{	
	namespace Utilities
	{
		struct mrcHeader;
	}
}

#include "../Typedefs.hpp"

namespace SingleParticle2dx
{
	namespace Utilities
	{
		
		/**
		 *  @brief      MRC file format header
		 *  @details    Struct that can be used to easily wrtie a legal mrc-header
		 */
		struct mrcHeader
		{
			typedef SingleParticle2dx::mrc_int mrc_int;
			typedef SingleParticle2dx::mrc_float mrc_float;
			typedef SingleParticle2dx::mrc_char mrc_char;

			mrc_int nx;              //  0   0       image size
			mrc_int ny;              //  1   4
			mrc_int nz;              //  2   8
			mrc_int mode;            //  3           0=char,1=short,2=float
			mrc_int nxStart;         //  4           unit cell offset
			mrc_int nyStart;         //  5
			mrc_int nzStart;         //  6
			mrc_int mx;              //  7           unit cell size in voxels
			mrc_int my;              //  8
			mrc_int mz;              //  9
			mrc_float a;             // 10   40      cell dimensions in A
			mrc_float b;             // 11
			mrc_float c;             // 12
			mrc_float alpha;         // 13           cell angles in degrees
			mrc_float beta;          // 14
			mrc_float gamma;         // 15
			mrc_int mapc;            // 16           column axis
			mrc_int mapr;            // 17           row axis
			mrc_int maps;            // 18           section axis
			mrc_float amin;          // 19           minimum density value
			mrc_float amax;          // 20   80      maximum density value
			mrc_float amean;         // 21           average density value
			mrc_int ispg;            // 22           space group number
			mrc_int nsymbt;          // 23           bytes used for sym. ops. table
			mrc_float extra[25];     // 24           user-defined info
			mrc_float xOrigin;       // 49           phase origin in pixels FIXME: is in pixels or [L] units?
			mrc_float yOrigin;       // 50
			mrc_float zOrigin;       // 51
			mrc_char map[4];         // 52           identifier for map file ("MAP ")
			mrc_char machst[4];      // 53           machine stamp
			mrc_float arms;          // 54           RMS deviation
			mrc_int nlabl;           // 55           number of labels used
			mrc_char labels[800];    // 56-255       10 80-character labels

		};
		
	} /* Utilities */

} /* SingleParticle2dx */

#endif /* end of include guard: SINGLE_PARTICLE_2DX_MRCHEADER_HPP */


