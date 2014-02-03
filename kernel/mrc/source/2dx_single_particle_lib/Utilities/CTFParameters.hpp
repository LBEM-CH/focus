#ifndef CTFPARAMETERS_HPP_3Z4NDTBL
#define CTFPARAMETERS_HPP_3Z4NDTBL


namespace SingleParticle2dx
{
	namespace Utilities
	{
		struct CTFParameters;
	}
}


#include "../Typedefs.hpp"


namespace SingleParticle2dx
{
	namespace Utilities
	{
		struct CTFParameters
		{
			typedef SingleParticle2dx::value_type value_type;
			typedef SingleParticle2dx::size_type size_type;
			
			value_type m_cs;
			value_type m_kv;
			value_type m_difmid1;
			value_type m_difmid2;
			value_type m_angast;
			value_type m_dstep;
			value_type m_mag;
			value_type m_tltaxis;
			value_type m_tltang;
			value_type m_posx;
			value_type m_posy;
			value_type m_size_x;
			value_type m_size_y;
			
			CTFParameters(value_type cs, value_type kv, value_type difmid1, value_type difmid2, value_type angast, value_type dstep, value_type mag, value_type tltaxis, value_type tltang, size_type size_x, size_type size_y)
			 : m_cs(cs), m_kv(kv), m_difmid1(difmid1), m_difmid2(difmid2), m_angast(angast), m_dstep(dstep), m_mag(mag), m_tltaxis(tltaxis), m_tltang(tltang), m_size_x(size_x), m_size_y(size_y)
			{ 
				m_posx = 0;
				m_posy = 0;
			}
			
			void print()
			{
				std::cout << "\n\nCTF Parameters" << std::endl;
				std::cout << "\tCS = " << m_cs << std::endl;
				std::cout << "\tKV = " << m_kv << std::endl;
				std::cout << "\tdifmid1 = " << m_difmid1 << std::endl;
				std::cout << "\tdifmid2 = " << m_difmid2 << std::endl;
				std::cout << "\tangast = " << m_angast << std::endl;
				std::cout << "\tdstep = " << m_dstep << std::endl;
				std::cout << "\tmag = " << m_mag << std::endl;
				std::cout << "\tposx = " << m_posx << std::endl;
				std::cout << "\tposy = " << m_posy << std::endl << std::endl;
			}
			
		};
				
	} /* Utilities */
	
} /* SingleParticle2dx */

#endif /* end of include guard: CTFPARAMETERS_HPP_3Z4NDTBL */
