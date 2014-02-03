#include "AverageWeighter.hpp"

#include <iostream>

SingleParticle2dx::Utilities::AverageWeighter::AverageWeighter(size_type mode, size_type n, value_type tilt)
 : m_mode(mode),
   m_n(n),
   m_tilt(tilt)
{}


SingleParticle2dx::Utilities::AverageWeighter::value_type SingleParticle2dx::Utilities::AverageWeighter::getWeight()
{
	switch (m_mode)
	{
		case 0:
			return 1;
			break;
		case 1:
			return m_n;
			break;
		case 2:
			return log(m_n);
			break;
		case 3:
			return exp(m_n/1000.0);
			break;
		case 4:
			return fabs(m_tilt)/60.0+1;
		default:
			std::cerr << "Unknown average weighting method" << std::endl;
			throw std::runtime_error("Bad operation");
	}
}
