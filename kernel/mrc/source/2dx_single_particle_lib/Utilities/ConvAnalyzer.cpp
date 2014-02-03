#include "ConvAnalyzer.hpp"


SingleParticle2dx::Utilities::ConvAnalyzer::ConvAnalyzer (value_type t, size_type n)
 : m_conv_threshold(t),
   m_cyclic_number(n)
{}


SingleParticle2dx::Utilities::ConvAnalyzer::~ConvAnalyzer ()
{}


void SingleParticle2dx::Utilities::ConvAnalyzer::addChange(value_type rhs)
{
	m_data.push_back(rhs);
}


void SingleParticle2dx::Utilities::ConvAnalyzer::reset()
{
	m_data.clear();
}


bool SingleParticle2dx::Utilities::ConvAnalyzer::isConverged()
{
	return ( m_data[static_cast<size_type>(m_data.size())-1] < m_conv_threshold );
}


bool SingleParticle2dx::Utilities::ConvAnalyzer::isCycling()
{
	if ( static_cast<size_type>(m_data.size()) < m_cyclic_number )
	{
		return false;
	}
	
	value_type last_change = m_data[static_cast<size_type>(m_data.size())-1];
	
	for(size_type i=static_cast<size_type>(m_data.size())-m_cyclic_number; i<static_cast<size_type>(m_data.size())-2; i++)
	{
		if(m_data[i]==last_change)
		{
			return true;
		}
	}
	
	return false;
		
}