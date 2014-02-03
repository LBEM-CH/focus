/*
 *  Copyright (C) 2012 by C-Cina University of Basel
 *  www.c-cina.unibas.ch
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the
 *  Free Software Foundation, Inc.,
 *  59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */


#include "OMPCCRefinementMethod.hpp"
#include <boost/filesystem.hpp>


SingleParticle2dx::Methods::OMPCCRefinementMethod::OMPCCRefinementMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context)
{
	m_context = context;
	m_bestproj_strategy.reset( new SingleParticle2dx::Methods::CCFindBestProjectionMethod (m_context) );
}


SingleParticle2dx::Methods::OMPCCRefinementMethod::~OMPCCRefinementMethod ()
{}


void SingleParticle2dx::Methods::OMPCCRefinementMethod::updateReconstruction(SingleParticle2dx::DataStructures::ParticleContainer& c, bool useneighbors, bool write_debug_output)
{
	m_context->resetAngularChangeMeasure();
	
//	boost::filesystem::remove_all("cc_profiles2");
//	boost::filesystem::create_directory("cc_profiles2");
	
	/*
	SingleParticle2dx::DataStructures::Orientation o;
	o.setTLTAXIS(0);
	o.setTLTANG(0);
	o.setTAXA(13);
	SingleParticle2dx::DataStructures::Projection2d p(m_context->getSizeX(), m_context->getSizeY());
	m_context->calculateProjection(o, p);
	p.writeToFile("new_projection_test.mrc");

	o.setTLTAXIS(0);
	o.setTLTANG(1);
	o.setTAXA(13);
	m_context->calculateProjection(o, p);
	p.writeToFile("new_projection_test2.mrc");

	o.setTLTAXIS(80.86);
	o.setTLTANG(28.56);
	o.setTAXA(38.231);
	m_context->calculateProjection(o, p);
	p.writeToFile("new_projection_test3.mrc");
	*/
	
//	std::string foldername = "CC_profiles";
//	boost::filesystem::remove_all(foldername);
//	boost::filesystem::create_directory(foldername);

	if (write_debug_output)
	{
		if(boost::filesystem::exists("ref_debug_output"))
		{
			boost::filesystem::remove_all("ref_debug_output");
		}
		std::cout << ":ref debug created" << std::endl;
		boost::filesystem::create_directory("ref_debug_output");
	}
	
	size_type dynamic_load = std::max(static_cast<size_type>(c.getNumberOfParticles()/128.0), 1);
	
//	size_type counter = 0;
	
	#pragma omp parallel for schedule(dynamic, dynamic_load)
	for (size_type i=0; i<c.getNumberOfParticles(); i++)
	{
		m_bestproj_strategy.get()->determineBestProjection(c(i), useneighbors, write_debug_output);
		
//		#pragma omp critical
//		{
//			counter++;
//			if(counter%100==0)
//			{
//				std::cout << counter << std::endl;
//			}
//		}
	}
	
	for (size_type i=0; i<c.getNumberOfParticles(); i++)
	{
		m_context->updateAngularChangeMeasure(c(i).getLastAngularChange());
	}
	
	std::cout << "::Angular change: " << m_context->getLastAngleChange() << std::endl;
	std::cout << "::Similarity value: " << c.getTotalSim() << std::endl;
}
