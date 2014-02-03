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

#include "EMANNNReconstructor.hpp"

#include "../../Utilities.hpp"

#include <transform.h>

SingleParticle2dx::Methods::EMANNNReconstructor::EMANNNReconstructor (SingleParticle2dx::DataStructures::Reconstruction3d* context)
{
	m_context = context;
	
	/*
	d.put("size", EMObject::INTARRAY, "Required. The dimensions of the real-space output volume, including any padding (must be handled by the calling application). Assumed that apix x/y/z identical.");
	d.put("sym", EMObject::STRING, "Optional. The symmetry of the reconstructed volume, c?, d?, oct, tet, icos, h?. Default is c1, ie - an asymmetric object");
	d.put("mode", EMObject::STRING, "Optional. Fourier pixel insertion mode name (nearest_neighbor, gauss_2, gauss_3, gauss_5, gauss_5_slow, gypergeom_5, experimental) gauss_2 is the default.");
	d.put("sqrtnorm", EMObject::BOOL, "Optional. When normalizing, additionally divides by the sqrt of the normalization factor to damp exaggerated features. Is this justifyable ? No idea (yet). Default is false.");
	d.put("verbose", EMObject::BOOL, "Optional. Toggles writing useful information to standard out. Default is false.");
	d.put("quiet", EMObject::BOOL, "Optional. If false, print verbose information.");
	d.put("subvolume",EMObject::INTARRAY, "Optional. (xorigin,yorigin,zorigin,xsize,ysize,zsize) all in Fourier pixels. Useful for parallelism.");
	d.put("savenorm",EMObject::STRING, "Debug. Will cause the normalization volume to be written directly to the specified file when finish() is called.");
	*/
	
	std::vector<int> size;
	size.push_back(m_context->getSizeX());
	size.push_back(m_context->getSizeY());
	size.push_back(m_context->getSizeZ());
	
	EMAN::EMData* eman_fftvol = new EMAN::EMData;
	EMAN::EMData* eman_weight = new EMAN::EMData;
	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();

	m_params["size"] = size[0];
	m_params["symmetry"] = (config->getSymmetryString()).c_str();
	m_params["ndim"] = 3;
	m_params["npad"] = 3;
	m_params["fftvol"] = eman_fftvol;
	m_params["weight"] = eman_weight;
	
	std::vector<int> array;
	array.push_back(0);
	array.push_back(0);
	array.push_back(0);
	array.push_back(m_context->getSizeX());
	array.push_back(m_context->getSizeX());
	array.push_back(m_context->getSizeX());
		
	m_rec = EMAN::Factory<EMAN::Reconstructor>::get("nn4", m_params);
}


SingleParticle2dx::Methods::EMANNNReconstructor::~EMANNNReconstructor ()
{}


void SingleParticle2dx::Methods::EMANNNReconstructor::setupForBackProjection()
{
	m_rec->setup();
}


void SingleParticle2dx::Methods::EMANNNReconstructor::finishReconstruction()
{
	size_type size = m_context->getSizeX() * m_context->getSizeY() * m_context->getSizeZ();
	EMAN::EMData* eman_data = new EMAN::EMData;
	eman_data->set_size(m_context->getSizeX(), m_context->getSizeY(), m_context->getSizeZ());
	//m_rec->print_params();
	eman_data = m_rec->finish();
	real_array3d_type rdata( boost::extents[m_context->getSizeX()][m_context->getSizeY()][m_context->getSizeZ()] );
	//eman_data->debug_print_parms();
	std::copy(eman_data->get_data(), eman_data->get_data()+size, rdata.origin() );
	SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&rdata);
	m_context->setFourierSpaceData(rdata);
	delete eman_data;
}
						

void SingleParticle2dx::Methods::EMANNNReconstructor::insertData(SingleParticle2dx::DataStructures::ParticleContainer &c)
{
	for (size_type i=0; i<c.getNumberOfParticles(); i++)
	{
		//std::cout << i << "/" << c.getNumberOfParticles()-1 << std::endl;
		insertParticle(c(i));
	}
}


void SingleParticle2dx::Methods::EMANNNReconstructor::insertParticle(SingleParticle2dx::DataStructures::Particle& p)
{
	if ( !p.getUseForReconstruction())
	{
		//std::cout << "too low cc value, not using this particle for reconstruction" << std::endl;
		
	//	#pragma omp critical (bad_particles_update)
	//	{
	//		m_bad_counter++;
	//	}
		
		return;
	}
	
	size_type size = m_context->getSizeX() * m_context->getSizeY();
	float* float_data_2d;
	float_data_2d = (float*) malloc(size * sizeof(float));
	real_array2d_type rdata( boost::extents[m_context->getSizeX()][m_context->getSizeY()] );
	
	SingleParticle2dx::DataStructures::Particle p2insert;
	p2insert = p;
	p2insert.applyLowPassFilter();
	p2insert.updateParticleShift();
	
	//p2insert.getRealSpaceData(&rdata);
	//std::copy(rdata.origin(), rdata.origin()+rdata.num_elements(), float_data_2d );		
	//EMAN::EMData eman_data;
	//eman_data.set_data(float_data_2d, m_context->getSizeX(), m_context->getSizeY(), 1);
	//eman_data.mult(p.getWeight());
	
	p2insert.applyWeight(p.getWeight());
	
	p2insert.getRealSpaceData(&rdata);
	std::copy(rdata.origin(), rdata.origin()+rdata.num_elements(), float_data_2d );
	EMAN::EMData eman_data;
	eman_data.set_data(float_data_2d, m_context->getSizeX(), m_context->getSizeY(), 1);
	
	EMAN::Transform t;
	EMAN::Dict rot;
	
	//std::cout << "\t" << p.getNewOrientation().getTLTAXIS() << ", " << p.getNewOrientation().getTLTANG() << ", " << p.getNewOrientation().getTAXA() << std::endl;
	
//	Eigen::Matrix3f rot_matrix = SingleParticle2dx::Utilities::UtilityFunctions::determineRotation( p.getNewOrientation() );
	
	rot["type"] = "spider";
//	rot["m11"] = rot_matrix(0,0);
//	rot["m12"] = rot_matrix(0,1);
//	rot["m13"] = rot_matrix(0,2);
//	rot["m21"] = rot_matrix(1,0);
//	rot["m22"] = rot_matrix(1,1);
//	rot["m23"] = rot_matrix(1,2);
//	rot["m31"] = rot_matrix(2,0);
//	rot["m32"] = rot_matrix(2,1);
//	rot["m33"] = rot_matrix(2,2);
	
//	rot["phi"] = p.getNewOrientation().getTLTAXIS() + 90;
//	rot["theta"] = p.getNewOrientation().getTLTANG();
//	rot["psi"] = p.getNewOrientation().getTAXA();

	rot["phi"] = p.getNewOrientation().getTAXA();
	rot["theta"] = p.getNewOrientation().getTLTANG();
	rot["psi"] = p.getNewOrientation().getTLTAXIS() + 90;
	
//	rot["psi"] = +p.getNewOrientation().getTLTAXIS();
//	rot["theta"] = -p.getNewOrientation().getTLTANG();
//	rot["phi"] = -p.getNewOrientation().getTAXA();

	t.set_rotation(rot);
	
	if (m_rec->insert_slice(&eman_data, t, 1) != 0)
	{
		std::cerr << "slice inserting in eman2 failed" << std::endl;
		throw std::runtime_error("Bad operation");
	}
}

