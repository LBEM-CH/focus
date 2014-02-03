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

#include <fstream>
#include <algorithm>

#include <boost/multi_array.hpp>
#include <boost/utility.hpp>

#include <boost/filesystem.hpp>
//#include <boost/algorithm/string.hpp>
//#include <boost/algorithm/string/split.hpp>

#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/serialization/utility.hpp>
#include <boost/serialization/vector.hpp>

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>
#include <boost/spirit/include/qi_char_.hpp>
#include <boost/spirit/include/qi_string.hpp>
#include <boost/spirit/include/qi_no_skip.hpp>

#include <boost/accumulators/accumulators.hpp>
#include <boost/accumulators/statistics/stats.hpp>
#include <boost/accumulators/statistics/mean.hpp>
#include <boost/accumulators/statistics/variance.hpp>
#include <boost/accumulators/statistics/min.hpp>
#include <boost/accumulators/statistics/max.hpp>

#include <time.h>
#include <boost/random/normal_distribution.hpp>
#include <boost/random/mersenne_twister.hpp>
#include <boost/random/variate_generator.hpp>
#include <boost/random/uniform_real.hpp>
#include <boost/random.hpp>
#include <boost/generator_iterator.hpp>

#include "ParticleContainer.hpp"
#include "ParticleFingerPrint.hpp"
#include "../Utilities/ParticleDiffComp.hpp"
#include "../Utilities.hpp"
#include "../Config.hpp"


SingleParticle2dx::DataStructures::ParticleContainer::ParticleContainer ()
 : m_bad_particles(0)
{}


SingleParticle2dx::DataStructures::ParticleContainer::~ParticleContainer ()
{}


void SingleParticle2dx::DataStructures::ParticleContainer::addParticle(SingleParticle2dx::DataStructures::Particle& part)
{
	#pragma omp critical (add_part_in_container_self)
	{
		m_particles.push_back(part);
	}
}


void SingleParticle2dx::DataStructures::ParticleContainer::addParticleFast(SingleParticle2dx::DataStructures::Particle& part)
{
	m_particles.push_back(part);
}


SingleParticle2dx::DataStructures::Particle& SingleParticle2dx::DataStructures::ParticleContainer::operator() (const int i)
{
	return m_particles[i];
}


int SingleParticle2dx::DataStructures::ParticleContainer::getNumberOfParticles()
{
	return m_particles.size();
}


void SingleParticle2dx::DataStructures::ParticleContainer::setParticleNumbers()
{
	for (size_type i=0; i<getNumberOfParticles(); i++)
	{
		m_particles[i].setParticleNumber(i);
	}
}


void SingleParticle2dx::DataStructures::ParticleContainer::getDistinctAngles(std::vector<SingleParticle2dx::DataStructures::Orientation>& o_vec)
{
	SingleParticle2dx::DataStructures::Orientation trial_o;
	
	for (size_type i=0; i<getNumberOfParticles(); i++)
	{
		trial_o = m_particles[i].getInitialOrientation();		
		if(std::find(o_vec.begin(), o_vec.end(), trial_o) == o_vec.end())
		{
			o_vec.push_back(trial_o);
		}
	}
}


void SingleParticle2dx::DataStructures::ParticleContainer::setAllOrientations( SingleParticle2dx::DataStructures::Orientation o )
{
	for (size_type i=0; i<getNumberOfParticles(); i++)
	{
		m_particles[i].setOrientation(o);
	}
}


void SingleParticle2dx::DataStructures::ParticleContainer::clear()
{
	m_particles.clear();
	m_particles.shrink_to_fit();
	m_bad_particles = 0;
}

void SingleParticle2dx::DataStructures::ParticleContainer::increaseBadParticles()
{
	m_bad_particles++;
}
			

SingleParticle2dx::DataStructures::ParticleContainer::size_type SingleParticle2dx::DataStructures::ParticleContainer::getBadParticles()
{
	return m_bad_particles;
}


void SingleParticle2dx::DataStructures::ParticleContainer::eraseLastElement()
{
//	std::cout << "erase last element called" << std::endl;
	#pragma omp critical (erease_last_element)
	{
		//m_particles.erase(m_particles.end());
//		std::cout << "deleting " << m_particles.size()-1 << " of " << getNumberOfParticles() << std::endl;
		deleteParticle(m_particles.size()-1);
		m_particles.shrink_to_fit();
	}
//	std::cout << "erase last element called (done)" << std::endl;
}


void SingleParticle2dx::DataStructures::ParticleContainer::splitContainer(SingleParticle2dx::DataStructures::ParticleContainer& cont_in, SingleParticle2dx::DataStructures::ParticleContainer& cont_1, SingleParticle2dx::DataStructures::ParticleContainer& cont_2)
{
	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Splitting particle container with " + SingleParticle2dx::Utilities::StringFunctions::TtoString(cont_in.getNumberOfParticles()), 1);

	cont_1.clear();
	cont_2.clear();
	
	typedef boost::mt19937 RNGType;
	RNGType rng(time(0));
	boost::uniform_int<> one_to_two( 1, 2 );
	boost::variate_generator< RNGType, boost::uniform_int<> >dice(rng, one_to_two);
	
	size_type number;
	
	/*
	for (size_type i=cont_in.getNumberOfParticles()-1; i>=0; i--)
	{
		number = dice();
		if ( number%2 == 0 )
		{
			cont_in(cont_in.getNumberOfParticles()-1).setContainerNumber(1);
			cont_1.addParticle(cont_in(cont_in.getNumberOfParticles()-1));
		}
		else
		{
			cont_in(cont_in.getNumberOfParticles()-1).setContainerNumber(2);
			cont_2.addParticle(cont_in(cont_in.getNumberOfParticles()-1));
		}
		cont_in.eraseLastElement();
	}
	*/
	
	for (size_type i=cont_in.getNumberOfParticles()-1; i>=0; i--)
	{
		if ( i%2 == 0 )
		{
			cont_in(cont_in.getNumberOfParticles()-1).setContainerNumber(1);
			cont_1.addParticle(cont_in(cont_in.getNumberOfParticles()-1));
		}
		else
		{
			cont_in(cont_in.getNumberOfParticles()-1).setContainerNumber(2);
			cont_2.addParticle(cont_in(cont_in.getNumberOfParticles()-1));
		}
		cont_in.eraseLastElement();
	}
	
	cont_1.setParticleNumbers();
	cont_2.setParticleNumbers();

	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Generated split container with " + SingleParticle2dx::Utilities::StringFunctions::TtoString(cont_1.getNumberOfParticles()), 1);
	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Generated split container with " + SingleParticle2dx::Utilities::StringFunctions::TtoString(cont_2.getNumberOfParticles()), 1);
	
	SingleParticle2dx::DataStructures::ParticleContainer av1;
	SingleParticle2dx::DataStructures::ParticleContainer av2;
	
	cont_in.clear();
}


void SingleParticle2dx::DataStructures::ParticleContainer::splitContainerMemSaving(SingleParticle2dx::DataStructures::ParticleContainer& cont_in, SingleParticle2dx::DataStructures::ParticleContainer& cont_1, SingleParticle2dx::DataStructures::ParticleContainer& cont_2)
{
	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Splitting particle container with " + SingleParticle2dx::Utilities::StringFunctions::TtoString(cont_in.getNumberOfParticles()), 1);

	cont_1.clear();
	cont_2.clear();
	
	for (size_type i=cont_in.getNumberOfParticles()-1; i>=0; i--)
	{
		if ( i%2 == 0 )
		{
			cont_in(cont_in.getNumberOfParticles()-1).setContainerNumber(1);
			cont_1.addParticle(cont_in(cont_in.getNumberOfParticles()-1));
		}
		else
		{
			cont_in(cont_in.getNumberOfParticles()-1).setContainerNumber(2);
			cont_2.addParticle(cont_in(cont_in.getNumberOfParticles()-1));
		}
		cont_in.eraseLastElement();
	}
	
	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Generated split container with " + SingleParticle2dx::Utilities::StringFunctions::TtoString(cont_1.getNumberOfParticles()), 1);
	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Generated split container with " + SingleParticle2dx::Utilities::StringFunctions::TtoString(cont_2.getNumberOfParticles()), 1);
	
	cont_in.clear();
}


void SingleParticle2dx::DataStructures::ParticleContainer::getParticlePointerVector(size_type cont_number, std::vector<SingleParticle2dx::DataStructures::Particle*>& res)
{
	res.clear();
	for(size_type i=0; i<getNumberOfParticles(); i++)
	{
		if(m_particles[i].getGlobalParticleInformation().getImageNumber() == cont_number)
		{
			res.push_back(&m_particles[i]);
		}
	}
	
	if(static_cast<size_type>(res.size()) == 0)
	{
		std::cerr << "did not find a good number of particle with the right number" << std::endl;
		throw std::runtime_error("Bad operation");
	}
}



void SingleParticle2dx::DataStructures::ParticleContainer::calcAndSetConsistency(size_type number_of_diff_containers)
{
	for(size_type cont_number=0; cont_number<number_of_diff_containers; cont_number++)
	{
		size_type n = 6;
		std::vector<Eigen::VectorXf> points;
		std::vector<Particle*> particles;
		getParticlePointerVector(cont_number, particles);
	
		for (size_type i=0; i<static_cast<size_type>(particles.size()); i++ )
		{
			Eigen::VectorXf new_vec(n);
			new_vec[0] = particles[i]->getNewOrientation().getTLTAXIS();
			new_vec[1] = particles[i]->getNewOrientation().getTLTANG();
			new_vec[2] = particles[i]->getNewOrientation().getTAXA();
			new_vec[3] = particles[i]->getParticleShift().getShiftX();
			new_vec[4] = particles[i]->getParticleShift().getShiftY();
			new_vec[5] = particles[i]->getSimMeasure();
			points.push_back(new_vec);
		}
	
		Eigen::VectorXf mean = Eigen::VectorXf::Zero(n);
	
		for(size_type i=0; i<static_cast<size_type>(points.size()); i++)
		{
			mean += points[i];
		}
		mean /= static_cast<value_type>(points.size());
	
		Eigen::MatrixXf covMat = Eigen::MatrixXf::Zero(n,n);
		for(size_type i=0; i<static_cast<size_type>(points.size()); i++)
		{
			Eigen::VectorXf diff = (points[i]-mean).conjugate();
			covMat += diff * diff.adjoint();
		}
	
		value_type det = covMat.determinant();
		
		//std::cout << "log cons det " << log(det) << std::endl;
		
		value_type result = -0.5 * log(covMat.determinant());
		
		for(size_type i=0; i<static_cast<size_type>(particles.size()); i++)
		{
			Eigen::VectorXf vec(n);
			vec[0] = particles[i]->getNewOrientation().getTLTAXIS();
			vec[1] = particles[i]->getNewOrientation().getTLTANG();
			vec[2] = particles[i]->getNewOrientation().getTAXA();
			vec[3] = particles[i]->getParticleShift().getShiftX();
			vec[4] = particles[i]->getParticleShift().getShiftY();
			vec[5] = particles[i]->getSimMeasure();
			
			Eigen::VectorXf tmp1 = (covMat.inverse()) * (vec-mean);
			value_type tmp2 = (vec-mean).dot(tmp1);
	
			value_type cons = result - 0.5 * tmp2;
			
			particles[i]->setConsistency(cons);
			
			//std::cout << "cons = " << - 0.5 * tmp2 << std::endl;
		}
	}
}


SingleParticle2dx::DataStructures::ParticleContainer::size_type SingleParticle2dx::DataStructures::ParticleContainer::getNumberOfDiffImages()
{
	std::vector<size_type> number_vec;
	for(size_type i=0; i<getNumberOfParticles(); i++)
	{
		number_vec.push_back(m_particles[i].getGlobalParticleInformation().getImageNumber());
	}
	
	std::sort(number_vec.begin(), number_vec.end());
	number_vec.erase(std::unique(number_vec.begin(), number_vec.end()), number_vec.end());
	
	return static_cast<size_type>(number_vec.size());
}


void SingleParticle2dx::DataStructures::ParticleContainer::selectParticlesBasedOnConsistency(size_type number_of_diff_containers)
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	for(size_type cont_number=0; cont_number<number_of_diff_containers; cont_number++)
	{
		std::vector<Particle*> particles;
		getParticlePointerVector(cont_number, particles);
		
		std::vector<float> cons_vec;
		
		for(size_type i=0; i<static_cast<size_type>(particles.size()); i++)
		{
			cons_vec.push_back(particles[i]->getConsistency());
		}
		
		std::sort(cons_vec.begin(), cons_vec.end());
		
		value_type t = cons_vec[static_cast<size_type>((config->getConsT()/100.0) * cons_vec.size())];
		
		for(size_type i=0; i<static_cast<size_type>(particles.size()); i++)
		{
			if(particles[i]->getConsistency() < t)
			{
				particles[i]->setUseForReconstruction(false);
			}
		}
	}
}

void SingleParticle2dx::DataStructures::ParticleContainer::mergeContainers(SingleParticle2dx::DataStructures::ParticleContainer& cont_large, SingleParticle2dx::DataStructures::ParticleContainer& cont_small)
{
	for (size_type i=0; i<cont_small.getNumberOfParticles(); i++)
	{
		cont_large.addParticle(cont_small(i));
	}
	cont_small.clear();
}


void SingleParticle2dx::DataStructures::ParticleContainer::applyMaskToContainer()
{
	for (size_type i=0; i<getNumberOfParticles(); i++)
	{
		m_particles[i].applyMask();
	}
}


void SingleParticle2dx::DataStructures::ParticleContainer::applyMaskToContainerInParallel()
{
	#pragma omp parallel for
	for (size_type i=0; i<getNumberOfParticles(); i++)
	{
		m_particles[i].applyMask();
	}
}


void SingleParticle2dx::DataStructures::ParticleContainer::resetSimForAllParticles()
{
	for (size_type i=0; i<getNumberOfParticles(); i++)
	{
		m_particles[i].setSimMeasure(value_type(1));
	}
}


void SingleParticle2dx::DataStructures::ParticleContainer::resetWeightForAllParticles()
{
	for (size_type i=0; i<getNumberOfParticles(); i++)
	{
		m_particles[i].setWeight(value_type(1));
	}
}


void SingleParticle2dx::DataStructures::ParticleContainer::mergeContainers(SingleParticle2dx::DataStructures::ParticleContainer& cont_in1, SingleParticle2dx::DataStructures::ParticleContainer& cont_in2, SingleParticle2dx::DataStructures::ParticleContainer& cont_out)
{
	//cont_out.clear();
	
	std::cout << "merging container" << std::endl;
	
	for (size_type i=cont_in1.getNumberOfParticles()-1; i>=0; i--)
	{
		std::cout << i << std::endl;
		cont_out.addParticle(cont_in1(cont_in1.getNumberOfParticles()-1));
		cont_in1.eraseLastElement();
	}
//	cont_in1.clear();
	
	for (size_type i=cont_in2.getNumberOfParticles()-1; i>=0; i--)
	{
		std::cout << i << std::endl;
		cont_out.addParticle(cont_in2(cont_in2.getNumberOfParticles()-1));
		cont_in2.eraseLastElement();
	}
//	cont_in2.clear();
	
	cont_out.sortContainer();
	cont_out.setParticleNumbers();
}


SingleParticle2dx::DataStructures::ParticleContainer::size_type SingleParticle2dx::DataStructures::ParticleContainer::pickParticlesFromCCProfile(std::string foldername, SingleParticle2dx::DataStructures::ParticleContainer& cont, SingleParticle2dx::DataStructures::PickingDiagnostics& dia, size_type image_number, bool do_double_pick)
{
	std::vector<std::string> split_vector;
	SingleParticle2dx::Utilities::StringFunctions::splitString( split_vector, foldername, std::string("/") );
	std::string filename_core = split_vector.back();
	SingleParticle2dx::DataStructures::PickingDiagnostics local_dia;
	size_type bad_particles = 0;
	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	std::string mrc_image_name = (foldername + "/m" + filename_core + ".mrc");
	
	if ( boost::filesystem::exists(mrc_image_name) )
	{
		std::cout << "::found a masked image" << std::endl;
	}
	else
	{
		mrc_image_name = (foldername + "/" + filename_core + ".mrc");
		std::cout << "::using non-masked image" << std::endl;
	}
	
	
	std::string profile_name = (foldername + "/cc_profile.dat");
	std::string config_name = (foldername + "/" + "2dx_image.cfg");
	
	SingleParticle2dx::Utilities::ImageConfigReader image_config (config_name);
	real_array_type mrc_input(boost::extents[1][1]);
	
	SingleParticle2dx::DataStructures::ParticleContainer local_cont;
	
	{
		using namespace SingleParticle2dx::Utilities;
		
		UtilityFunctions::generate2dxOutput("Working on: " + foldername, 1);
		UtilityFunctions::generate2dxOutput("image name: " + mrc_image_name, 3);
		UtilityFunctions::generate2dxOutput("profile name: " + profile_name, 3);
		UtilityFunctions::generate2dxOutput("config name: " + config_name, 3);
	}
		
	/** @todo find also masked images */
	SingleParticle2dx::Utilities::MRCFileIO::readFromMrc(mrc_image_name, &mrc_input);
	
	std::string image_copy;
	
	size_type n = mrc_input.shape()[0];
	
	if ( SingleParticle2dx::ConfigContainer::Instance()->getInvertImage() == true )
	{
		SingleParticle2dx::Utilities::DataContainerFunctions::invertData(&mrc_input);
	}
		
	std::vector<value_type> lattice_rec(4,0);
	lattice_rec[0] = image_config.getConfigElement("lattice")[0];
	lattice_rec[1] = image_config.getConfigElement("lattice")[1];
	lattice_rec[2] = image_config.getConfigElement("lattice")[2];
	lattice_rec[3] = image_config.getConfigElement("lattice")[3];
	
	value_type tltaxis = image_config.getConfigElement("TLTAXIS")[0] + 90;
	value_type tltang = image_config.getConfigElement("TANGL")[0];
	value_type taxa = image_config.getConfigElement("TAXA")[0];
	
	/** @todo make it better */
	if ( config->getIsGLPF() )
	{
		if( (tltang<35) && (tltang>25)  )
		{
			SingleParticle2dx::Utilities::DataContainerFunctions::invertData(&mrc_input);
		}
	}

	std::vector<value_type> lattice_real(4,0);
	SingleParticle2dx::DataStructures::ParticleContainer::getRealLattice(lattice_rec,  n, lattice_real);
	
	std::pair<value_type, value_type> phaori;
	phaori.first = image_config.getConfigElement("phaori")[0];
	phaori.second = image_config.getConfigElement("phaori")[1];
	
	value_type x_center = lattice_real[0] * phaori.first/value_type(360) + lattice_real[2] * phaori.second/value_type(360);
	value_type y_center = lattice_real[1] * phaori.first/value_type(360) + lattice_real[3] * phaori.second/value_type(360);
	
	{
		using namespace SingleParticle2dx::Utilities;
		
		UtilityFunctions::generate2dxOutput("tltaxis: " + SingleParticle2dx::Utilities::StringFunctions::TtoString(tltaxis), 3);
		UtilityFunctions::generate2dxOutput("tltang: " + SingleParticle2dx::Utilities::StringFunctions::TtoString(tltang), 3);
		UtilityFunctions::generate2dxOutput("taxa: " + SingleParticle2dx::Utilities::StringFunctions::TtoString(taxa), 3);
		UtilityFunctions::generate2dxOutput("reciprocal lattice: " + SingleParticle2dx::Utilities::StringFunctions::TtoString(lattice_rec[0]) + " " + SingleParticle2dx::Utilities::StringFunctions::TtoString(lattice_rec[1]) + " " + SingleParticle2dx::Utilities::StringFunctions::TtoString(lattice_rec[2]) + " " + SingleParticle2dx::Utilities::StringFunctions::TtoString(lattice_rec[3]), 3);
		UtilityFunctions::generate2dxOutput("real lattice: " + SingleParticle2dx::Utilities::StringFunctions::TtoString(lattice_real[0]) + " " + SingleParticle2dx::Utilities::StringFunctions::TtoString(lattice_real[1]) + " " + SingleParticle2dx::Utilities::StringFunctions::TtoString(lattice_real[2]) + " " + SingleParticle2dx::Utilities::StringFunctions::TtoString(lattice_real[3]), 3);
		UtilityFunctions::generate2dxOutput("phase origin: " + SingleParticle2dx::Utilities::StringFunctions::TtoString(phaori.first) + " " + SingleParticle2dx::Utilities::StringFunctions::TtoString(phaori.second), 3);
		UtilityFunctions::generate2dxOutput("x_center: " + SingleParticle2dx::Utilities::StringFunctions::TtoString(x_center), 2);
		UtilityFunctions::generate2dxOutput("y_center: " + SingleParticle2dx::Utilities::StringFunctions::TtoString(y_center), 2);
	}
	
	std::vector<SingleParticle2dx::Utilities::PeakObject> vec_peak;
	
	{
		using boost::spirit::qi::float_;
		using boost::spirit::qi::char_;
		using boost::spirit::qi::_1;
		using boost::spirit::qi::lit;
		using boost::spirit::qi::phrase_parse;
		using boost::spirit::ascii::space;
		using boost::spirit::ascii::string;
		using boost::phoenix::ref;
		using boost::phoenix::push_back;
		using boost::spirit::no_skip;
		
		boost::shared_ptr<std::ifstream> myfile( new std::ifstream(profile_name.c_str()), SingleParticle2dx::Utilities::CustomizedDeleter() );
		
		std::string line;
		
		value_type offset_x = .5 * ( lattice_real[0] + lattice_real[2] );
		value_type offset_y = .5 * ( lattice_real[1] + lattice_real[3] );
			
		value_type peak, x, y;
		size_type dx = 4*(SingleParticle2dx::ConfigContainer::Instance()->getParticleSize()) / 2 + 200;
		size_type dy = 4*(SingleParticle2dx::ConfigContainer::Instance()->getParticleSize()) / 2 + 200;
		
		if (myfile.get()->is_open())
		{
			// read the first eight useless lines of the cc-profile
			for(size_type i=0; i<8; i++)
			{
				getline ((*myfile.get()),line);
			}
			
			while ( myfile.get()->good() )
			{
				getline ((*myfile.get()),line);
				
				if(phrase_parse(
								line.begin(),
								line.end(),

								(float_-char_("0"))[ref(x) = (_1-x_center)] >>
								float_[ref(y) = (_1-y_center)] >>
								float_[ref(peak) = _1],

								space))
				{
					if ( (x>(dx)) && (x<(n-dx)) && (y>(dy)) && (y<(n-dy)) )
					{
						SingleParticle2dx::Utilities::PeakObject new_peak;
						new_peak.m_x = x;
						new_peak.m_y = y;
						
						if (do_double_pick)
						{
							new_peak.m_x += offset_x;
							new_peak.m_y += offset_y;
						}
						
						new_peak.m_peak = peak;
						vec_peak.push_back(new_peak);
					}
				}
			}
		}
	}
	
	std::sort(vec_peak.begin(), vec_peak.end(), SingleParticle2dx::Utilities::comparePeaks);	
	
	size_type particle_size = config->getParticleSize();
	
	typedef real_array_type::index_range range;
	
	size_type mag = image_config.getConfigElement("magnification")[0];
	value_type scaling_ratio = mag / config->getMaxMag();
	scaling_ratio = 1;
	
	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("scaling factor: " + SingleParticle2dx::Utilities::StringFunctions::TtoString(scaling_ratio), 3);
	SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&mrc_input);

	value_type sd_image = SingleParticle2dx::Utilities::DataContainerFunctions::calculateSD(&mrc_input);
	std::cout << "sd of input image: " << sd_image << std::endl;
	
	real_array_type pos(boost::extents[n][n]);
	int i=0;
	
	value_type cs = image_config.getConfigElement("CS")[0];
	value_type kv = image_config.getConfigElement("KV")[0];
	value_type difmid1 = image_config.getConfigElement("defocus")[0];
	value_type difmid2 = image_config.getConfigElement("defocus")[1];
	value_type angast = image_config.getConfigElement("defocus")[2];
	value_type dstep = image_config.getConfigElement("stepdigitizer")[0];
	size_type size_x = mrc_input.shape()[0];
	size_type size_y = mrc_input.shape()[1];
	
	fft_array_type large_fft(boost::extents[4*particle_size][4*particle_size]);
	size_type particles_to_pick;
	
	if ( config->getParticleSelectionMethod() == 0 )
	{
		particles_to_pick = config->getNumberOfParticles();
	}
	else
	{
		particles_to_pick = floor( config->getPercentageOfParticles() * 0.01 * vec_peak.size() );
	}
	
	if ( particles_to_pick > static_cast<size_type>(vec_peak.size()) )
	{
		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("You are selecting more particles than stored in the profile, thus picking the largest possible number", 1);
		particles_to_pick = static_cast<size_type>(vec_peak.size());
	}
	
	if (do_double_pick)
	{
		std::cout << ": doing double pick" << std::endl;
	}
	
	real_array_type part_data(boost::extents[particle_size][particle_size]);
	real_array_type part_data_large(boost::extents[4*particle_size][4*particle_size]);
	
	for (size_type i=0; i<particles_to_pick; i++)
	{
		SingleParticle2dx::Utilities::CTFParameters ctf_params(cs, kv, difmid1, difmid2, angast, dstep, static_cast<value_type>(mag), tltaxis, tltang, size_x, size_y);
		size_type ii,jj;
		value_type sd_part;

		jj = static_cast<size_type>(floor(vec_peak[i].m_x+0.5));
		ii = static_cast<size_type>(floor(vec_peak[i].m_y+0.5));
		
		ctf_params.m_posx = jj;
		ctf_params.m_posy = ii;
		
		part_data_large = mrc_input[ boost::indices[range(jj-4*particle_size/2, jj+4*particle_size/2)][range(ii-4*particle_size/2, ii+4*particle_size/2)] ];

		sd_part = SingleParticle2dx::Utilities::DataContainerFunctions::calculateSD(&part_data_large);
		
		SingleParticle2dx::DataStructures::CTFParticleInformation ctf_part;

		if (config->getLocalCTFCorrection())
		{
			SingleParticle2dx::Utilities::DataContainerFunctions::correctCTF(&part_data_large, ctf_params, ctf_part);
		}
		else
		{
			SingleParticle2dx::Utilities::DataContainerFunctions::correctCTF(&part_data_large, ctf_params, ctf_part, true);
		}

		if (config->getDoBinning())
		{
			SingleParticle2dx::Utilities::DataContainerFunctions::applyBinning(&part_data_large);
		}
		
		part_data = part_data_large[ boost::indices[range(2*particle_size-particle_size/2, 2*particle_size+particle_size/2)][range(2*particle_size-particle_size/2, 2*particle_size+particle_size/2)] ];
		
		if ( (sd_part<config->getMinStdPicking()) )
		{
			dia.addTooLowSdParticle();
			local_dia.addTooLowSdParticle();
		}
		else if ( !std::isfinite(sd_part) )
		{
			dia.addBadSdParticle();
			local_dia.addBadSdParticle();
		}
		else if ( sd_part < (0.02 * sd_image) )
		{
			dia.addTooLowRelativeSdParticle();
			local_dia.addTooLowRelativeSdParticle();
		}
		else
		{
			SingleParticle2dx::DataStructures::GlobalParticleInformation part_info;
			part_info.setImageNumber(image_number);
			part_info.setPositionX(ii);
			part_info.setPositionY(jj);
			part_info.setImageWidth(n);
			
			if(do_double_pick)
			{
				part_info.setImageName(filename_core + "_dual");
			}
			else
			{
				part_info.setImageName(filename_core);
			}

			value_type tltaxis_, tltang_, taxa_;

			if (do_double_pick)
			{
				tltaxis_ = tltaxis;
				tltang_ = tltang + 180;
				taxa_ = (-taxa) + 90;
			}
			else
			{
				tltaxis_ = tltaxis;
				tltang_ = tltang;
				taxa_ = taxa;
			}

			SingleParticle2dx::DataStructures::Orientation part_orientation(tltaxis_, tltang_, taxa_);

			SingleParticle2dx::DataStructures::Particle part(particle_size, particle_size, part_orientation, part_info);
			
			part.setCTFInformation(ctf_part);
			
		//	std::cout << part.getCTFInfo().getDefocus() << std::endl;
			
			part.setSimMeasure(vec_peak[i].m_peak);

			if ( config->getSymInPlane() )
			{
				if ( fabs(tltang) < config->getMaxInPlaneTltang() )
	    		{
					#pragma omp critical (eman_sym)
					{
	    				size_type size = particle_size * particle_size;
	    				float* float_data_2d;
	    				float_data_2d = (float*) malloc(size * sizeof(float));

	    				std::copy(part_data.origin(), part_data.origin()+part_data.num_elements(), float_data_2d );

	    				EMAN::EMData* eman_data = new EMAN::EMData;
	    				eman_data->set_data(float_data_2d, particle_size, particle_size, 1);	
					
	    				eman_data = eman_data->symvol(config->getSymmetryString().c_str());
	
						std::copy(eman_data->get_data(), eman_data->get_data()+size, part_data.origin() );
					}
	    		}
			}
			
			part.setFourierSpaceData(&part_data);

			if ( config->getIsGLPF() )
			{
				if ( fabs(tltang) < 5)
				{
					part.flipXAxis();
				}

				if ( fabs(tltang-30.) < 5)
				{
					//part.flipYAxis();
					//part.flipXAxis();		
				}

				if ( fabs(tltang-45.) < 5)
				{
					//part.flipYAxis();
					//part.flipXAxis();
				}
			}

			part.applyHighPassFilter();
			part.scale(scaling_ratio);
			
			if (config->getEqualizeHistograms())
			{
				part.equalizeHistogram();
			}
			
			
			part.applyMask();
			part.normalizeRealSpace();
			
			if ( part.checkParticle() )
			{
				
				if ( std::isnan(sd_part) || (!std::isfinite(sd_part)) )
				{
					dia.addBadSdParticle();
					local_dia.addBadSdParticle();
				}
				else
				{
					dia.addGoodParticle();
					local_dia.addGoodParticle();
					
					cont.addParticleFast(part);
					local_cont.addParticleFast(part);
				}
			}
			else 
			{
				dia.addFailingParticle();
				local_dia.addFailingParticle();
				
				#pragma omp critical (dont_insert_part)
				{
					cont.increaseBadParticles();
				}
			}
		}
	}
	
	std::string cont_path = config->getContainerName();
	std::string container_out_ave = cont_path + "/Particles/Picking_output/" + filename_core;
	std::cout << "cont_out debug " << container_out_ave << std::endl;
	
	if (!do_double_pick)
	{
		if ( cont.getNumberOfParticles() > 10 )
		{
			local_cont.writeContainerToDisk(container_out_ave, 0);
		}
		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Picking Diagnosics for " + filename_core + ":", 2);
	}
	else
	{
		if ( cont.getNumberOfParticles() > 10 )
		{
			local_cont.writeContainerToDisk((container_out_ave + "_dual"), 0);
		}
		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Picking Diagnosics for " + filename_core + " (dual):", 2);
		
	}
	
	local_dia.print(2);
	
	return local_dia.getNumberOfGoodParticles();
}


void SingleParticle2dx::DataStructures::ParticleContainer::getRealLattice(std::vector<value_type>& rec_lat, size_type n, std::vector<value_type>& real_lat)
{
	value_type a1, a2, b1, b2;
	
	a1 = rec_lat[0];
	a2 = rec_lat[1];
	b1 = rec_lat[2];
	b2 = rec_lat[3];
	
	value_type det = a1*b2 - b1*a2;
	
	real_lat[0] = b2 * n / det;
	real_lat[1] = -b1 * n / det;
	real_lat[2] = -a2 * n / det;
	real_lat[3] = a1 * n / det;
}


std::pair<std::vector<SingleParticle2dx::DataStructures::ParticleContainer::value_type>, std::vector<SingleParticle2dx::DataStructures::ParticleContainer::value_type> > SingleParticle2dx::DataStructures::ParticleContainer::calculateFSC( SingleParticle2dx::DataStructures::ParticleContainer& cont, std::string even_name, std::string odd_name)
{
	std::cout.flush();
	std::cout << "calculating resolution...\n";
	std::cout.flush();
	
	std::cout << "name even = " << even_name << " " <<  even_name.length() << std::endl;
	std::cout << "name odd  = " << odd_name << " " <<  odd_name.length() << std::endl;
	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	size_type n = config->getParticleSize();
	SingleParticle2dx::DataStructures::ParticleContainer cont_even;
	SingleParticle2dx::DataStructures::ParticleContainer cont_odd;
		
	SingleParticle2dx::DataStructures::ParticleContainer::splitContainer(cont, cont_even, cont_odd);
	
	SingleParticle2dx::DataStructures::Reconstruction3d rec_even(n, n, n);
	SingleParticle2dx::DataStructures::Reconstruction3d rec_odd(n, n, n);
	
	rec_even.generateInitialModel(cont_even);
	rec_odd.generateInitialModel(cont_odd);
	
	if ( (even_name.length()>0) && (odd_name.length()>0) )
	{
		rec_even.writeToFile( even_name );
		rec_odd.writeToFile( odd_name );
		
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(even_name, "MAP: even", config->getScriptName(), false);
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(odd_name, "MAP: odd", config->getScriptName(), false);
	}
	
	return (calculateFSC( rec_even, rec_odd ));
}

std::pair<std::vector<SingleParticle2dx::DataStructures::ParticleContainer::value_type>, std::vector<SingleParticle2dx::DataStructures::ParticleContainer::value_type> > SingleParticle2dx::DataStructures::ParticleContainer::calculateFSC( SingleParticle2dx::DataStructures::Reconstruction3d& rec1, SingleParticle2dx::DataStructures::Reconstruction3d& rec2, std::string outfile, bool dosmoothing )
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	size_type n = config->getParticleSize();
	
	typedef std::vector<value_type> Vector;
	Vector r_values;
	
	for (value_type r = config->getFSCrmin()-1; r <= config->getFSCrmax(); r += config->getFSCdr())
	{
		r_values.push_back(r);
	}
	
	std::vector<fft_type> sum1(r_values.size(), fft_type(0,0));
	Vector sum2(r_values.size(), value_type(0));
	Vector sum3(r_values.size(), value_type(0));
	
	value_type r_ijk;
	fft_type f1, f2;
	for (size_type i=0; i<n; i++)
	{
		for (size_type j=0; j<n; j++)
		{
			for (size_type k=0; k<n; k++)
			{
				r_ijk = sqrt( (i-n/2)*(i-n/2) + (j-n/2)*(j-n/2) + (k-n/2)*(k-n/2) );
				for (size_type r=1; r<static_cast<size_type>(r_values.size()); r++)
				{
					f1 = rec1(i,j,k);
					f2 = rec2(i,j,k);
					if( (r_ijk <= r_values[r]) && (r_ijk > r_values[r-1]) )
					{
						sum1[r] += f1 * conj(f2);
						sum2[r] += abs(f1) * abs(f1);
						sum3[r] += abs(f2) * abs(f2);
					}
				}
			}
		}
	}
	
	std::string filename = outfile;
	boost::shared_ptr<FILE> file( fopen ( filename.c_str(), "w" ), SingleParticle2dx::Utilities::CustomizedDeleter() );
	
	Vector fsc(r_values.size(), value_type(0));
	
	for (size_type r=0; r<static_cast<size_type>(r_values.size()); r++)
	{
		if (r==0)
		{
			fsc[r] = 1;
		}
		else
		{
			fsc[r] = abs(sum1[r]) / sqrt( sum2[r] * sum3[r] );
		}
	}
	
	std::vector<value_type> smoothed_fsc;
	if ( dosmoothing )
	{
		for (size_type r=0; r<static_cast<size_type>(r_values.size()); r++)
		{
			if ( (r==0) || (r==static_cast<size_type>(r_values.size()-1)) )
			{
				smoothed_fsc.push_back(fsc[r]);
			}
			else
			{
				smoothed_fsc.push_back((fsc[r-1]+fsc[r]+fsc[r+1])/3);
			}
		}
	}
	
	if ( dosmoothing && fsc.size() != smoothed_fsc.size() )
	{
		std::cerr << "smoothing the fsc did not work" << std::endl;
		throw std::runtime_error("Bad operation");
	}
	
	if ( dosmoothing )
	{
		for (size_type r=0; r<static_cast<size_type>(fsc.size()); r++)
		{
			fprintf(file.get(), "%f\t%f\n", r_values[r], smoothed_fsc[r]);
		}
	}
	else
	{
		for (size_type r=0; r<static_cast<size_type>(fsc.size()); r++)
		{
			fprintf(file.get(), "%f\t%f\n", r_values[r], fsc[r]);
		}
	}
	
	std::pair<Vector, Vector> result(r_values, fsc);
	if ( dosmoothing )
	{
		result.second = smoothed_fsc;
	}
	
	
	return result;
}


std::pair<std::vector<SingleParticle2dx::DataStructures::ParticleContainer::value_type>, std::vector<SingleParticle2dx::DataStructures::ParticleContainer::value_type> > SingleParticle2dx::DataStructures::ParticleContainer::calculateFSC_XY( SingleParticle2dx::DataStructures::Reconstruction3d& rec1, SingleParticle2dx::DataStructures::Reconstruction3d& rec2, bool dosmoothing )
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	size_type n = config->getParticleSize();
	
	typedef std::vector<value_type> Vector;
	Vector r_values;
	
	for (value_type r = config->getFSCrmin()-1; r <= config->getFSCrmax(); r += config->getFSCdr())
	{
		r_values.push_back(r);
	}
	
	std::vector<fft_type> sum1(r_values.size(), fft_type(0,0));
	Vector sum2(r_values.size(), value_type(0));
	Vector sum3(r_values.size(), value_type(0));
	
	value_type r_jk;
	fft_type f1, f2;
	for (size_type i=0; i<n; i++)
	{
		for (size_type j=0; j<n; j++)
		{
			for (size_type k=0; k<n; k++)
			{
				r_jk = sqrt( (j-n/2)*(j-n/2) + (k-n/2)*(k-n/2) );
				for (size_type r=1; r<static_cast<size_type>(r_values.size()); r++)
				{
					f1 = rec1(i,j,k);
					f2 = rec2(i,j,k);
					if( (r_jk <= r_values[r]) && (r_jk > r_values[r-1]) )
					{
						sum1[r] += f1 * conj(f2);
						sum2[r] += abs(f1) * abs(f1);
						sum3[r] += abs(f2) * abs(f2);
					}
				}
			}
		}
	}
	
	std::string filename = "FSC_XY.dat";
	boost::shared_ptr<FILE> file( fopen ( filename.c_str(), "w" ), SingleParticle2dx::Utilities::CustomizedDeleter() );
	
	Vector fsc(r_values.size(), value_type(0));
	
	for (size_type r=0; r<static_cast<size_type>(r_values.size()); r++)
	{
		if (r==0)
		{
			fsc[r] = 1;
		}
		else
		{
			fsc[r] = abs(sum1[r]) / sqrt( sum2[r] * sum3[r] );
		}
	}
	
	std::vector<value_type> smoothed_fsc;
	if ( dosmoothing )
	{
		for (size_type r=0; r<static_cast<size_type>(r_values.size()); r++)
		{
			if ( (r==0) || (r==static_cast<size_type>(r_values.size()-1)) )
			{
				smoothed_fsc.push_back(fsc[r]);
			}
			else
			{
				smoothed_fsc.push_back((fsc[r-1]+fsc[r]+fsc[r+1])/3);
			}
		}
	}
	
	if ( dosmoothing && fsc.size() != smoothed_fsc.size() )
	{
		std::cerr << "smoothing the fsc did not work" << std::endl;
		throw std::runtime_error("Bad operation");
	}
	
	if ( dosmoothing )
	{
		for (size_type r=0; r<static_cast<size_type>(fsc.size()); r++)
		{
			fprintf(file.get(), "%f\t%f\n", r_values[r], smoothed_fsc[r]);
		}
	}
	else
	{
		for (size_type r=0; r<static_cast<size_type>(fsc.size()); r++)
		{
			fprintf(file.get(), "%f\t%f\n", r_values[r], fsc[r]);
		}
	}
	
	std::pair<Vector, Vector> result(r_values, fsc);
	if ( dosmoothing )
	{
		result.second = smoothed_fsc;
	}
	
	return result;
}


std::pair<std::vector<SingleParticle2dx::DataStructures::ParticleContainer::value_type>, std::vector<SingleParticle2dx::DataStructures::ParticleContainer::value_type> > SingleParticle2dx::DataStructures::ParticleContainer::calculateFSC_Z( SingleParticle2dx::DataStructures::Reconstruction3d& rec1, SingleParticle2dx::DataStructures::Reconstruction3d& rec2, bool dosmoothing )
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	size_type n = config->getParticleSize();
	
	typedef std::vector<value_type> Vector;
	Vector r_values;
	
	for (value_type r = config->getFSCrmin()-1; r <= config->getFSCrmax(); r += config->getFSCdr())
	{
		r_values.push_back(r);
	}
	
	std::vector<fft_type> sum1(r_values.size(), fft_type(0,0));
	Vector sum2(r_values.size(), value_type(0));
	Vector sum3(r_values.size(), value_type(0));
	
	value_type r_i;
	fft_type f1, f2;
	for (size_type i=0; i<n; i++)
	{
		for (size_type j=0; j<n; j++)
		{
			for (size_type k=0; k<n; k++)
			{
				r_i = sqrt( (i-n/2)*(i-n/2) );
				for (size_type r=1; r<static_cast<size_type>(r_values.size()); r++)
				{
					f1 = rec1(i,j,k);
					f2 = rec2(i,j,k);
					if( (r_i <= r_values[r]) && (r_i > r_values[r-1]) )
					{
						sum1[r] += f1 * conj(f2);
						sum2[r] += abs(f1) * abs(f1);
						sum3[r] += abs(f2) * abs(f2);
					}
				}
			}
		}
	}
	
	std::string filename = "FSC_Z.dat";
	boost::shared_ptr<FILE> file( fopen ( filename.c_str(), "w" ), SingleParticle2dx::Utilities::CustomizedDeleter() );
	
	Vector fsc(r_values.size(), value_type(0));
	
	for (size_type r=0; r<static_cast<size_type>(r_values.size()); r++)
	{
		if (r==0)
		{
			fsc[r] = 1;
		}
		else
		{
			fsc[r] = abs(sum1[r]) / sqrt( sum2[r] * sum3[r] );
		}
	}
	
	std::vector<value_type> smoothed_fsc;
	if ( dosmoothing )
	{
		for (size_type r=0; r<static_cast<size_type>(r_values.size()); r++)
		{
			if ( (r==0) || (r==static_cast<size_type>(r_values.size()-1)) )
			{
				smoothed_fsc.push_back(fsc[r]);
			}
			else
			{
				smoothed_fsc.push_back((fsc[r-1]+fsc[r]+fsc[r+1])/3);
			}
		}
	}
	
	if ( dosmoothing && fsc.size() != smoothed_fsc.size() )
	{
		std::cerr << "smoothing the fsc did not work" << std::endl;
		throw std::runtime_error("Bad operation");
	}
	
	if ( dosmoothing )
	{
		for (size_type r=0; r<static_cast<size_type>(fsc.size()); r++)
		{
			fprintf(file.get(), "%f\t%f\n", r_values[r], smoothed_fsc[r]);
		}
	}
	else
	{
		for (size_type r=0; r<static_cast<size_type>(fsc.size()); r++)
		{
			fprintf(file.get(), "%f\t%f\n", r_values[r], fsc[r]);
		}
	}
	
	std::pair<Vector, Vector> result(r_values, fsc);
	if ( dosmoothing )
	{
		result.second = smoothed_fsc;
	}
	
	return result;
}



void SingleParticle2dx::DataStructures::ParticleContainer::writeStatToFile(std::string filename, bool append)
{
	boost::shared_ptr<FILE> file;
	
	if(append)
	{
		file.reset( fopen ( filename.c_str(), "a" ), SingleParticle2dx::Utilities::CustomizedDeleter() );
	}
	else
	{
		file.reset( fopen ( filename.c_str(), "w" ), SingleParticle2dx::Utilities::CustomizedDeleter() );
		fprintf(file.get(), "%s\n", m_particles[0].getDescString().c_str());
	}
	
	for ( size_type i=0; i<getNumberOfParticles(); i++)
	{
		std::string data_string = m_particles[i].getDataString();
		fprintf(file.get(), "%s\n", data_string.c_str());
	}
}


void SingleParticle2dx::DataStructures::ParticleContainer::getAverage(real_array_type& ave_im)
{
	SingleParticle2dx::Utilities::DataContainerFunctions::resetData(&ave_im);
	
	real_array_type temp_im ( boost::extents[m_particles[0].getSizeX()][m_particles[0].getSizeY()] );
	size_type count = 0;
	
	for (size_type i=0; i<getNumberOfParticles(); i++)
	{
		if(m_particles[i].getUseForReconstruction())
		{
			count++;
			m_particles[i].getRealSpaceData(&temp_im);
			for(size_type ii=0; ii<m_particles[0].getSizeX(); ii++)
			{
				for(size_type jj=0; jj<m_particles[0].getSizeY(); jj++)
				{
					ave_im[ii][jj] += temp_im[ii][jj];
				}
			}
		}
	}
	
	for(size_type ii=0; ii<m_particles[0].getSizeX(); ii++)
	{
		for(size_type jj=0; jj<m_particles[0].getSizeY(); jj++)
		{
			ave_im[ii][jj] /= count;
		}
	}
	
}


void SingleParticle2dx::DataStructures::ParticleContainer::sortContainer()
{
	std::sort(m_particles.begin(), m_particles.end(), SingleParticle2dx::DataStructures::compareParticles);
}


void SingleParticle2dx::DataStructures::ParticleContainer::deleteParticle(size_type index)
{
	//std::cout << "deleting " << index << " of " << getNumberOfParticles() << std::endl;
	m_particles.erase(m_particles.begin()+index);
	//std::cout << "deleting done" << std::endl;
}


void SingleParticle2dx::DataStructures::ParticleContainer::deleteParticle(data_array_type::iterator it)
{
	m_particles.erase(it);
}


void SingleParticle2dx::DataStructures::ParticleContainer::deleteMarkedParticles()
{
	return;
}


void SingleParticle2dx::DataStructures::ParticleContainer::deleteImage(size_type image_number)
{
	// BROKEN
	data_array_type::iterator it = m_particles.begin();
	size_type count = 0;
	
	while(it != m_particles.end())
	{
		std::cout << "number: " << (*it).getGlobalParticleInformation().getImageNumber() <<  std::endl;
		std::cout << "delete image: " << count << std::endl;
		if ((*it).getGlobalParticleInformation().getImageNumber() == image_number)
		{
			//deleteParticle(count);
			count++;
		}
		else
		{
			count++;
		}
		boost::next(it);
	}
}


void SingleParticle2dx::DataStructures::ParticleContainer::updateUseForReconstructionBasedOnWeights()
{
	boost::accumulators::accumulator_set<value_type, boost::accumulators::stats<boost::accumulators::tag::min, boost::accumulators::tag::max> > acc;
	
	for (size_type i=0; i<getNumberOfParticles(); i++)
	{
		acc(m_particles[i].getWeight());
	}
		
	value_type min_w = boost::accumulators::extract_result<boost::accumulators::tag::min>(acc);
	value_type max_w = boost::accumulators::extract_result<boost::accumulators::tag::max>(acc);
	value_type diff = max_w - min_w;
	value_type t = min_w + 0.1*diff;
	t = 0.0;
	
	for (size_type i=0; i<getNumberOfParticles(); i++)
	{
		if ( m_particles[i].getWeight() < t )
		{
			std::cout << "update t=" << t << " w=" << m_particles[i].getWeight() << std::endl;
			m_particles[i].setUseForReconstruction(false);
		}
		
		//if ( m_particles[i].getRejectTreeSplitting() )
		//{
		//	m_particles[i].setUseForReconstruction(false);
		//}
		
	//	m_particles[i].setUseForReconstruction(true);
	//	m_particles[i].setWeight(1.0);
	}
}


SingleParticle2dx::DataStructures::ParticleContainer::size_type SingleParticle2dx::DataStructures::ParticleContainer::getNumberOfClasses()
{
	std::vector<size_type> class_vector;
	for(int i=0; i<getNumberOfParticles(); i++)
	{
	//	std::cout << m_particles[i].getClassNumber() << std::endl;
		class_vector.push_back(m_particles[i].getClassNumber());
	}
	
	std::sort(class_vector.begin(), class_vector.end());
    class_vector.erase(std::unique(class_vector.begin(), class_vector.end()), class_vector.end());

	
	return static_cast<size_type>(class_vector.size());
}


void SingleParticle2dx::DataStructures::ParticleContainer::splitContainerAccordingToClasses(SingleParticle2dx::DataStructures::ParticleContainer cont, std::vector<SingleParticle2dx::DataStructures::ParticleContainer>& cont_vec)
{
	size_type current_class_number = cont(0).getClassNumber();
	cont_vec.clear();
	
	for(size_type i=0; i<cont.getNumberOfClasses(); i++)
	{
		SingleParticle2dx::DataStructures::ParticleContainer cont_new;
		cont_vec.push_back(cont_new);
	}
		
	for(int i=cont.getNumberOfParticles()-1; i>=0; i--)
	{
		cont_vec[cont(i).getClassNumber()].addParticle(cont(i));
		//cont.eraseLastElement();
	}
}


void SingleParticle2dx::DataStructures::ParticleContainer::getNumberOfElementsInEachClass( std::vector<size_type>& vec )
{
	vec.clear();
	size_type number_of_diff_classes = getNumberOfClasses();
	vec = std::vector<size_type>(number_of_diff_classes, 0);
	
	
	for(int i=0; i<getNumberOfParticles(); i++)
	{
		vec[m_particles[i].getClassNumber()]++;
	}
}


SingleParticle2dx::DataStructures::ParticleContainer::size_type SingleParticle2dx::DataStructures::ParticleContainer::getNumberOfWrongClasses()
{
	size_type wrong_count = 0;
	SingleParticle2dx::DataStructures::ClassInformation ci;
	for(int i=0; i<getNumberOfParticles(); i++)
	{
		ci = m_particles[i].getClassInformation();
		if ( ci.getClass() != ci.getTrueClass() )
		{
			wrong_count++;
		}
	}
	return wrong_count;
}


void SingleParticle2dx::DataStructures::ParticleContainer::printClassMembers()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	std::vector<size_type> class_vector;
	for(int i=0; i<getNumberOfParticles(); i++)
	{
		class_vector.push_back(m_particles[i].getTrueClassNumber());
	}
	
	for(size_type i=0; i<config->getNumberOfClasses(); i++)
	{
		std::cout << ":\tclass number: " << i << "\t" << std::count(class_vector.begin(), class_vector.end(), i) << std::endl;
	}
}


void SingleParticle2dx::DataStructures::ParticleContainer::setRandomClasses()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	//boost::random::mt19937 rng_rec(time(0));
	boost::random::mt19937 rng_rec(17);
	boost::random::uniform_int_distribution<> die_rec(0, config->getNumberOfClasses()-1);
	
	for(size_type i=0; i<getNumberOfParticles(); i++)
	{
		m_particles[i].setClassNumber(die_rec(rng_rec));
	}
}


void SingleParticle2dx::DataStructures::ParticleContainer::setModuloClasses()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	size_type nc = config->getNumberOfClasses();
	
	for(size_type i=0; i<getNumberOfParticles(); i++)
	{
		m_particles[i].setClassNumber(i%nc);
	}
}


SingleParticle2dx::DataStructures::ParticleContainer::size_type SingleParticle2dx::DataStructures::ParticleContainer::MRAClassify(std::vector<SingleParticle2dx::DataStructures::Reconstruction3d>& rec_vec)
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	size_type n = config->getParticleSize();
	size_type nc = config->getNumberOfClasses();
	size_type cc_size = config->getCrossCorrelationWindowSize();
	
	boost::scoped_ptr<SingleParticle2dx::Methods::AbstractPeakSearchMethod> peaksearch_strategy;
	boost::scoped_ptr<SingleParticle2dx::Methods::AbstractCalcCCMethod> calc_cc_strategy;
	
	peaksearch_strategy.reset( new SingleParticle2dx::Methods::MaxSearchMethod );
	calc_cc_strategy.reset( new SingleParticle2dx::Methods::BasicCalcCCMethod );
	
	size_type total_change = 0;
	
	#pragma omp parallel for reduction(+:total_change)
	for(size_type i=0; i<getNumberOfParticles(); i++)
	{
		real_array2d_type cc_data( boost::extents[cc_size][cc_size] );
		SingleParticle2dx::DataStructures::Projection2d proj(n,n);
		SingleParticle2dx::DataStructures::Particle p;
		SingleParticle2dx::DataStructures::Orientation o;
		SingleParticle2dx::DataStructures::ParticleShift current_shift;
		value_type current_cc_value;
		real_array2d_type part_sum( boost::extents[n][n] );
		real_array2d_type part_tmp( boost::extents[n][n] );
		
		p = m_particles[i];
		o = p.getNewOrientation();
		p.getRealSpaceData(&part_sum);
		
		std::vector<value_type> cc_vec;
		std::vector<SingleParticle2dx::DataStructures::Particle*> n_vec = p.getNeighbors();
		
		p.calculateDistances();
		std::vector<value_type> dis_vec = p.getDistances();
        
		value_type sigma = SingleParticle2dx::Utilities::UtilityFunctions::calculateCorrespondingSigma(0.25, p.getMaxDistance());
		value_type weight;
		SingleParticle2dx::DataStructures::Particle p2add;
		
		SingleParticle2dx::DataStructures::Particle p_sum = p;
		
		
		if(config->getMaskEllipseA())
		{
			for (size_type ii=0; ii<static_cast<size_type>(n_vec.size()); ii++)
			{
				n_vec[ii]->getRealSpaceData(&part_tmp);
				weight = 1;
        
				if ( (weight<=0) || (weight>1) )
				{
					std::cerr << "weight = " << weight << " which can't be (sigma=" << sigma << ", r=" << dis_vec[ii] << ")" << std::endl;
					throw std::runtime_error("Bad operation");
				}
				SingleParticle2dx::Utilities::DataContainerFunctions::addRealData(&part_tmp, weight, &part_sum);
			}
			p_sum.setFourierSpaceData(&part_sum);
		}
				
		for(size_type c=0; c<nc; c++)
		{
			rec_vec[c].calculateProjection(o, proj);
			calc_cc_strategy.get()->calculateCrossCorrelation( p_sum, proj, cc_data);
			current_cc_value = peaksearch_strategy.get()->findMaximalElementAndSetShift(cc_data, current_shift);
			cc_vec.push_back(current_cc_value);
		}
		
	//	#pragma omp critical (aflalfksdlf)
	//	{
	//		for(size_type index=0; index<static_cast<size_type>(cc_vec.size()); index++)
	//		{
	//			std::cout << cc_vec[index] << "\t";
	//		}
	//		std::cout << std::endl;
	//	}
		
		size_type max_index = std::distance(cc_vec.begin(), std::max_element(cc_vec.begin(), cc_vec.end()));
		
		if( max_index != m_particles[i].getClassNumber() )
		{
			total_change++;
		}
		
		m_particles[i].setClassNumber(max_index);
	}
	return total_change;
}


void SingleParticle2dx::DataStructures::ParticleContainer::updateUseForReconstruction()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	size_type forced = 0;
	size_type sim = 0;
	size_type shift = 0;
	size_type qual = 0;
	
	size_type n = static_cast<size_type>((config->getImageDirectories()).size());
	
	if(config->getDoDoublePick())
	{
		n *= 2;
	}
	
	for(size_type image_number=0; image_number<n; image_number++)
	{
		std::cout << image_number << std::endl;
		boost::accumulators::accumulator_set<value_type, boost::accumulators::stats<boost::accumulators::tag::mean, boost::accumulators::tag::variance(boost::accumulators::lazy)> > acc;

		for (size_type i=0; i<getNumberOfParticles(); i++)
		{
			if ( m_particles[i].getGlobalParticleInformation().getImageNumber() == image_number)
			{
				if ( !m_particles[i].getForceDontUse() )
				{
					acc(m_particles[i].getSimMeasure());
				}
			}
		}

		value_type threshold = boost::accumulators::extract_result<boost::accumulators::tag::mean>(acc) - 1.5 * sqrt(boost::accumulators::extract_result<boost::accumulators::tag::variance>(acc));
		size_type max_shift = SingleParticle2dx::ConfigContainer::Instance()->getCrossCorrelationWindowSize() / 2;

		for (size_type i=0; i<getNumberOfParticles(); i++)
		{
			if ( m_particles[i].getGlobalParticleInformation().getImageNumber() == image_number)
			{
				if ( m_particles[i].getForceDontUse() )
				{
					m_particles[i].setUseForReconstruction(false);
					forced++;
					continue;
				}

				if(m_particles[i].getSimMeasure() < threshold)
				{
					m_particles[i].setUseForReconstruction(false);
					sim++;
					continue;
				}

				if ( fabs(m_particles[i].getParticleShift().getShiftX()) == max_shift ||  fabs(m_particles[i].getParticleShift().getShiftY()) == max_shift )
				{
					m_particles[i].setUseForReconstruction(false);
					shift++;
					continue;
				}

				if ( m_particles[i].getQuality() < 0.9f )
				{
					m_particles[i].setUseForReconstruction(false);
					qual++;
					continue;
				}
				
				m_particles[i].setUseForReconstruction(true);	
				
			}	
		}
	}
	
	size_type good_counter = 0;
	for (size_type i=0; i<getNumberOfParticles(); i++)
	{
		if (m_particles[i].getUseForReconstruction())
		{
			good_counter++;
		}
	}
	
	std::cout << ":: Good percents: " << static_cast<value_type>(good_counter)/getNumberOfParticles() << std::endl;
	std::cout << "::" << forced << " particles removed because of forced decision" << std::endl;
	std::cout << "::" << sim << " particles removed because of similarity" << std::endl;
	std::cout << "::" << shift << " particles removed because of shift" << std::endl;
	std::cout << "::" << qual << " particles removed because of quality" << std::endl;
}


void SingleParticle2dx::DataStructures::ParticleContainer::resetImageNumber(size_type image_number)
{
	for(size_type i=0; i<static_cast<size_type>(m_particles.size()); i++)
	{
		m_particles[i].getGlobalParticleInformation().setImageNumber(image_number);
	}
}



void SingleParticle2dx::DataStructures::ParticleContainer::addRandomNoiseToContainer(value_type sd)
{
	boost::variate_generator<boost::mt19937, boost::normal_distribution<> > generator(boost::mt19937(time(0)), boost::normal_distribution<>());
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	size_type n = config->getParticleSize();
	
	real_array_type rdata ( boost::extents[n][n] );
	int ii, jj;
	value_type rand;
	
	for(int i=0; i<getNumberOfParticles(); i++)
	{
		m_particles[i].getRealSpaceData(&rdata);
		for(ii=0; ii<n; ii++)
		{
			for(jj=0; jj<n; jj++)
			{
				rand = generator();
				rdata[ii][jj] += sd*rand;
			}
		}
		m_particles[i].setFourierSpaceData(&rdata);
	}
	
}


void SingleParticle2dx::DataStructures::ParticleContainer::findNeighbors(size_type n, bool local_container_only)
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	if(n==1)
	{
		local_container_only = true;
	}
	
	size_type np = getNumberOfParticles();
	size_type num_neighbors = config->getNNNumber();
	
	#pragma omp parallel for
	for(size_type i=0; i<n; i++)
	{
		std::vector<SingleParticle2dx::DataStructures::Particle*> part_vec;
		std::vector<SingleParticle2dx::DataStructures::Particle*> part_vec_orig;
		
		if(local_container_only)
		{
			i = m_particles[0].getGlobalParticleInformation().getImageNumber();
		}
	
		for(size_type j=0; j<np; j++)
		{
			if( m_particles[j].getGlobalParticleInformation().getImageNumber() == i )
			{
				part_vec.push_back(&m_particles[j]);
				part_vec_orig.push_back(&m_particles[j]);
			}
		}
		
		if ( (num_neighbors+1) > static_cast<size_type>(part_vec.size()) )
		{
			std::cerr << "not enough particles in the container to calculate the neighbors (" << (num_neighbors+1) << " > " << static_cast<size_type>(part_vec.size()) << ")" << std::endl;
			throw std::runtime_error("Bad operation");
		}
		
		for(size_type j=0; j<static_cast<size_type>(part_vec.size()); j++)
		{						
			part_vec_orig[j]->resetNeighbors();
			SingleParticle2dx::Utilities::ParticleDiffComp comperator( part_vec_orig[j]->getGlobalParticleInformation().getPositionX(), part_vec_orig[j]->getGlobalParticleInformation().getPositionY() );
			std::sort (part_vec.begin(), part_vec.end(), comperator);
		
			for (size_type k=1; k<(num_neighbors+1); k++)
			{
				part_vec_orig[j]->addNeighbor(part_vec[k]);
			}
			part_vec_orig[j]->calculateDistances();
		}	
	}
}


void SingleParticle2dx::DataStructures::ParticleContainer::findNeighborsIncludeClasses(size_type n)
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	std::cout << ":MRA NN-search started" << std::endl;
		
	size_type np = getNumberOfParticles();
	size_type num_neighbors = config->getNNNumber();

	for(size_type i=0; i<n; i++)
	{
		for(size_type c=0; c<config->getNumberOfClasses(); c++)
		{
			std::vector<SingleParticle2dx::DataStructures::Particle*> part_vec;
			std::vector<SingleParticle2dx::DataStructures::Particle*> part_vec_orig;
		
			for(size_type j=0; j<np; j++)
			{
				if( (m_particles[j].getGlobalParticleInformation().getImageNumber()==i) && (m_particles[j].getClassNumber()==c) )
				{
					part_vec.push_back(&m_particles[j]);
					part_vec_orig.push_back(&m_particles[j]);
				}
			}
		
			if ( (num_neighbors+1) > static_cast<size_type>(part_vec.size()) )
			{
				//std::cerr << "not enough particles in the container to calculate the neighbors (" << (num_neighbors+1) << " > " << static_cast<size_type>(part_vec.size()) << ")" << std::endl;
				//throw std::runtime_error("Bad operation");
				for(size_type j=part_vec.size(); j<num_neighbors; j++)
				{
					part_vec.push_back(&m_particles[j]);
					part_vec_orig.push_back(&m_particles[j]);
				}
			}
		
			for(size_type j=0; j<static_cast<size_type>(part_vec.size()); j++)
			{						
				part_vec_orig[j]->resetNeighbors();
				SingleParticle2dx::Utilities::ParticleDiffComp comperator( part_vec_orig[j]->getGlobalParticleInformation().getPositionX(), part_vec_orig[j]->getGlobalParticleInformation().getPositionY() );
				std::sort (part_vec.begin(), part_vec.end(), comperator);
	
				for (size_type k=1; k<(num_neighbors+1); k++)
				{
					part_vec_orig[j]->addNeighbor(part_vec[k]);
				}
				part_vec_orig[j]->calculateDistances();
			}
		}
	}
	std::cout << ":MRA NN-search done" << std::endl;
}


void SingleParticle2dx::DataStructures::ParticleContainer::shakeContainer(value_type range)
{
	boost::mt19937 baseGen(time(0));
	// Define distribution U[0,1) [double values]
	boost::uniform_real<> uniDblUnit(-1,1);
	// Define a random variate generator using our base generator and distribution
	boost::variate_generator<boost::mt19937&, boost::uniform_real<> > generator(baseGen, uniDblUnit);
	
	SingleParticle2dx::DataStructures::Orientation o;
	
	for (size_type i=0; i<getNumberOfParticles(); i++)
	{
		o = m_particles[i].getNewOrientation();
		o.setTLTAXIS(o.getTLTAXIS() + generator()*range);
		o.setTLTANG(o.getTLTANG() + generator()*range);
		o.setTAXA(o.getTAXA() + generator()*range);
		m_particles[i].updateOrientation(o);
	}
}


SingleParticle2dx::DataStructures::ParticleContainer::value_type SingleParticle2dx::DataStructures::ParticleContainer::getTotalSim()
{
	value_type result = 0;
	for (size_type i=0; i<getNumberOfParticles(); i++)
	{
		result += m_particles[i].getSimMeasure();
	}
	return result;
}


void SingleParticle2dx::DataStructures::ParticleContainer::splitIntoPerImageContainer(std::vector<std::vector<SingleParticle2dx::DataStructures::Particle*> >& cont_vec)
{
	SingleParticle2dx::DataStructures::ParticleContainer cont;
	cont_vec.clear();
	
	size_type image_counter = 1;
	size_type tmp_image_number = m_particles[0].getGlobalParticleInformation().getImageNumber();
	
	std::map<size_type,size_type> number_2_index;
	number_2_index[m_particles[0].getGlobalParticleInformation().getImageNumber()] = image_counter-1;
	
	for (size_type i=0; i<getNumberOfParticles(); i++)
	{
		if ( tmp_image_number !=  m_particles[i].getGlobalParticleInformation().getImageNumber())
		{
			image_counter++;
			tmp_image_number = m_particles[i].getGlobalParticleInformation().getImageNumber();
			number_2_index[tmp_image_number] = image_counter-1;
		}
	}
		
	for(size_type i=0; i<image_counter; i++)
	{
		std::vector<SingleParticle2dx::DataStructures::Particle*> cont;
		cont_vec.push_back(cont);
	}
	
	for (size_type i=getNumberOfParticles()-1; i>=0; i--)
	{
		size_type current_number = number_2_index[m_particles[i].getGlobalParticleInformation().getImageNumber()];
		cont_vec[current_number].push_back(&m_particles[i]);
	}
}


SingleParticle2dx::DataStructures::ParticleContainer::data_array_type& SingleParticle2dx::DataStructures::ParticleContainer::getParticles()
{
	return m_particles;
}


void SingleParticle2dx::DataStructures::ParticleContainer::splitIntoPerImageContainer(std::vector<SingleParticle2dx::DataStructures::ParticleContainer>& cont_vec)
{
	SingleParticle2dx::DataStructures::ParticleContainer cont;
	cont_vec.clear();
	
	size_type image_counter = 1;
	size_type tmp_image_number = m_particles[0].getGlobalParticleInformation().getImageNumber();
	
	std::map<size_type,size_type> number_2_index;
	number_2_index[m_particles[0].getGlobalParticleInformation().getImageNumber()] = image_counter-1;
	
	for (size_type i=0; i<getNumberOfParticles(); i++)
	{
		if ( tmp_image_number !=  m_particles[i].getGlobalParticleInformation().getImageNumber())
		{
			image_counter++;
			tmp_image_number = m_particles[i].getGlobalParticleInformation().getImageNumber();
			number_2_index[tmp_image_number] = image_counter-1;
		}
	}
	
	for(size_type i=0; i<image_counter; i++)
	{
		SingleParticle2dx::DataStructures::ParticleContainer cont;
		cont_vec.push_back(cont);
	}
	
	for (size_type i=getNumberOfParticles()-1; i>=0; i--)
	{
		size_type current_number = number_2_index[m_particles[i].getGlobalParticleInformation().getImageNumber()];
		cont_vec[current_number].addParticle(m_particles[i]);
		eraseLastElement();
	}
	
	/*
	
	size_type current_number = m_particles[getNumberOfParticles()-1].getGlobalParticleInformation().getImageNumber();
		
	for (size_type i=getNumberOfParticles()-1; i>=0; i--)
	{
		std::cout << i << std::endl;
		SingleParticle2dx::DataStructures::GlobalParticleInformation current_info = m_particles[i].getGlobalParticleInformation();
		SingleParticle2dx::DataStructures::Orientation current_orientation = m_particles[i].getInitialOrientation();
		
		if ( m_particles[i].getGlobalParticleInformation().getImageNumber() != current_number )
		{
			current_number = m_particles[i].getGlobalParticleInformation().getImageNumber();
			cont_vec.push_back(cont);
			cont.clear();
			cont.addParticle(m_particles[i]);
		}
		else
		{
			cont.addParticle(m_particles[i]);
		}
		this->eraseLastElement();
	}
	
	if(cont.getNumberOfParticles() > 0)
	{
		cont_vec.push_back(cont);
	}
	
	*/
}


void SingleParticle2dx::DataStructures::ParticleContainer::generateAverageContainer(SingleParticle2dx::DataStructures::ParticleContainer& cont_out, bool applyWeight, bool doinparallel)
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	cont_out.clear();
	
	//std::vector<SingleParticle2dx::DataStructures::ParticleContainer> cont_vec;
	std::vector<std::vector<SingleParticle2dx::DataStructures::Particle*> > cont_vec;
	
	//this->splitIntoPerImageContainer(cont_vec);
	this->splitIntoPerImageContainer(cont_vec);
	
	for(size_type i=0; i<static_cast<size_type>(cont_vec.size()); i++)
	{
		SingleParticle2dx::DataStructures::Particle p(cont_vec[i][0]->getSizeX(), cont_vec[i][0]->getSizeY(), cont_vec[i][0]->getInitialOrientation(), cont_vec[i][0]->getGlobalParticleInformation());
		p.setParticleShift(cont_vec[i][0]->getParticleShift());
		cont_out.addParticle(p);
	}
	
	if(doinparallel)
	{
		#pragma omp parallel for schedule(dynamic,1)
		for(size_type i=0; i<static_cast<size_type>(cont_vec.size()); i++)
		{
			real_array_type ave_im ( boost::extents[m_particles[0].getSizeX()][m_particles[0].getSizeY()] );
		
			SingleParticle2dx::Utilities::UtilityFunctions::calculateAverage(cont_vec[i], ave_im, false);
			cont_out(i).setFourierSpaceData(&ave_im);
			cont_out(i).updateParticleShift();
			cont_out(i).normalizeRealSpace();
		
			if(applyWeight)
			{
				SingleParticle2dx::Utilities::AverageWeighter weighter(config->getAveWeightMode(), static_cast<value_type>(cont_vec[i].size()), cont_out(i).getInitialOrientation().getTLTANG());
				cont_out(i).setWeight(weighter.getWeight());
			}
		}
	}
	else
	{
		for(size_type i=0; i<static_cast<size_type>(cont_vec.size()); i++)
		{
			real_array_type ave_im ( boost::extents[m_particles[0].getSizeX()][m_particles[0].getSizeY()] );
		
			SingleParticle2dx::Utilities::UtilityFunctions::calculateAverage(cont_vec[i], ave_im, true);
			cont_out(i).setFourierSpaceData(&ave_im);
			cont_out(i).updateParticleShift();
			cont_out(i).normalizeRealSpace();
		
			if(applyWeight)
			{
				SingleParticle2dx::Utilities::AverageWeighter weighter(config->getAveWeightMode(), static_cast<value_type>(cont_vec[i].size()), cont_out(i).getInitialOrientation().getTLTANG());
				cont_out(i).setWeight(weighter.getWeight());
			}
		}
	}
	
	cont_out.setParticleNumbers();
	cont_out(0).getGlobalParticleInformation().setImageName(m_particles[0].getGlobalParticleInformation().getImageName());
		
	/*
	size_type current_number = m_particles[0].getGlobalParticleInformation().getImageNumber();
	
	size_type counter = 0;
	
	for (size_type i=0; i<getNumberOfParticles(); i++)
	{
		SingleParticle2dx::DataStructures::GlobalParticleInformation current_info = m_particles[i].getGlobalParticleInformation();
		SingleParticle2dx::DataStructures::Orientation current_orientation = m_particles[i].getInitialOrientation();
		
		if ( m_particles[i].getGlobalParticleInformation().getImageNumber() != current_number )
		{
			current_number = m_particles[i].getGlobalParticleInformation().getImageNumber();
			SingleParticle2dx::DataStructures::Particle p(m_particles[i].getSizeX(), m_particles[i].getSizeY(), m_particles[i-1].getInitialOrientation(), m_particles[i-1].getGlobalParticleInformation());
			p.setFourierSpaceData(&ave_im);
			p.normalizeRealSpace();
			cont_out.addParticle(p);
			m_particles[i].getRealSpaceData(&ave_im);
			std::cout << ":counter = " << counter << std::endl;
			counter = 1;
		}
		else
		{
			if(m_particles[i].getUseForReconstruction())
			{
				counter++;
				m_particles[i].getRealSpaceData(&tmp_im);
				SingleParticle2dx::Utilities::DataContainerFunctions::addRealData(&tmp_im, 1.0, &ave_im);
			}
		}
	}
	
	if (counter > 0)
	{
		SingleParticle2dx::DataStructures::Particle p(m_particles[getNumberOfParticles()-1].getSizeX(), m_particles[getNumberOfParticles()-1].getSizeY(), m_particles[getNumberOfParticles()-1].getInitialOrientation(), m_particles[getNumberOfParticles()-1].getGlobalParticleInformation());
		p.setFourierSpaceData(&ave_im);
		p.normalizeRealSpace();
		cont_out.addParticle(p);
		std::cout << ":counter = " << counter << std::endl;
	}
	
	cont_out.setParticleNumbers();
	*/
}

//TODO
void SingleParticle2dx::DataStructures::ParticleContainer::updateInitialTiltGeometryAndShift(SingleParticle2dx::DataStructures::ParticleContainer& cont_out)
{
	std::vector<SingleParticle2dx::DataStructures::Orientation> o_vec;
	std::vector<SingleParticle2dx::DataStructures::ParticleShift> s_vec;
	
	std::map<size_type,size_type> number_2_index;
	
	for(size_type i=0; i<static_cast<size_type>(cont_out.getNumberOfParticles()); i++)
	{
		o_vec.push_back(cont_out(i).getNewOrientation());
		s_vec.push_back(cont_out(i).getParticleShift());
		number_2_index[cont_out(i).getGlobalParticleInformation().getImageNumber()] = i;
	}
	
	size_type image_number;
	for(size_type i=0; i<static_cast<size_type>(getNumberOfParticles()); i++)
	{
		image_number = m_particles[i].getGlobalParticleInformation().getImageNumber();
		m_particles[i].setOrientation(o_vec[number_2_index[image_number]]);
		m_particles[i].setParticleShift(s_vec[number_2_index[image_number]]);
	}
}


void SingleParticle2dx::DataStructures::ParticleContainer::shuffleContainer(size_type n)
{
	typedef boost::mt19937 RNGType;
	RNGType rng;
	boost::uniform_int<> zero_to_n( 0, getNumberOfParticles()-1 );
	boost::variate_generator< RNGType, boost::uniform_int<> >dice(rng, zero_to_n);
	
	for (size_type i=0; i<n; i++ )
	{
		size_type first_ind = dice();
		size_type second_ind = dice();
		
		SingleParticle2dx::DataStructures::Particle p_temp = m_particles[first_ind];
		m_particles[first_ind] = m_particles[second_ind];
		m_particles[second_ind] = p_temp;
	}
}


SingleParticle2dx::DataStructures::Particle& SingleParticle2dx::DataStructures::ParticleContainer::findParticleWithNumber (size_type number)
{
	for(size_type i=0; i<getNumberOfParticles(); i++)
	{
		if( m_particles[i].getGlobalParticleInformation().getImageNumber() == number)
		{
			return m_particles[i];
		}
	}
	
	std::cerr << "No corresponding particle found" << std::endl;
	throw std::runtime_error("Bad operation");
}


void SingleParticle2dx::DataStructures::ParticleContainer::updateWeightForReconstruction()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	boost::accumulators::accumulator_set<value_type, boost::accumulators::stats<boost::accumulators::tag::min, boost::accumulators::tag::max> > acc_sim;
	boost::accumulators::accumulator_set<value_type, boost::accumulators::stats<boost::accumulators::tag::min, boost::accumulators::tag::max> > acc_qual;
	boost::accumulators::accumulator_set<value_type, boost::accumulators::stats<boost::accumulators::tag::min, boost::accumulators::tag::max> > acc_cons;
	
	#pragma omp parallel
	for (size_type i=0; i<getNumberOfParticles(); i++)
	{
		m_particles[i].calculateConsistency();
		//std::cout << "NEW consistency: " << m_particles[i].getConsistency() << std::endl;
		if ( m_particles[i].getUseForReconstruction() )
		{
			
			#pragma omp critical
			{
				acc_sim(m_particles[i].getSimMeasure());
				acc_qual(m_particles[i].getQuality());
				std::cout << i << std::endl;
			}
			
			value_type cons = m_particles[i].getConsistency();
			//if ( std::isfinite(cons) )
			if ( (fabs(cons)>0.0001) && (std::isfinite(cons)) )
			{
				#pragma omp critical
				{
					acc_cons(cons);
				}
			}
			
		}
	}
	
	value_type min_sim = boost::accumulators::extract_result<boost::accumulators::tag::min>(acc_sim);
	value_type max_sim = boost::accumulators::extract_result<boost::accumulators::tag::max>(acc_sim);
	
	value_type min_qual = boost::accumulators::extract_result<boost::accumulators::tag::min>(acc_qual);
	value_type max_qual = boost::accumulators::extract_result<boost::accumulators::tag::max>(acc_qual);
	
	value_type min_cons = boost::accumulators::extract_result<boost::accumulators::tag::min>(acc_cons);
	value_type max_cons = boost::accumulators::extract_result<boost::accumulators::tag::max>(acc_cons);
	
	value_type s,q,c,w;
	
	value_type w1 = config->getWeightS();
	value_type w2 = config->getWeightQ();
	value_type w3 = config->getWeightC();
	value_type w_total = w1 + w2 + w3;
	w1 /= w_total;
	w2 /= w_total;
	w3 /= w_total;
	
	for (size_type i=0; i<getNumberOfParticles(); i++)
	{
		if ( m_particles[i].getUseForReconstruction() )
		{
			s = ((m_particles[i].getSimMeasure()-min_sim)/(max_sim-min_sim)-0.5)*2;
			q = ((m_particles[i].getQuality()-min_qual)/(max_qual-min_qual)-0.5)*2;
		
			//if ( !std::isfinite(m_particles[i].getConsistency()) )
			if ( (fabs(m_particles[i].getConsistency()) < 0.000001) || (!std::isfinite(m_particles[i].getConsistency())))
			{
				std::cout << ":RESET CONS" << std::endl;
				m_particles[i].setConsistency(min_cons);
			}
			
			c = ((m_particles[i].getConsistency()-min_cons)/(max_cons-min_cons)-0.5)*2;
			
			if (w3 > 0.0)
			{
				w = exp( - ( w1*acos(s) + w2*acos(q) + w3*acos(c) ) );
			}
			{
				if(w2 > 0.0)
				{
					w = exp( - ( w1*acos(s) + w2*acos(q) ) );
				}
				else
				{
					w = exp( - w1*acos(s) );
				}
			}
		
			std::cout << "weight: " << s << " " << q << " " <<  w << " " << c << std::endl;
		
			m_particles[i].setWeight(w);
		}
	}
	
	boost::accumulators::accumulator_set<value_type, boost::accumulators::stats<boost::accumulators::tag::min, boost::accumulators::tag::max> > acc_w;
	
	for (size_type i=0; i<getNumberOfParticles(); i++)
	{
		if ( m_particles[i].getUseForReconstruction() )
		{
			acc_w(m_particles[i].getWeight());
		}
	}
	
	value_type min_w = boost::accumulators::extract_result<boost::accumulators::tag::min>(acc_w);
	value_type max_w = boost::accumulators::extract_result<boost::accumulators::tag::max>(acc_w);
	
	for (size_type i=0; i<getNumberOfParticles(); i++)
	{
		if ( m_particles[i].getUseForReconstruction() )
		{
			w = (m_particles[i].getWeight()-min_w)/(max_w-min_w);
			m_particles[i].setWeight(w+1);
		}
	}
	
	boost::shared_ptr<FILE> file( fopen ( "debug_weights.txt", "w" ), SingleParticle2dx::Utilities::CustomizedDeleter() );
	fprintf(file.get(), "%s\t%s\t%s\t%s\t%s\n", "nb", "weight", "sim", "qual", "cons");
	for (size_type i=0; i<getNumberOfParticles(); i++)
	{
		if ( m_particles[i].getUseForReconstruction() )
		{
			fprintf(file.get(), "%i\t%f\t%f\t%f\t%f\n", i, m_particles[i].getWeight(), m_particles[i].getSimMeasure(), m_particles[i].getQuality(), m_particles[i].getConsistency()); 
		}
	}
	
}


void SingleParticle2dx::DataStructures::ParticleContainer::writeContainerToDisk(std::string foldername, size_type mode)
{
	if ( getNumberOfParticles() == 0 )
	{
		std::cout << "I don't write container out because it is empty" << std::endl;
		return;
	}
	else
	{
		std::cout << "writing non empty container" << std::endl;
	}

	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	if ( boost::filesystem::exists(foldername) )
	{
		boost::filesystem::remove(foldername + "/average.mrc");
		boost::filesystem::remove(foldername + "/particle_info.txt");
	}
	else
	{
		boost::filesystem::create_directory(foldername);
	}
	
	std::string part_filename = foldername + "/particle_info.txt";
	boost::shared_ptr<FILE> file( fopen ( part_filename.c_str(), "w" ), SingleParticle2dx::Utilities::CustomizedDeleter() );

	std::string filename;
	
	if (mode == 1)
	{
		//#pragma omp parallel for private (filename)
		for ( int i=0; i<getNumberOfParticles(); i++)
		{
			filename = foldername + "/particle_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(i) + ".mrc";
			std::cout << i << " check = " << m_particles[i].checkParticle() << std::endl;
			m_particles[i].writeToFile(filename);
		}
	}
	
	
	for ( size_type i=0; i<getNumberOfParticles(); i++)
	{
		fprintf(file.get(), "%i\t%i\t%f\t%f\t%f\t%f\n", i, m_particles[i].getGlobalParticleInformation().getImageNumber(), m_particles[i].getNewOrientation().getTLTAXIS(), m_particles[i].getNewOrientation().getTLTANG(), m_particles[i].getNewOrientation().getTAXA(), m_particles[i].getSimMeasure());
	}
	
	real_array_type ave_im ( boost::extents[m_particles[0].getSizeX()][m_particles[0].getSizeY()] );
	getAverage(ave_im);
	filename = foldername + "/average.mrc";
	
	SingleParticle2dx::Utilities::DataContainerFunctions::normalizeRealSpaceData(&ave_im);
	
	if(SingleParticle2dx::Utilities::DataContainerFunctions::checkData(&ave_im))
	{
		std::cerr << "bad container average" << std::endl;
		throw std::runtime_error("Bad operation");
	}
	
	SingleParticle2dx::Utilities::MRCFileIO::writeToMrc(&ave_im, filename);
	
	std::vector<std::string> split_vector;
	SingleParticle2dx::Utilities::StringFunctions::splitString( split_vector, foldername, std::string("/") );
	
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(filename, "Avg: " + split_vector.back(), config->getScriptName(), false, false);
	
	std::cout << "writing done" << std::endl;
}


void SingleParticle2dx::DataStructures::ParticleContainer::storeParticleEMAN2(std::string filename)
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	size_type n = config->getParticleSize();
	
	SingleParticle2dx::real_array2d_type particle_data(boost::extents[n][n]);
	SingleParticle2dx::real_array3d_type stack(boost::extents[n][n][n]);
	
	std::cout << "::shape x = " << stack.shape()[0];
	std::cout << "::shape y = " << stack.shape()[1];
	std::cout << "::shape z = " << stack.shape()[2];
	
	for(size_type i=0; i<getNumberOfParticles(); i++)
	{
		m_particles[i].getRealSpaceData(&particle_data);
		SingleParticle2dx::Utilities::MRCFileIO::writeToMrc(&particle_data, "debug.mrc");
		
		for(size_type ii=0; ii<n; ii++)
		{
			for(size_type jj=0; jj<n; jj++)
			{
				//stack[ii][50][jj] = particle_data[ii][jj];
				stack[i][ii][jj] = particle_data[jj][ii];
			}
		}
	}
	SingleParticle2dx::Utilities::MRCFileIO::writeToMrc(&stack, filename);
}


void SingleParticle2dx::DataStructures::ParticleContainer::getCenterOfMass(std::pair<value_type, value_type>& center_of_mass)
{
	boost::accumulators::accumulator_set<value_type, boost::accumulators::stats<boost::accumulators::tag::mean> > acc_x;
	boost::accumulators::accumulator_set<value_type, boost::accumulators::stats<boost::accumulators::tag::mean> > acc_y;
	
	SingleParticle2dx::DataStructures::GlobalParticleInformation part_info;
	for(int i=0; i<getNumberOfParticles(); i++)
	{
		part_info = m_particles[i].getGlobalParticleInformation();
		acc_x(static_cast<value_type>(part_info.getPositionX()));
		acc_y(static_cast<value_type>(part_info.getPositionY()));
	}
	
	center_of_mass.first = boost::accumulators::extract_result<boost::accumulators::tag::mean>(acc_x);
	center_of_mass.second = boost::accumulators::extract_result<boost::accumulators::tag::mean>(acc_y);
}


SingleParticle2dx::DataStructures::ParticleContainer::size_type SingleParticle2dx::DataStructures::ParticleContainer::getNearestParticleNumber(std::pair<value_type, value_type> center_of_mass)
{
	value_type best_radius = 99999;
	value_type current_radius;
	size_type best_index = -1;
	SingleParticle2dx::DataStructures::GlobalParticleInformation part_info;
	
	
	for(int i=0; i<getNumberOfParticles(); i++)
	{
		part_info = m_particles[i].getGlobalParticleInformation();
		current_radius = sqrt((center_of_mass.first - part_info.getPositionX()) * (center_of_mass.first - part_info.getPositionX()) + (center_of_mass.second - part_info.getPositionY()) * (center_of_mass.second - part_info.getPositionY()));
		if(current_radius < best_radius)
		{
			best_radius = current_radius;
			best_index = i;
		}
	}

	if(best_index<0)
	{
		std::cerr << "Not able to find best maching particle in CM" << std::endl;
		throw std::runtime_error("Bad CM");
	}

	std::cout << "best: " << best_index << std::endl;
	return best_index;
}


void SingleParticle2dx::DataStructures::ParticleContainer::serializeContainerToDisk(SingleParticle2dx::DataStructures::ParticleContainer& cont, bool do_full_write, std::string filename)
{
	typedef boost::archive::binary_oarchive archive_out_type;
	
//	std::cout << ":Storing to: " << filename << std::endl;
	
	SingleParticle2dx::Utilities::SystemTimer timer;
	value_type t_start = timer.GetTime();
	
	if (do_full_write)
	{
		std::ofstream myfile;
		myfile.open("/home/scherers/Desktop/ser_debug.txt");
		
		for(size_type i=0; i<cont.getNumberOfParticles(); i++)
		{
			myfile << cont(i).getParticleNumber() << "\t" << cont(i).getGlobalParticleInformation().getImageNumber() << std::endl;
		}
		
		myfile.close();
		
		std::ofstream ofs(filename.c_str());
		archive_out_type oa(ofs);
		oa << cont;
	}
	else
	{
		std::vector<SingleParticle2dx::DataStructures::ParticleFingerPrint> fp_vec;
		for(size_type i=0; i<cont.getNumberOfParticles(); i++)
		{
			SingleParticle2dx::DataStructures::ParticleFingerPrint tmp_print(cont(i));
			fp_vec.push_back(tmp_print);
		}
		std::ofstream ofs(filename.c_str());
		archive_out_type oa(ofs);
		oa << fp_vec;
	}
	
	value_type time_used = timer.GetTime() - t_start;
//	std::cout << ":time for storing: " << time_used << std::endl;
//	std::cout << ":speed for storing: " << boost::filesystem::file_size(filename) / (1.074e+9*time_used) << " GB/s" << std::endl;
}


void SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(std::string filename, SingleParticle2dx::DataStructures::ParticleContainer& cont, bool do_full_read)
{
	typedef boost::archive::binary_iarchive archive_in_type;
	
	if ( !boost::filesystem::exists(filename) )
	{
		std::cerr << "Container not there " << filename << std::endl;
		throw std::runtime_error("File not there");
	}
	
//	std::cout << ":Loading from: " << filename << std::endl;
	
	SingleParticle2dx::Utilities::SystemTimer timer;
	value_type t_start = timer.GetTime();

	if (do_full_read)
	{
		std::ifstream ifs(filename.c_str());
		archive_in_type ia(ifs);
		ia >> cont;
	}
	else
	{
		std::vector<SingleParticle2dx::DataStructures::ParticleFingerPrint> fp_vec;
		std::ifstream ifs(filename.c_str());
		archive_in_type ia(ifs);
		ia >> fp_vec;
		
		if(static_cast<size_type>(fp_vec.size()) != cont.getNumberOfParticles())
		{
			std::cerr << "wrong particle finger print vector" << std::endl;
			throw std::runtime_error("Wrong size of particle finger print vector");
		}
		
		for(size_type i=0; i<cont.getNumberOfParticles(); i++)
		{
			cont(i).applyParticleFingerPrint(fp_vec[i]);
		}
		
	}

	value_type time_used = timer.GetTime() - t_start;
//	std::cout << ":time for reloading: " << time_used << std::endl;
//	std::cout << ":speed for reloading: " << boost::filesystem::file_size(filename) / (1.074e+9*time_used) << " GB/s" << std::endl;
}




