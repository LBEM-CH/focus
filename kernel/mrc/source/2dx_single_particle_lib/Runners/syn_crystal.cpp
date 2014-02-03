#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int_distribution.hpp>
#include <boost/random/uniform_real_distribution.hpp>
#include <boost/random/uniform_01.hpp>

#include "../2dxSingleParticle.hpp"


int main()
{
	int nc = 20;
	int n = 120;
	int nperside = 40;
	
	float sd_angles = 3;
	
	float tilts[] = {0, 15, 15, 15, 30, 30, 30, 45, 45, 45};
	std::vector<float> tilt_vec(tilts, tilts+sizeof(tilts)/sizeof(float));
	
	boost::random::mt19937 rng(time(0));
	boost::random::uniform_int_distribution<> die(0, tilt_vec.size()-1);
	
	int c_count = 0;
	int tilt_index = 0;
	std::vector<float> tilt_vector;
	while(c_count < nc)
	{
		int tilt = tilt_vec[tilt_index++];
		if ( tilt_index == static_cast<int>(tilt_vec.size()) )
		{
			tilt_index = 0;
		}
		c_count++;
		tilt_vector.push_back(tilt);
	}
	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	config->setParticleSize(n);
	config->setProjectionMethod(4);
	config->setParallelProjection(true);
	config->setProjectionMaskingMethod(1);
	config->setProjectionMaskingRadius(n/2-5);
	config->setProjectionMaskingdR(5);
	config->setParticleMaskingMethod(1);
	config->setParticleMaskingRadius(n/2-5);
	config->setParticleMaskingdR(5);
	
	SingleParticle2dx::DataStructures::ParticleContainer dummy_container;
	
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d_1(n,n,n);
	rec3d_1.readFromFile("/home/scherers/Desktop/aq/ref_both.mrc");
	
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d_2(n,n,n);
	rec3d_2.readFromFile("/home/scherers/Desktop/aq/ref_none.mrc");
	
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d_3(n,n,n);
	rec3d_3.readFromFile("/home/scherers/Desktop/aq/ref_left.mrc");
	
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d_4(n,n,n);
	rec3d_4.readFromFile("/home/scherers/Desktop/aq/ref_right.mrc");
	
	std::vector<SingleParticle2dx::DataStructures::Reconstruction3d> rec_vec;
	rec_vec.push_back(rec3d_1);
	rec_vec.push_back(rec3d_2);
	rec_vec.push_back(rec3d_3);
	rec_vec.push_back(rec3d_4);
	
	for(int i=0; i<static_cast<int>(rec_vec.size()); i++)
	{
		rec_vec[i].forceProjectionPreparation(dummy_container);
	}
	
	
//	rec3d.writeToFile("test.mrc");
	
	
	boost::variate_generator<boost::mt19937, boost::normal_distribution<> > generator(boost::mt19937(time(0)), boost::normal_distribution<>());
	boost::random::mt19937 rng_2(time(0));
	boost::random::uniform_01<> taxa;
	
	boost::random::mt19937 rng_rec(time(0));
	boost::random::uniform_int_distribution<> die_rec(0,rec_vec.size()-1);
	
	//#pragma omp parallel for
	for(int c=0; c< static_cast<int>(tilt_vector.size()); c++)
	{
		SingleParticle2dx::DataStructures::Projection2d proj(n,n);
		SingleParticle2dx::real_array2d_type rdata( boost::extents[n][n] );
		SingleParticle2dx::DataStructures::Orientation o;
		
		o.setTLTAXIS(60 + generator()*sd_angles);
		o.setTLTANG(tilt_vector[c] + generator()*sd_angles);
		//o.setTAXA( rng_2(taxa) * 360 );
		o.setTAXA( taxa(rng_2) * 360 );
		
		SingleParticle2dx::DataStructures::ParticleContainer container;
		for(int i=0; i<nperside; i++)
		{
			for(int j=0; j<nperside; j++)
			{
				int rec_index = die_rec(rng_rec);
				
				std::cout << rec_index << " ";
				
				rec_vec[rec_index].calculateProjection(o, proj);
				
				SingleParticle2dx::DataStructures::GlobalParticleInformation global(0, i, j);
				SingleParticle2dx::DataStructures::ClassInformation class_info(rec_index, rec_index);
				SingleParticle2dx::DataStructures::Particle part(n, n, o, global);
				part.setClassInformation(class_info);
			
				proj.getRealSpaceData(&rdata);
				
				for(int ii=0; ii<n; ii++)
				{
					for(int jj=0; jj<n; jj++)
					{
						rdata[ii][jj] += 0*generator();
					}
				}
				
				part.setFourierSpaceData(&rdata);
			
				container.addParticle(part);
			}
		}
		
		container.applyMaskToContainer();
		
		std::cout << std::endl;
		
		SingleParticle2dx::DataStructures::ParticleContainer::serializeContainerToDisk(container, true, "part_cont" + SingleParticle2dx::Utilities::StringFunctions::TtoString(c) + ".bin");
		container(0).writeToFile("part_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(c) + ".mrc");
	}
	
	
	
	
		
	return 0;
}
