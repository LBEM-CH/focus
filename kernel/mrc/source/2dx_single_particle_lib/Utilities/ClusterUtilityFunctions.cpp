#include "ClusterUtilityFunctions.hpp"

#include "../DataStructures.hpp"

#include <utility> 

SingleParticle2dx::Utilities::ClusterUtilityFunctions::size_type SingleParticle2dx::Utilities::ClusterUtilityFunctions::findSmallestIndex(std::vector<size_type>& vec)
{
	size_type result = 0;
	
	for(size_type i=0; i<static_cast<size_type>(vec.size()); i++)
	{
		if (vec[result] > vec[i])
		{
			result = i;
		}
	}
	
	return result;
}


void SingleParticle2dx::Utilities::ClusterUtilityFunctions::printLoad(std::vector<std::vector<size_type> > load)
{
	for(size_type i=0; i<static_cast<size_type>(load.size()); i++)
	{
		std::cout << "mpi-task #" << i << ": ";
		for(size_type j=0; j<static_cast<size_type>(load[i].size()); j++)
		{
			std::cout << load[i][j] << " ";
		}
		std::cout << std::endl;
	}
}


void SingleParticle2dx::Utilities::ClusterUtilityFunctions::balanceLoad_naiv(size_type np, size_type n_images, std::vector<std::vector<size_type> >& load)
{
	size_type number_per_task = ceil(static_cast<float>(n_images) / static_cast<float>(np) );
	
	std::cout << "maximally " << number_per_task << " images per mpi-task" << std::endl;
	
	for(size_type i=0; i<np; i++)
	{
		std::vector<size_type> new_vec;
		load.push_back(new_vec);
	}
	
	for(size_type i=0; i<n_images; i++)
	{
		size_type index = i%np;
		load[index].push_back(i);
	}
	
	SingleParticle2dx::Utilities::ClusterUtilityFunctions::printLoad(load);
}


void SingleParticle2dx::Utilities::ClusterUtilityFunctions::balanceLoad_sizedep(size_type np, size_type n_images, std::vector<std::string> folder_content, std::vector<std::vector<size_type> >& load)
{
	for(size_type i=0; i<np; i++)
	{
		std::vector<size_type> new_vec;
		load.push_back(new_vec);
	}
	
	std::vector<std::pair<size_type, size_type> > size_vec;
	
	#pragma omp parallel for schedule(dynamic,1)
	for(int i=0; i<n_images; i++)
	{
		SingleParticle2dx::DataStructures::ParticleContainer cont;
		SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(folder_content[i], cont, true);
		
		std::pair<size_type,size_type> size(i, cont.getNumberOfParticles());
		
		#pragma omp critical (insert_to_size_vec)
		{
			size_vec.push_back(size);
		}
	}
	
	std::sort (size_vec.begin(), size_vec.end(), SingleParticle2dx::Utilities::ClusterUtilityFunctions::custumComperator);
	
	for(size_type i=0; i<n_images; i++)
	{
		size_type index = i%np;
		load[index].push_back(size_vec[i].first);
	}
	
	SingleParticle2dx::Utilities::ClusterUtilityFunctions::printLoad(load);
}


void SingleParticle2dx::Utilities::ClusterUtilityFunctions::balanceLoad_optimal_sizedep(size_type np, size_type n_images, std::vector<std::string> folder_content, std::vector<std::vector<size_type> >& load)
{
	for(size_type i=0; i<np; i++)
	{
		std::vector<size_type> new_vec;
		load.push_back(new_vec);
	}
	
	std::vector<std::pair<size_type, size_type> > size_vec;
	
	#pragma omp parallel for schedule(dynamic,1)
	for(size_type i=0; i<n_images; i++)
	{
		SingleParticle2dx::DataStructures::ParticleContainer cont;
		SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(folder_content[i], cont, true);
		
		std::pair<size_type,size_type> size(i, cont.getNumberOfParticles());
		
		#pragma omp critical (insert_to_size_vec)
		{
			size_vec.push_back(size);
		}
	}
	
	std::sort (size_vec.begin(), size_vec.end(), SingleParticle2dx::Utilities::ClusterUtilityFunctions::custumComperator);
	std::vector<size_type> load_size (np, 0);
	std::cout << "TEST: " << load_size.size() << std::endl;
	
	for(size_type i=0; i<n_images; i++)
	{
		size_type index = SingleParticle2dx::Utilities::ClusterUtilityFunctions::findSmallestIndex(load_size);
		load[index].push_back(size_vec[i].first);
		load_size[index] += size_vec[i].second;
	}
	
	SingleParticle2dx::Utilities::ClusterUtilityFunctions::printLoad(load);
}
