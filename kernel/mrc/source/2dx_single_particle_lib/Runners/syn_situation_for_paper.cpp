#include <time.h>
#include <boost/random/normal_distribution.hpp>
#include <boost/random/uniform_real.hpp>
#include <boost/random/mersenne_twister.hpp>
#include <boost/random/variate_generator.hpp>

#include "../2dxSingleParticle.hpp"

int r_min = 12;
int dr = 18;
int dh = 24;

int n = 402;
int lat = 80;

int offset = 5;


void generateObject(int i, int j, int k, SingleParticle2dx::real_array3d_type& data)
{
	int r;
	
	for (int ii=i-dr; ii<=i+dr; ii++)
	{
		for (int jj=j-dr; jj<=j+dr; jj++)
		{
			r = sqrt( (i-ii)*(i-ii) + (j-jj)*(j-jj) );
			if ( (r>=r_min) && (r<=dr) )
			{
				for (int kk=k-dh; kk<=k+dh; kk++)
				{
					data[ii][jj][kk] = 1;
					//if( (abs(kk-k) < dh/2) && ((abs(ii-i)<r_min) || (abs(jj-j)<r_min)) )
					//{
					//	data[ii][jj][kk] = 0;
					//}
				}
			}
		}
	}
	
	int dx = 6;
	int offset2 = data.shape()[0]/2;
	for (int ii=4-dx; ii<=4+dx; ii++)
	{
		for (int jj=offset2-dx; jj<=offset2+dx; jj++)
		{
			for (int kk=offset2-dx; kk<=offset2+dx; kk++)
			{
				data[ii][jj][kk] = 1;
			}
		}
	}
}

template <typename T>
void insertObject(int i, int j, int k, SingleParticle2dx::real_array3d_type& data, SingleParticle2dx::real_array3d_type& protein, T &gen, int mode)
{
	typedef SingleParticle2dx::value_type value_type;
	typedef SingleParticle2dx::size_type size_type;
	
	value_type pi = 3.1415926;
	value_type dev = 0.0;
	
	value_type aux = 180*gen();
	aux = 0;
	value_type phi = aux * pi/180.0;
	value_type theta = dev * gen() * pi/180.0;
	value_type psi = (dev * gen() - aux) * pi/180.0;
	
	phi = 0.5 * pi;
	
	if(mode == 1)
	{
		if( i/lat == 1 )
		{
			theta += -10 * pi/180.0;
		}

		if( i/lat == 2 )
		{
			theta += 0;
			k += 2*5;
		}

		if( i/lat == 3 )
		{
			theta += 10 * pi/180.0;
		}

		if( i/lat == 4 )
		{
			theta += 25 * pi/180.0;
			k -= 2*13;
		}
	}
	else
	{
		if( i/lat == 1 )
		{
			psi += -12 * pi/180.0;
		}

		if( i/lat == 2 )
		{
			psi += 0;
			j -= 2*5;
		}

		if( i/lat == 3 )
		{
			psi += 12 * pi/180.0;
		}

		if( i/lat == 4 )
		{
			psi += 28 * pi/180.0;
			j += 2*10;
		}
	}
	
	std::cout << gen() << "\t" << aux << "\t" << phi << "\t" << theta << "\t" << psi << std::endl;
	
	size_type shift_scale = 2;
	size_type ds_x = floor(shift_scale*gen());
	size_type ds_y = floor(shift_scale*gen());
	size_type ds_z = floor(shift_scale*gen());
	
	//j += ds_x;
	//k += ds_y;
	//i += ds_z;
	
	value_type a11 = cos(psi)*cos(phi) - cos(theta)*sin(phi)*sin(psi);
	value_type a12 = cos(psi)*sin(phi) + cos(theta)*cos(phi)*sin(psi);
	value_type a13 = sin(psi)*sin(theta);
	
	value_type a21 = -sin(psi)*cos(phi) - cos(theta)*sin(phi)*cos(psi);
	value_type a22 = -sin(psi)*sin(phi) + cos(theta)*cos(phi)*cos(psi);
	value_type a23 = cos(psi)*sin(theta);
	
	value_type a31 = sin(theta)*sin(phi);
	value_type a32 = -sin(theta)*cos(phi);
	value_type a33 = cos(theta);
	
	value_type loc_i;
	value_type loc_j;
	value_type loc_k;
	
	value_type rot_i;
	value_type rot_j;
	value_type rot_k;
	
	size_type i_l, j_l, k_l;
	size_type i_h, j_h, k_h;
	
	value_type dx_l, dy_l, dz_l;
	value_type dx_h, dy_h, dz_h;
	
	for (int ii=i-dh-offset; ii<=i+dh+offset; ii++)
	{
		for (int jj=j-dh-offset; jj<=j+dh+offset; jj++)
		{
			for (int kk=k-dh-offset; kk<=k+dh+offset; kk++)
			{
				loc_i = ii-i;
				loc_j = jj-j;
				loc_k = kk-k;
				
				rot_i = loc_i*a11 + loc_j*a12 + loc_k*a13;
				rot_j = loc_i*a21 + loc_j*a22 + loc_k*a23;
				rot_k = loc_i*a31 + loc_j*a32 + loc_k*a33;
				
				i_l = floor(rot_i + dh + offset);
				j_l = floor(rot_j + dh + offset);
				k_l = floor(rot_k + dh + offset);
				
				i_h = i_l + 1;
				j_h = j_l + 1;
				k_h = k_l + 1;

				//std::cout << i_l << "," << j_l << "," << k_l << std::endl;
				
				dx_l = (1-(rot_i - i_l)) * (1-(rot_i - i_l));
				dy_l = (1-(rot_j - j_l)) * (1-(rot_j - j_l));
				dz_l = (1-(rot_k - k_l)) * (1-(rot_k - k_l));
				
				dx_h = 1-dx_l;
				dy_h = 1-dy_l;
				dz_h = 1-dz_l;
				
				dx_l = dy_l = dz_l = 1;
				dx_h = dy_h = dz_h = 1;
				
				if(  (i_l>=0) && (j_l>=0) && (k_l>=0) && (i_h<(2*dh+1+offset)) && (j_h<(2*dh+1+offset)) && (k_h<(2*dh+1+offset)) )
				{
					data[ii][jj][kk] += dx_l*dy_l*dz_l * protein[i_l][j_l][k_l];
				}				

				if(  (i_l>=0) && (j_l>=0) && (k_l>=0) && (i_h<(2*dh+1+offset)) && (j_h<(2*dh+1+offset)) && (k_h<(2*dh+1+offset)) )
				{
					data[ii+1][jj][kk] += dx_h*dy_l*dz_l * protein[i_h][j_l][k_l];
				}
				
				if(  (i_l>=0) && (j_l>=0) && (k_l>=0) && (i_h<(2*dh+1+offset)) && (j_h<(2*dh+1+offset)) && (k_h<(2*dh+1+offset)) )
				{
					data[ii][jj+1][kk] += dx_l*dy_h*dz_l * protein[i_l][j_h][k_l];
				}
				
				if(  (i_l>=0) && (j_l>=0) && (k_l>=0) && (i_h<(2*dh+1+offset)) && (j_h<(2*dh+1+offset)) && (k_h<(2*dh+1+offset)) )
				{
					data[ii][jj][kk+1] += dx_l*dy_l*dz_h * protein[i_l][j_l][k_h];
				}
				
				if(  (i_l>=0) && (j_l>=0) && (k_l>=0) && (i_h<(2*dh+1+offset)) && (j_h<(2*dh+1+offset)) && (k_h<(2*dh+1+offset)) )
				{
					data[ii+1][jj+1][kk] += dx_h*dy_h*dz_l * protein[i_h][j_h][k_l];
				}
				
				if(  (i_l>=0) && (j_l>=0) && (k_l>=0) && (i_h<(2*dh+1+offset)) && (j_h<(2*dh+1+offset)) && (k_h<(2*dh+1+offset)) )
				{
					data[ii+1][jj][kk+1] += dx_h*dy_l*dz_h * protein[i_h][j_l][k_h];
				}
				
				if(  (i_l>=0) && (j_l>=0) && (k_l>=0) && (i_h<(2*dh+1+offset)) && (j_h<(2*dh+1+offset)) && (k_h<(2*dh+1+offset)) )
				{
					data[ii][jj+1][kk+1] += dx_l*dy_h*dz_h * protein[i_l][j_h][k_h];
				}
				
				if(  (i_l>=0) && (j_l>=0) && (k_l>=0) && (i_h<(2*dh+1+offset)) && (j_h<(2*dh+1+offset)) && (k_h<(2*dh+1+offset)) )
				{
					data[ii+1][jj+1][kk+1] += dx_h*dy_h*dz_h * protein[i_h][j_h][k_h];
				}				
			}
		}
	}
}


void shuffleData(SingleParticle2dx::real_array3d_type& rdata)
{
	for(int i=0; i<n; i++)
	{
		for(int j=0; j<n; j++)
		{
			for(int k=0; k<i; k++)
			{
				float aux = rdata[i][j][k];
				rdata[i][j][k] = rdata[k][j][i];
				rdata[k][j][i] = aux;
			}
		}
	}
}


int main()
{
	boost::uniform_real<> uni_dist(-1,1);
	boost::variate_generator<boost::mt19937, boost::uniform_real<> > generator(boost::mt19937(time(0)), uni_dist );
	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	config->setParticleSize(n);
	
	SingleParticle2dx::DataStructures::Projection2d proj(n,n);
	
	SingleParticle2dx::real_array3d_type protein = SingleParticle2dx::real_array3d_type(boost::extents[2*dh+2*offset+1][2*dh+2*offset+1][2*dh+2*offset+1]);
	SingleParticle2dx::real_array3d_type data3d = SingleParticle2dx::real_array3d_type(boost::extents[n][n][n]);	
	
	generateObject(dh+offset, dh+offset, dh+offset, protein);
	SingleParticle2dx::Utilities::MRCFileIO::writeToMrc(&protein, "protein.mrc");
	
	SingleParticle2dx::DataStructures::Reconstruction3d rec(n,n,n);
	
	SingleParticle2dx::Utilities::DataContainerFunctions::resetData(&data3d);

	for(int i=lat; i<(n-lat); i+=lat)
	{
		for(int j=lat; j<(n-lat); j+=lat)
		{
			insertObject(i, j, n/2, data3d, protein, generator, 1);
		}
	}
	
	shuffleData(data3d);

	rec.setFourierSpaceData(data3d);
	rec.setProjectionMethod(4);
	SingleParticle2dx::DataStructures::ParticleContainer dummy_container;
	rec.forceProjectionPreparation(dummy_container);
		
	SingleParticle2dx::DataStructures::Orientation o1(0, 0, -90);
	rec.calculateProjection(o1, proj);
	proj.invertContrast();
	proj.writeToFile("proj_1_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(0) + ".mrc" );
		
	SingleParticle2dx::DataStructures::Orientation o2(90, 30, -180);
	rec.calculateProjection(o2, proj);
	proj.invertContrast();
	proj.writeToFile("proj_1_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(30) + ".mrc" );
	
	rec.writeToFile( "test_syn_1.mrc" );
	
	
	SingleParticle2dx::Utilities::DataContainerFunctions::resetData(&data3d);

	for(int i=lat; i<(n-lat); i+=lat)
	{
		for(int j=lat; j<(n-lat); j+=lat)
		{
			insertObject(i, j, n/2, data3d, protein, generator, 2);
		}
	}
	
	shuffleData(data3d);

	rec.setFourierSpaceData(data3d);
	rec.setProjectionMethod(2);
	rec.forceProjectionPreparation(dummy_container);
		
	SingleParticle2dx::DataStructures::Orientation o3(0, 0, -90);
	rec.calculateProjection(o3, proj);
	proj.invertContrast();
	proj.writeToFile("proj_2_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(0) + ".mrc" );
		
	SingleParticle2dx::DataStructures::Orientation o4(90, 30, -180);
	rec.calculateProjection(o4, proj);
	proj.invertContrast();
	proj.writeToFile("proj_2_" + SingleParticle2dx::Utilities::StringFunctions::TtoString(30) + ".mrc" );
	
	rec.writeToFile( "test_syn_2.mrc" );
	
	
	return 0;
}
