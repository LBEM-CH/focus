#include <iostream>
#include <complex>
#include <cstdlib>
#include <string.h>
#include <mrcImage.h>
#include <fftw3.h>
#ifdef USE_THREADS_2DX
#include <pthread.h>
#endif
using namespace std;

struct threadPacket {
  int minM, maxM;
  int minN, maxN;
  int w,h;
  int stepSize;
  int sx, sy;
  int divisions;
  float *average;
  fftwf_complex *image;
  fftwf_complex *window;
  fftwf_plan *p;
};

#ifdef USE_THREADS_2DX
  pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
#endif

bool importWisdom()
{
  char str[80]; 
  strcpy(str, getenv("HOME"));
  strcat(str,"/.2dx/fftlib.wis");
  FILE *f = fopen(str,"r");
  if(f==NULL) return false;
  fftwf_import_wisdom_from_file(f);
  fclose(f);
  return true;
}

bool exportWisdom()
{
  char str[80]; 
  strcpy(str, getenv("HOME"));
  strcat(str,"/.2dx/fftlib.wis");
  FILE *f = fopen(str,"w");
  if(f==NULL) return false;
  fftwf_export_wisdom_to_file(f);
  fclose(f);
  return true;
}

fftwf_complex *dataFromMrc(char *fileName, int &sx, int &sy)
{
  mrcImage image(fileName);
  if(image.isEmpty()) return NULL;
  
  sx=image.width();
  sy=image.height();
  int mode=image.mode(), cellSize;
  float  temp;
  if(mode == 0) cellSize = 1;
  if(mode == 1) cellSize = 2;
  if(mode == 2) cellSize = 4;
  fftwf_complex *complexImage = (fftwf_complex*) fftwf_malloc(sx*sy*cellSize*sizeof(fftwf_complex));

  for(int j=0;j<sy;j++)
    for(int i=0;i<sx;i++)
    {
      if(mode == 0) temp = (float)((unsigned char*)image.rawData())[j*sx+i];
      if(mode == 1) temp = (float)((unsigned short*)image.rawData())[j*sx+i];
      if(mode == 2) temp = (float)((float*)image.rawData())[j*sx+i];
      complexImage[j*sx+i][0] = temp;
      complexImage[j*sx+i][1] = 0.0;
    }
  
  return complexImage; 
  
}

void clear(float *image, int sx, int sy)
{
  for(int j=0;j<sy;j++)
    for(int i=0;i<sx;i++)
      image[j*sx+i] = 0.0;
}

void *generatePeriodogramRange(void *arg)
{
  	threadPacket *tP = (threadPacket*)arg;
	float *average = tP->average;
	fftwf_complex *image = tP->image;

  	int w=tP->w, h=tP->h;
  	int stepSize=tP->stepSize;
  	int sx=tP->sx, sy=tP->sy;
  	int divisions = tP->divisions;
  	int lN=tP->minN, rN=tP->maxN;
  	int lM=tP->minM, rM=tP->maxM;

	fftwf_complex *window = (fftwf_complex*) fftwf_malloc(w*h*sizeof(fftw_complex));
#ifdef USE_THREADS_2DX
  	pthread_mutex_lock(&mutex);
#endif

	fftwf_plan p = fftwf_plan_dft_2d(w,h,window,window,FFTW_FORWARD,FFTW_ESTIMATE);

#ifdef USE_THREADS_2DX
  	pthread_mutex_unlock(&mutex);
#endif

	for(int n = lN; n < rN; n++)
		for(int m = lM; m < rM; m++)
		{
			for(int j=0;j<h;j++)
				for(int i=0;i<w;i++)
				{
					window[j*w+i][0] = image[((n*stepSize)+j)*sx+((m*stepSize)+i)][0]*powf(-1,i+j);
					window[j*w+i][1] = 0.0;
				}
			fftwf_execute(p);

			float onevol = 1.0/sqrtf(w*h);

			for(int j=0;j<h;j++)
				for(int i=0;i<w;i++)
				{
					average[j*w+i]+=sqrtf(window[j*w+i][0]*window[j*w+i][0] + window[j*w+i][1]*window[j*w+i][1])*onevol/divisions;
				} 
		} 
#ifdef USE_THREADS_2DX
        pthread_mutex_lock(&mutex);
#endif
	fftwf_destroy_plan(p);
#ifdef USE_THREADS_2DX
        pthread_mutex_unlock(&mutex);
#endif
	fftwf_free(window);
	return nullptr;
}


void generatePeriodogram(fftwf_complex *image, int sx, int sy, int size, int steps, char *fileName) 
{
	int w=size, h = size;
	float *average = (float*) fftwf_malloc(w*h*sizeof(float));
	clear(average,w,h);
	//importWisdom();
	//fftwf_set_timelimit(30);
	int stepSize = (sx-size)/steps;
	if(stepSize<=0) stepSize = 1;
	if(stepSize>sx) stepSize = sx;
	if(steps>sx/stepSize) steps = sx/stepSize;
	cout<<"Image size: "<<sx<<endl;
	cout<<"Window size: "<<size<<endl;
	cout<<"Steps: "<<steps<<endl;
	cout<<"Step Size: "<<stepSize<<endl;

	int divisions = steps*steps;
	int percent;

	threadPacket tP;
	tP.minN=0; tP.maxN=steps;
	tP.minM=0; tP.maxM=steps;
	tP.average=average;
	tP.image=image;
  tP.w=w;
  tP.h=h;
  tP.stepSize=stepSize;
  tP.sx=sx;
  tP.sy=sy;
  tP.divisions=divisions;

  int k=9;
  threadPacket packets[k];

#ifdef USE_THREADS_2DX 
	pthread_t threads[k];
#endif

	for(int i=0;i<k;i++)
	{
		packets[i] = tP;
		packets[i].minN=steps/k*i;
		packets[i].maxN=min(steps/k*(i+1),steps);
#ifdef USE_THREADS_2DX 
    pthread_create(&(threads[i]),NULL,generatePeriodogramRange,(void*)(&(packets[i])));
#else  
		generatePeriodogramRange(&(packets[i]));
#endif
	}

#ifdef USE_THREADS_2DX 
  for(int i=0;i<k;i++)
    pthread_join(threads[i],NULL);
#endif

	char *complexData = mrcImage::complexFromReal(w,h,2,(char*)average);
	mrcImage::mrcHeader *header = mrcImage::headerFromData(w/2+1,h,4,complexData);
	mrcImage(header,complexData,fileName);
	fftwf_free(average);
}

int main(int argc, char **argv)
{
	if(argc<2)
	{
		cout<<"Usage periodogram {image}"<<endl;
		return (1);
	}

	int size = 512;
	int steps = 20;
	if(argc>=3) size = atoi(argv[2]);
	if(argc>=4) steps = atoi(argv[3]);

	int imageWidth, imageHeight;
	fftwf_complex *image = dataFromMrc(argv[1], imageWidth, imageHeight);
	if(image == NULL) {cout<<"Error loading "<<argv[1]<<endl; return 1;}

        char str[80]; 
        strcpy(str,"./periodogram.mrc");
	generatePeriodogram(image,imageWidth,imageHeight,size,steps,str);
	fftwf_free(image);
	return 0;
}
