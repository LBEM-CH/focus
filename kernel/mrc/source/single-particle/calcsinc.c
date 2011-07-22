
void calcsinc(float *sinclut, int N)
{	int j;
	float pi=3.1415926535897;

	for(j=0; j<N;j++)
		sinclut[j]=sin(j*pi/180.0)/(j*pi/180.0);

}

