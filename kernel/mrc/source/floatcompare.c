#include <stdio.h>

main(int argc, char *argv[])
{
	float a,b;
	sscanf(argv[1],"%f",&a);
	sscanf(argv[2],"%f",&b);
	if(a>b)
		printf("1");
	else
		printf("0");
}
