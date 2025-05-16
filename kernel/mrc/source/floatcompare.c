#include <stdio.h>

int main(int argc, char *argv[])
{
	float a,b;
        if (argc < 3) {
	    	fprintf(stderr, "Usage: %s <float a> <float b>\n", argv[0]);
    		return 1;
	}
	sscanf(argv[1],"%f",&a);
	sscanf(argv[2],"%f",&b);
	if(a>b)
		printf("1");
	else
		printf("0");
        return 0;
}
