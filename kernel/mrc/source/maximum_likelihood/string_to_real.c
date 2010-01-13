
#include <common.h>

float  string_to_real(char *st)
{
	int i,len,j,flag=1,  sign=1,start=0;
	float x;  
	j=0;
	x=0;

	len=strlen(st);
	if(st[0]=='-')
	{   sign=-1;
		start=1;
	}

	for(i=start;i<len;i++)
	{

		if(st[i]=='.')
			flag=0;
		else if(st[i]!=' ')
			x=(x+(st[i]-'0'))*10;

		if(flag==0 && st[i]!=' ' )
			j++; 
	} 

	if(j==0)
		x=x/10;
	else
		for(i=0; i<j; i++)
			x=x/10;

	x=x*sign;
	return(x);

}



