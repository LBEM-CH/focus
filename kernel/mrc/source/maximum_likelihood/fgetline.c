#include <common.h>

int  fgetline(FILE *imgcfg, char *strchar, float *x)
{          
	char *st,  *st1,  *st4, *st5, *st6, *pch;
        char *strcha2;
	int k, len, rval;

	st=(char *)calloc(200,sizeof(char));
	st1=(char *)calloc(200,sizeof(char));

	st4=(char *)calloc(200,sizeof(char));
	st5=(char *)calloc(200,sizeof(char));
	st6=(char *)calloc(200,sizeof(char));

        strcha2=(char *)calloc(200,sizeof(char));

        strcpy(strcha2,strchar);
        strncat(strcha2," ",1);
        

	if ( fgets(st,200,imgcfg) == NULL )
	{
		perror ("Error reading file in fgetline (1)");
	}

	if(feof(imgcfg)==0)
	{  
		strcpy(st4,"");
		strncat(st4,st,3);

		strcpy(st1,"");
		
		//strncat(st1,st+4,200);
		strncat(st1,st+4,120);
		// st1 = const_cast<char*>((std::string(st1) + std::string(st)).c_str());
	
		while(feof(imgcfg)==0 && ( strncmp(st4,"set",3)!=0 ||  strncmp(st1, strcha2,strlen(strcha2))!=0))
		{
			if ( fgets(st,200,imgcfg) == NULL )
			{
				perror ("Error reading file in fgetline (2)");
			}
			
			strcpy(st4,"");
			strncat(st4,st,3);

			strcpy(st1,"");
			strncat(st1,st+4,120);
		}

		if(feof(imgcfg)==0 && strncmp(st1, strcha2,strlen(strcha2))==0)
		{ 


			pch=(char *)memchr(st, '"', strlen(st));   
			strcpy(st5,pch+1);


			pch=(char *)memchr(st5, '"',strlen(st5));
			strcpy(st6,"");
			strncat(st6,st5, strlen(st5)-strlen(pch));


			k=0;
			pch=(char *)memchr(st6,',',strlen(st6)); 

			while(pch!=NULL)
			{   strcpy(st1,"");

				strncat(st1,st6, strlen(st6)-strlen(pch));          
				x[k]=string_to_real(st1);

				len=strlen(st1);
				k++;
				strcpy(st1,"");
				strncat(st1,st6+len+1,120);

				strcpy(st6,st1);
				pch=(char *)memchr(st6,',',strlen(st6)); 

			}    

			x[k]=string_to_real(st6);    rewind(imgcfg); rval=0; 

		}        
		else      
                {  printf(" '%s' float parameter does not exist in  2dx_image.cfg\n", strchar);    rewind(imgcfg);  rval=1; }
	}
	else
	{  printf(" '%s' float parameter does not exist  in 2dx_image.cfg \n", strchar);  rewind(imgcfg);  rval=1; }

        // printf("::fgetline.c: parameter %s is %f\n", strchar,x[0]);

	free(st);
	free(st1);
	free(st4);
	free(st5);
	free(st6);
        free(strcha2);

        return(rval);

}
