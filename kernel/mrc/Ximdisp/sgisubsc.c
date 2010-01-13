/* subsc.c   -  package of general purpose machines 27.01.98 */

/*
 Notes on fortran to C interfacing :
 For each character argument in a Fortran call, an int argument is
 appended to the end of the arg list, defining the Character string 
 length. e.g. from Fortran :
 CALL CFUNC (STR1, STR2, F1, I1)
 in C :
 cfunc_ (char *str1, char *str2, float *f1, int *i1, int lenstr1,
                          int lenstr2)

 When Fortran calls a character function, two args are prepended
 to the list, defining a char * pointer to the result string and
 an int containing the length of the result string. 
 e.g. From Fortran :
 STRING = CFUNC(ARG1, ARG2,...)
 in C :
 cfunc_(char *string, int lenstring, arg1, arg2,...)

 Other examples :
 STRING = CFUNC(STRING1)
 cfunc_ (char *string, int lenstring, char *string1, int lenstring1)

 STRING = CFUNC(STRING)
 cfunc_ (char *string, int lenstring)
 
 This last example may cause problems if the C code does strcpy(dest, src)
*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

void getdate_ (char *date,int *secs);
int imin_ (int i1, int i2);
int imax_ (int i1, int i2);
void genv_ (char *env,char *filename);
void intoch_ (char *cres, int creslen, int *number, int *nchars);
int lnblank_ (char *string, int stringlen);
void rmblank_ (char *result, int reslen,
                char *string, int *nchars, int stringlen);
void sleepdelay_ (int *delay);

/************************************************************/
/* return date                                              */
/************************************************************/
void getdate_ (char *date, int *secs)
{
        time_t tm;


        tm = time(NULL);
        *secs = tm;
        strcpy(date,ctime(&tm));
}

/**************************************************************/
/* min integer 						      */
/**************************************************************/
int imin_ (int i1, int i2)
{
	if(i1 < i2) return i1;
	else
	return i2;
}
/**************************************************************/
/* max integer 						      */
/**************************************************************/
int imax_ (int i1, int i2)
{
	if(i1 > i2) return i1;
	else
	return i2;
}
/**************************************************************/
/* integer to character conversion                            */
/**************************************************************/
void intoch_ (char *cres, int creslen, int *number, int *nchars)
/* C requires follow string with an integer and precede number of
   chars to be returned with original number */
{
	sprintf(cres,"%d",*number);
	*nchars = strlen(cres);
}

/**************************************************************/
/* return environment variable                                */
/**************************************************************/
void genv_ (char *env,char *filename)
{
	char *env_var;
	env_var = getenv(env);
	if(env_var == 0)
		strcpy(filename," ");
	else
		strcpy(filename,env_var);
}
/***************************************************************/
/* returns number of non-blank characters and sets rest to ' ' */
/***************************************************************/
int lnblank_ (char *string, int stringlen)
{

int	i;
int	j;
/* first search for null character or to end of string */
	for(i = 0; i < stringlen; i++)
		if(string[i] == '\0' || i == stringlen - 1)
		{
			if(string[i] == '\0') i--;
			for (j = i; j >= 0; j--)
				if(string[j] != ' ') return j+1;
		}
	return 0;
}
/***************************************************************/
/* removes blanks from a character string and returns no chars */
/***************************************************************/
void rmblank_ (char *cres, int creslen, 
		char *string, int *nchars, int stringlen)
{

int	i;
int	j;
/* copy non-blank characters to cres */
	j = 0;
	for(i = 0; i < stringlen; i++)
	{
		if(string[i] != ' ')
		{
		strncpy(&cres[j],&string[i],1);
		j = j + 1;
		}
	}

/* set remainder of string to blank */
	for(i = j; i < stringlen; i++)
		strncpy(&cres[i]," ",1);

	*nchars = j;
}
/***************************************************************/
/* delay for specified time  100 == 1 second                   */
/***************************************************************/
void sleepdelay_ (int *delay)
{
	unsigned idelay;
	idelay = *delay / 10000;
	sginap(idelay);
}
