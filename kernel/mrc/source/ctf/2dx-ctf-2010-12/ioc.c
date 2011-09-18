/***************************************************************************/
/*   2008 by Nilay K Roy                                                   */
/*   nilayr@brandeis.edu                                                   */
/***************************************************************************/
/* Description of the funtions:                                            */
/* FOR OPENING                                                             */
/* In fortran - call copen(arg1, arg2, arg3)                               */
/*  arg1 - file name                                                       */
/*  arg2 - file flag - UNIT=?                                              */
/*  arg3 - opening mode - 0 for read only, 1 for write only,               */
/*         and 2 for read/write - all binary i/o                           */
/* FOR READING                                                             */
/* In fortran - call cread(arg1, arg2, arg3, arg4, arg5)                   */
/*  arg1 - name of array to read into - binary format                      */
/*  arg2 - offset                                                          */
/*  arg3 - record length                                                   */
/*  arg4 - record number                                                   */
/*  arg5 - file flag - UNIT=?                                              */
/*  REMARK: The other args in the C function are internal ones sent        */
/*          automatically by fortran for the character inputs              */
/* FOR WRITING                                                             */
/* In fortran - call cwrite(arg1, arg2, arg3, arg4, arg5)                  */
/*  arg1 - name of array to write from - binary format                     */
/*  arg2 - offset                                                          */
/*  arg3 - record length                                                   */
/*  arg4 - record number                                                   */
/*  arg5 - file flag - UNIT=?                                              */
/*  REMARK: The other args in the C function are internal ones sent        */
/*          automatically by fortran for the character inputs              */
/* FOR CLOSING                                                             */
/* In fortran - call cclose(arg1)                                          */
/* arg 1 - file flag - UNIT=?                                              */
/***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAXFILES       200
#define MAXFILENAME    1000

static FILE *file_stream[MAXFILES];
static char file_name[MAXFILES][MAXFILENAME];
static int file_mode[MAXFILES];
static int initialised=0;

void copen_(char *fname, int *f_flag, int *f_mode, int f_name)
{
     int i, j, ftemp, k, l;
     char ctemp[MAXFILENAME];char *cfinal;char c1;
     for (k=0;k<MAXFILENAME;k++){
          ctemp[k] = ' ';
     }
     strncpy(ctemp,fname,f_name);     
     ftemp=strlen(ctemp);
     i=*f_flag;
     j=*f_mode;
     for (k=0;k<ftemp;k++){
          c1=ctemp[k];
          l=isblank(c1);
          if (l!=0) ftemp=k;
     }
     cfinal = (char *) malloc(ftemp+101);
     strncpy(cfinal,ctemp,ftemp);
     cfinal[ftemp]='\0';
     for (k=ftemp+1;k<ftemp+100;k++){
          cfinal[k] = ' ';
     }
     if (!initialised){
         for (k = 0; k < MAXFILES; k++) {
          file_stream[k] = NULL;
          file_name[k][0]='\0';
          file_mode[k]=-1;
         }
      initialised=1;
     }
     
    if ((file_stream[i] != NULL) || (i==MAXFILES)){
        printf("Cannot allocate file buffer to %s. UNIT in use or maximum number of files open...\n", cfinal);
        exit(1);
    } 

   if (j==0){
    if ((file_stream[i]=fopen(cfinal,"rb"))==NULL){
      printf("Cannot open file %s for reading...\n", cfinal);
      exit(1);
    }
    strcpy(file_name[i],cfinal);
    file_mode[i]=j;
  }
   if (j==1){
    if ((file_stream[i]=fopen(cfinal,"wb"))==NULL){
      printf("Cannot open file %s for writing...\n", cfinal);
      exit(1);
    }
    strcpy(file_name[i],cfinal);
    file_mode[i]=j;
  }
   if (j==2){
    if ((file_stream[i]=fopen(cfinal,"rb+"))==NULL){
      printf("Cannot open file %s for reading/writing...\n", cfinal);
      exit(1);
    }
    strcpy(file_name[i],cfinal);
    file_mode[i]=j;
  }
  free(cfinal);

 return;
}

void cclose_(int *f_flag)
{
  int i, k;
  i=*f_flag;
  if (file_stream[i] == NULL){
     printf("Cannot close file ...\n");
     exit(1);
  }
     fclose(file_stream[i]);
     file_stream[i]=NULL;
     for (k=0;k<MAXFILENAME;k++){
          file_name[i][k] = ' ';
     }
/*     file_name[i][0]='\0';  */
     file_mode[i]=-1;
 return;
}

void cread_(char *readarray, int *offs, int *reclen, int *recno, int *f_flag, int f_name, int r_array){
     int fw,result_read,ftemp,toffs,trecno,i;
     long int ff;
     char ctemp[MAXFILENAME];
     fw=*reclen;ff=*reclen;toffs=*offs;trecno=*recno;
     ff=((((trecno)-1)*ff)+ (toffs));
     i=*f_flag;
     strcpy(ctemp,file_name[i]);
     ftemp=strlen(ctemp);
     if ((ftemp==0)||(file_stream[i]==NULL)||(file_mode[i]==-1)) {
      printf("Cannot use file - no file name. Open file first.\n");
      exit(1);
     }
     if (file_mode[i]==1) {
      printf("Cannot read file. File opened in write only mode.\n");
      exit(1);
     }
       fflush(file_stream[i]);
       if (fseek(file_stream[i],0L,SEEK_SET)!=0){
       printf("Cannot seek in file ...\n");
       exit(1);
       }
       if (fseek(file_stream[i],ff,SEEK_SET)!=0){
       printf("Cannot seek in file ...\n");
       exit(1);
       }
       result_read=fread(readarray,1,fw,file_stream[i]);
       if (result_read != fw) {
        printf("Cannot read file %s\n", ctemp);
        exit(1);
       }
   return;
}

void cwrite_(char *writearray, int *offs, int *reclen, int *recno, int *f_flag, int f_name, int w_array){
     int fw,result_write,ftemp,toffs,trecno,i;
     long int ff;
     char ctemp[MAXFILENAME];
     fw=(*reclen);ff=(*reclen);toffs=*offs;trecno=*recno;
     ff=((((trecno)-1)*ff)+ (toffs));
     i=*f_flag;
     strcpy(ctemp,file_name[i]);
     ftemp=strlen(ctemp);
     if ((ftemp==0)||(file_stream[i]==NULL)||(file_mode[i]==-1)) {
      printf("Cannot use file - no file name. Open the file first.\n");
      exit(1);
     }
     if (file_mode[i]==0) {
      printf("Cannot write file. File opened in read only mode.\n");
      exit(1);
     }
       if (fseek(file_stream[i],0L,SEEK_SET)!=0){
       printf("Cannot seek in file ...\n");
       exit(1);
       }
       if (fseek(file_stream[i],ff,SEEK_SET)!=0){
       printf("Cannot seek in file ...\n");
       exit(1);
       }
       result_write=fwrite(writearray,1,fw, file_stream[i]);
       if (result_write != fw) {
        printf("Cannot write file %s\n", ctemp);
        exit(1);
       }
    return;
}
