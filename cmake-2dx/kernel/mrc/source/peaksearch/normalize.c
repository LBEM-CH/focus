#include <stdio.h>
#include <stdlib.h>
#include <math.h>



#define Row 1024  /*  Rows  */
#define Col 1024   /*  Columns  */


main()

{ 


   FILE *input, *output;
   int n1,n2;
   float y,y1,y2,min, max,temp;
   int i_max,j_max, i_min, j_min;
   float  *amp;
   float temp1,t;



  
  
   amp=(float *)malloc(Row*Col*sizeof(float)); 
  
     
         //  input=fopen("phase.dat","r");
             input=fopen("amp_LHpass.dat","r"); 



       output=fopen("amp_int.dat","w");
        fprintf(output,"P2 %d %d \n", Col,Row);
	fprintf(output,"256 \n");


    min=100000000; max=-100000000;
 
  for(n2=0;n2<Col;n2++) 
    for(n1=0;n1<Row; n1++)  
       
        {
                     
           fscanf(input,"%f ", &temp);
           if(min>temp) {min=temp; i_min=n1; j_min=n2;}
           if(max<temp) {max=temp; i_max=n1; j_min=n2;}
           *(amp+n2+(Row-n1-1)*Col)=temp;
        }

   printf("max=%f min=%f i_max=%d j_max=%d i_min=%d j_min=%d \n",max,min, i_max, j_max, i_min, j_min);


   for(n1=0;n1<Row; n1++)
      for(n2=0;n2<Col;n2++)
        if(*(amp+n2+n1*Col)>max*0.1)   *(amp+n2+n1*Col)=max*0.1; 


        
   max=max*0.1; 



   for(n1=0;n1<Row; n1++)
      for(n2=0;n2<Col;n2++)
          *(amp+n2+n1*Col)=(*(amp+n2+n1*Col)-min)*256.0/(max-min)+1;
      
 



 //  printf("\nin[0][0]=%f  in[63][63]=%f",amp[0+0*Col],amp[63+63*Col]);


   for(n1=0;n1<Row; n1++)
     {
        for(n2=0;n2<Col;n2++)
             fprintf(output,"%d  ",(int)(*(amp+n2+n1*Col)));
        fprintf(output,"\n");   
      }  

  free(amp);

  fclose(input);  fclose(output); 
} 
