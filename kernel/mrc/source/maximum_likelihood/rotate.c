/*    Subroutine of 2d-rotation by angle
*/

/*
 *
 *  Created by Xiangyan Zeng on 1/1/2007
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#include <common.h>

void  rotate(int rows, int cols, float angle, float *Image, float *RTimage)

{   


	int i,j,k;
	float mean,alpha, RN2, SN2, RW2, RS2,i_new,j_new, x_old,y_old,cod,sid,power;

	mean=0;

	for(i=0;i<rows;i++)
		for(j=0;j<cols;j++)
			mean+=Image[IDX(i,j,cols,rows)];
	mean/=(rows*cols);





	for(i=0;i<rows;i++)
		for(j=0;j<cols;j++)
			RTimage[IDX(i,j,cols,rows)]=mean;


	alpha=(angle*3.141592653589793238462643383279502884197)/180;
	sid=sin(alpha);
	cod=cos(alpha);

	RN2=-(rows/2-1.0);
	SN2=-(cols/2-1.0);
	RW2=-RN2;
	RS2=-SN2;

	if(fmod((float)(rows),2.0)<0.5) RW2=RW2-1.0;
	if(fmod((float)(cols),2.0)<0.5) RS2=RS2-1.0;


	for(i=0;i<rows;i++)
	{   
		i_new=(float)(i-rows/2);

		if(i_new<RN2) 
		{ if(RW2<(RW2+i_new-RN2+1.0)) i_new=RW2;
			else i_new=RW2+i_new-RN2+1.0;
		}

		if(i_new>RW2)
		{ if(RN2>(RN2+i_new-RW2-1.0)) i_new=RN2;
			else i_new=RN2+i_new-RW2-1.0;
		}


		for(j=0;j<cols;j++)
		{   
			j_new=(float)(j-cols/2);


			if(j_new<SN2) 
			{  if(RS2<(RS2+j_new-SN2+1.0)) j_new=RS2;
				else j_new=RS2+j_new-SN2+1.0;
			}

			if(j_new>RS2)
			{  if(SN2>(SN2+j_new-RS2-1.0)) j_new=SN2;
				else j_new=SN2+j_new-RS2-1.0;
			} 

			x_old=(i_new*cod+j_new*sid)+(float)(rows/2);
			y_old=(-i_new*sid+j_new*cod)+(float)(cols/2);

			if(x_old>=0 && x_old<rows && y_old>=0 && y_old<cols)
				RTimage[IDX(i,j,cols,rows)]=quadintp(x_old,y_old,rows,cols,Image);
			//     else  RTimage[IDX(i,j,cols,rows)]=Image[IDX(i,j,cols,rows)];              
		}

	}



	for(i=0;i<rows;i++)
	{  RTimage[IDX(i,0,rows,cols)]=2*RTimage[IDX(i,1,rows,cols)]-RTimage[IDX(i,2,rows,cols)];
		RTimage[IDX(i,cols-1,rows,cols)]=2*RTimage[IDX(i,cols-2,rows,cols)]-RTimage[IDX(i,cols-3,rows,cols)];
	}


	for(j=0;j<cols;j++)
	{  RTimage[j]=2*RTimage[IDX(1,j,rows,cols)]-RTimage[IDX(2,j,rows,cols)];
		RTimage[IDX(rows-1,j,rows,cols)]=2*RTimage[IDX(rows-2,j,rows,cols)]-RTimage[IDX(rows-3,j,rows,cols)];
	}





	/*
		 mean=0;	 
		 for(i=0;i<rows;i++)
		 for(j=0;j<cols;j++)
		 mean+=RTimage[IDX(i,j,cols,rows)];

		 mean/=(rows*cols);	 
		 power=0;	 
		 for(i=0;i<rows;i++)
		 for(j=0;j<cols;j++)
		 {
		 RTimage[IDX(i,j,cols,rows)]=(RTimage[IDX(i,j,cols,rows)]-mean);	  
		 power+=powf(RTimage[IDX(i,j,cols,rows)],2.0);
		 } 	   

		 power=sqrtf(power/(rows*cols));
		 for(i=0;i<rows;i++)
		 for(j=0;j<cols;j++)
		 RTimage[IDX(i,j,cols,rows)]=RTimage[IDX(i,j,cols,rows)]/power;	  

	 */ 	 

}

