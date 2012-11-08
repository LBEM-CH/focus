/**********************************************************************/
/*	subroutine package to write mrc image format map              */
/**********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>

FILE 	*mapfile;

/**********************************************************************/
/*             header structure definition                            */
/**********************************************************************/
	struct 	header
	{
		int 	nxyz[3];
		int 	mode;
		int 	nxyzstart[3];
		int 	mxyz[3];
		float 	cell[6];
		int	mapcrs[3];
		float	denmmm[3];
		int	spg;
		int	nsymbt;
		char	extra[116];
		float	origxy[2];
		int	nlabl;
		char	gap[800];
	};

/********************************************************************/
/*              global declarations                                 */
/********************************************************************/

/********************************************************************/
/*              routine declarations                                */
/********************************************************************/
void 	swap_bytes(char *swap_data,int nbytes_to_swap,int nitems);

/**********************************************************************/
/*		start of main program                                 */
/**********************************************************************/
int main()
{
	char		data[65536];
	char		filename[256];

	int		byte_size;
	int		file_size;
	int		header_size;
	int		icol;
	int		imode;
	int		ipack;
	int		irow;
	int		isec;
	int		line_offset;
	int		nbytes_to_swap = 4;
	int		ncols;
	int		nitems;
	int		nrows;
	int		nsecs;
	long int	offset = 0;

	off_t		file_stat_size;

	struct 		header 	map_header;
	struct		stat	file_stat;

	printf("\n");
	printf("***********************************************\n");
	printf("******* byte_swap_map v0.2 22.07.96 ***********\n");
	printf("***********************************************\n");
	printf("\n");
	printf("Type in file name ...\n");
	scanf("%s",filename);

/* open input file */
	if((mapfile = fopen(filename,"r+")) == NULL) 
	{
		printf("cannot open file ... %s \n",filename);
		exit(1);
	}

/* read input file header */
	header_size = sizeof(struct header);
	nitems = 1;
	if(fread(&map_header,header_size,nitems,mapfile) != nitems)
		{
		printf("fread failed during header read ...\n");
		exit(1);
		}

/* New or old-style map ?*/
	if(map_header.extra[112] == 'M')
		{
		printf
                ("Error - cannot byte-swap new-style map, use image_convert \n");
		exit(1);
		}

/* get mode, byte swap if necessary */
	imode = map_header.mode;
	if(imode < 0 || imode > 4)
	{
		nitems = 1;
		swap_bytes((char *)&imode, nbytes_to_swap, nitems);
		if(imode < 0 || imode > 4)
		{
			printf("Unknown mode ...\n");
			exit(1);
		}
	}

/* calculate file size from mode and nx ny nz */
	ncols = map_header.nxyz[0];
	nrows = map_header.nxyz[1];
	nsecs = map_header.nxyz[2];

/* byte data */
	if(imode == 0)
		{
		byte_size = 1;
		ipack = 1;
		}
/* 2 byte integers */
	else if(imode == 1)
		{
		byte_size = 2;
		ipack = 1;
		}
/* 4 byte reals */
	else if(imode == 2)	
		{
		byte_size = 4;
		ipack = 1;
		}
/* 2 byte transform */
	else if(imode == 3)
		{
		byte_size = 2;
		ipack = 2;
		}
/* 4 byte real transform */
	else if(imode == 4)	
		{
		byte_size = 4;
		ipack = 2;
		}

		file_size = ncols * nrows * nsecs * byte_size * ipack;

/* use system call stat to extract file size */
	if(stat(filename,&file_stat) != 0)
		{
		printf("Cannot stat file ...\n");
		exit(1);
		}

/* if system file size does not agree with calculated size, byte swap */
	file_stat_size = file_stat.st_size;
	if(file_size + header_size != file_stat_size)
		{
		nitems = 1;
		swap_bytes((char *)&ncols, nbytes_to_swap, nitems);
		swap_bytes((char *)&nrows, nbytes_to_swap, nitems);
		swap_bytes((char *)&nsecs, nbytes_to_swap, nitems);
/* try filesize match again */
		file_size = ncols * nrows * nsecs * byte_size * ipack;
		if(file_size + header_size != file_stat_size)
	 		  printf("WARNING Filesize does not match header...\n");
		}

/* check array size */
	if(ncols * ipack * byte_size > 65536)
		{
		printf("Program dimensions too small ...\n");
		exit(1);
		}

/* file size matched print out header information */
	printf("Number of columns, rows, sections ........%d %d %d \n",
		ncols, nrows, nsecs);
	printf("Map mode .................................%d\n",imode);

/*************** byte swap header ************************************/

/* single items */
	nitems = 1;
	swap_bytes((char *)&map_header.mode, nbytes_to_swap, nitems);
	swap_bytes((char *)&map_header.spg, nbytes_to_swap, nitems);
	swap_bytes((char *)&map_header.nsymbt, nbytes_to_swap, nitems);
	swap_bytes((char *)&map_header.nlabl, nbytes_to_swap, nitems);

/* 2 items */
	nitems = 2;
	swap_bytes((char *)&map_header.origxy, nbytes_to_swap, nitems);

/* 3 items */
	nitems = 3;
	swap_bytes((char *)&map_header.nxyz, nbytes_to_swap, nitems);
	swap_bytes((char *)&map_header.mxyz, nbytes_to_swap, nitems);
	swap_bytes((char *)&map_header.mapcrs, nbytes_to_swap, nitems);
	swap_bytes((char *)&map_header.denmmm, nbytes_to_swap, nitems);

/* 6 items */
	nitems = 6;
	swap_bytes((char *)&map_header.cell, nbytes_to_swap, nitems);

/***************** write byte-swapped header to file ******************/

	nitems = 1;
	if(fseek(mapfile, offset, 0) != 0)
		{
		printf("fseek failed before header rewite ...\n");
		exit(1);
		}

	if(fwrite(&map_header,header_size,nitems,mapfile) != nitems)
		{
		printf("fread failed during header write ...\n");
		exit(1);
		}

/************** read, swap and write data if mode non-zero ************/

	if(imode > 0)
		{
		printf(" Writing byte-swapped data ...\n");
		nitems = ncols * ipack;
		offset = header_size;
		line_offset = nitems * byte_size;
		for (isec = 0; isec < nsecs; isec ++)
		  {
		  for (irow = 0; irow < nrows; irow++)
			{
			if(fseek(mapfile, offset, 0) != 0)
				{
				printf("fseek failed before data read ...\n");
				exit(1);
				}
			if(fread(data, byte_size, nitems, mapfile) != nitems)
				{
				printf("fread failed during data read ...\n");
				exit(1);
				}
			swap_bytes(data, byte_size, nitems);
			if(fseek(mapfile, offset, 0) != 0)
				{
				printf("fseek failed before data write ...\n");
				exit(1);
				}
			if(fwrite(data, byte_size, nitems, mapfile) != nitems)
				{
				printf("fread failed during data write ...\n");
				exit(1);
				}
			offset = offset + line_offset;
			}
		  }
		}
	
	fclose(mapfile);
	printf("Normal termination ...\n");
	exit(0);
}

/********************************************************************/
/*              byte swapping subroutine                            */
/********************************************************************/

void swap_bytes(char *swap_data,int nbytes_to_swap,int nitems)
{
	char		bswap;

	int		i;
	int		j;

	j = 0;
	if(nbytes_to_swap == 2)
	{
		for(i = 0; i < nitems; i++)
		{
		bswap = swap_data[j];
		swap_data[j] = swap_data[j+1];
		swap_data[j+1] = bswap;
		j = j + nbytes_to_swap;
		}
	}
	else if(nbytes_to_swap == 4)
	{
		for(i = 0; i < nitems; i++)
		{
		bswap = swap_data[j];
		swap_data[j] = swap_data[j+3];
		swap_data[j+3] = bswap;
		bswap = swap_data[j+1];
		swap_data[j+1] = swap_data[j+2];
		swap_data[j+2] = bswap;
		j = j + nbytes_to_swap;
		}
	}
}
