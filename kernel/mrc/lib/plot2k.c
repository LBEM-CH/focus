/* all routines have a C-callable and a Fortran-callable entry-point */
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#define A4heightins     11.65
#define A4widthins      8.25
#define A4scale         0.9*A4widthins*72/2
/****#define A4scale         1.0*A4widthins*72/2****/
#define A4xoffset       A4widthins*72/2
#define A4yoffset       A4heightins*72/2
#define FALSE		0
#define points_per_mm	72/25.4
#define TRUE		1



#define	A4WIDTHmm	209
#define A4HEIGHTmm	296
#define	PI		3.14159
#define STACKLIM	10
#define	TICKLEN		2



#define MAXCOLOUR 9



void	p2k_arrow(float x, float y, float z);
void	p2k_axis(float axmin, float axmax, int nticks,
                 float X0, float Y0, float axangle,
                 float *adjustedX0,
                 char *axislabel, int nchars);
void	p2k_circle(float radius);
void	p2k_colour(int colour);
void	p2k_rgb_colour(float rr, float rg, float rb);
void	p2k_dash(float l1, float gap1, float l2);
void	p2k_draw(float p0, float p1, float p2);
void	p2k_ellipse(float major, float minor, float angle);
void	p2k_font(char *fontname, float charheight_mm);
void	p2k_grid(float p0, float p1, float p2);
void	p2k_here(void);
void	p2k_home(void);
void	p2k_line(float x0, float y0, float z0, float x1, float y1, float z1);
void	p2k_lwidth(float widthmm);
void	p2k_move(float p0, float p1, float p2);
void	p2k_origin(float x0, float y0, float z0);
void	p2k_outfile(char *filename, int nchars);
void	p2k_page(void);
void	p2k_point(float x0, float y0, float z0);
void	p2k_pop1(int*);
void	p2k_push3(int *depth, float *list, float p0, float p1, float p2);
void	p2k_rgr(void);
void	p2k_ror(void);
void	p2k_rscale(void);
void	p2k_rtw(void);
void	p2k_scale(float p0);
void	p2k_setfac(void);
void	p2k_string(char *text, int nchars, float angle);
void	p2k_cstring(char *text, int nchars, float angle);
void	p2k_symbol(int symtype, float size, float angle);
void	p2k_tcstring(char *text, float angle);
void	p2k_tstring(char *text, float angle);
void	p2k_twist(float p0, float p1, float p2);
void	p2k_xyposn(float p0, float p1, float p2);


void	p2k_devdep_circle(float radius);
void	p2k_devdep_colour(int colour);
void	p2k_devdep_cmyk_colour(float rc, float rm, float ry, float rk);
void	p2k_devdep_rgb_colour(float rr, float rg, float rb);
void	p2k_devdep_font(char *fontname, float charheight_mm);
void	p2k_devdep_init(void);
void	p2k_devdep_linto(float x, float y);
void	p2k_devdep_lwidth(float widthmm);
void	p2k_devdep_movto(float x, float y);
void	p2k_devdep_pntto(float x, float y);
void	p2k_devdep_page(void);
void	p2k_devdep_string(char *text, float angle);
void	p2k_devdep_cstring(char *text, float angle);


float	a;
float	angle;
float	cmyk[MAXCOLOUR+1][4]=
	{0,0,0,1,		/* 0 black	*/
	.005,1,1,0,		/* 1 brown	*/
	0,1,1,0,		/* 2 red	*/
	0,.35,1,0,		/* 3 orange	*/
	0,0,1,0,		/* 4 yellow	*/
	1,0,1,0,		/* 5 green	*/
	1,1,0,0,		/* 6 blue	*/
	0,1,0,0,		/* 7 purple	*/
	1,1,1,0,		/* 8 grey	*/
	0,0,0,0};		/* 9 white	*/
float	cosx[3][STACKLIM+1];
float	currentcharheight;
int	currentcolour;
char	currentfont[256];
float	currentlinewidth;
int	debug=FALSE;
//int	debug=TRUE;
float	gr[3][STACKLIM+1];
int	grdep;
int	i;
int	idash=4;
int	inited=FALSE;
float	o[3];
float	or[3][STACKLIM+1];
int	ordep;
FILE*	fd=NULL;
float	ox;
float	oy;
float	p[3];
int	pathstarted;
float	rdash[4];
float	rtdash[4];
float	sc[STACKLIM+1];
int	scdep;
float	sinx[3][STACKLIM+1];
float	tw[3][STACKLIM+1];
int	twdep;
float	x2d;
float	xfctr[4];
float	xnow;
float	xscrn;
float	y2d;
float	yfctr[4];
float	ynow;
float	yscrn;




/*****************************************************************************/
/* SGI C-library doesnt have nint */
float our_nint(float v)
{
float f;
f=0.5;
if (v < 0.0) f=-0.5;
return (float)(int)(v+f);
}

/*****************************************************************************/
float total_xtwist(void)
{
float temp_angle;
temp_angle=0;
/* compute the total rotated angle for postscript text */
for (i=0; i<=twdep; i++) temp_angle=temp_angle+tw[0][i];
return temp_angle;
}

/*****************************************************************************/
/* draw an arrow from current position to x,y,z) */
void p2k_arrow_(float *x, float *y, float *z)
{p2k_arrow(*x, *y, *z);}

void p2k_arrow(float x, float y, float z)
{
float	barb=0.02;
float	cosa;
float	cos30=0.866;
float	flen;
float	sina;
float	sin30=0.5;
float	xinc;
float	yinc;

if (debug == TRUE) fprintf(stderr,"***p2k_arrow\n");
xnow=x2d;
ynow=y2d;
p2k_xyposn(x,y,z);
p2k_devdep_linto(x2d,y2d);
p2k_here();
p2k_grid(1.0,1.0,1.0);
xinc=x2d-xnow;
yinc=y2d-ynow;
flen=sqrt(xinc*xinc+yinc*yinc);
cosa=xinc/flen;
sina=yinc/flen;
p2k_draw(-barb*(cosa*cos30-sina*sin30),-barb*(sina*cos30+cosa*sin30),0.0);
p2k_line(-barb*(cosa*cos30+sina*sin30),-barb*(sina*cos30-cosa*sin30),0.0,
         0.0,0.0,0.0);
p2k_rgr();
p2k_ror();
}

/*****************************************************************************/
/*
Draw an axis between the given start and end points,
rotated anticlockwise from the current xtwist by the given angle
with the given number of tick marks.
If the number of tick marks is negative, the tick-marks and the
corresponding labels will be draw above the axis
*/

/*
The tick-labels will be automatically scaled such that they can 
be printed with a maximum of 3 significant digits. If more didgits
are required, an exponent will be added to the axis label.
*/

/* drawing will be done in the current line-style, thickness and colour */
/* and any text will be in the current font */

void p2k_axis(float axmin, float axmax, int nticks,
              float X0, float Y0, float axangle,
              float *adjustedX0,
              char *axislabel, int nchars)
{
float	axislength;
int	exponent;
int	i;
int	imin;
float	f;
float	fmin;
int	newnticks;
float	newaxmax;
float	newaxmin;
float	prefticks[9]={1.0,1.5,2.0,2.5,3.0,4.0,5.0,7.5,8.0};
float	realinterval;
int	signoftick;
char	tempstr[256];
float	tickinterval;
float	ticklen;
float	x0;

if (debug == TRUE) fprintf(stderr,"***p2k_axis\n");
signoftick=-1;
if (nticks < 0)
  {
  nticks = -nticks;
  signoftick=1;
  }

/*
Choose a tick-interval from the available list of intervals,
which best fits the given axis parameters, by normalising
the actual tick interval to the range 1 - 10 and then choosing
the nearest value from the list.
*/
tickinterval=fabsf(axmax-axmin)/(float)nticks;
exponent=0;
while (tickinterval < 1.0)
  {
/*printf("+++ tickinterval=%f --exp %i\n",tickinterval,exp);*/
  exponent--;
  tickinterval=tickinterval*10.0;
  }
while (tickinterval >= 10.0)
  {
/*printf("+++ tickinterval=%f ++exp %i\n",tickinterval,exp);*/
  exponent++;
  tickinterval=tickinterval/10.0;
  }
fmin=100;
for (i=0; i<9; i++)
  {
  f=fabsf(tickinterval-prefticks[i]);
  if (f < fmin)
    {
    fmin=f;
    imin=i;
    }
  }
tickinterval=prefticks[imin];
/*printf("+++exp=%i tickinterval=%f\n",exponent,tickinterval);*/

/*
Make an adjusted axmin and axmax rounded to the nearest tickmark.
Round down to get the new xmin, and up to get the new xmax.
If we are very close to a tick-point, dont bother.
*/
realinterval=tickinterval;
if (exponent < 0) for (i=1; i<= -exponent; i++) realinterval=realinterval/10.0;
if (exponent > 0) for (i=1; i<=  exponent; i++) realinterval=realinterval*10.0;
/*printf("+++exp=%i realinterval=%f\n",exponent,realinterval);*/
newaxmin=axmin;
f=realinterval*(float)(int)(axmin/realinterval);
if (f-axmin > 0.05*realinterval) newaxmin=f-realinterval;
/* extend the axis if necessary */
if (X0 < axmin) newaxmin=X0;
*adjustedX0=newaxmin;
/*printf("+++axmin=%f newaxmin=%f\n",axmin,newaxmin);*/
newaxmax=axmax;
f=realinterval*(float)(int)(axmax/realinterval);
if (axmax -f > 0.05*realinterval) newaxmax=f+realinterval;
/*printf("+++%i\n",(int)our_nint(axmax/realinterval+0.5));*/
/*printf("+++axmax=%f newaxmax=%f\n",axmax,newaxmax);*/
newnticks=our_nint(fabsf(newaxmax-newaxmin)/realinterval+0.1);

/*
Label the ticks to N significant digits + a sign + a decimal-point.
If more are needed, we scale the tick label-values and
add an exponent number to the main axis label
*/
f=powf(10.0,(float)exponent);
x0=newaxmin;
p2k_twist(axangle, axangle+90., 0.0);
/* put the left-limit of the axis at the left edge */
p2k_move(-newaxmin, 0.0, 0.0);
p2k_here();
for (i=0; i<=newnticks; i++)
  {
  p2k_move(x0, Y0, 0.0);
  p2k_here();
  p2k_grid(1.0, ((float)A4WIDTHmm)/2.0, 1.0);
  p2k_draw(0.0, signoftick*TICKLEN, 0.0);
  if (signoftick < 0) p2k_move(0.0, -(TICKLEN+currentcharheight+1.0), 0.0);
  if (signoftick > 0) p2k_move(0.0, (TICKLEN+1.0), 0.0);
  sprintf(tempstr,"%3.1f",x0/f);
  /* junk trailing zero's */
  p2k_tcstring(tempstr,0.0);
  p2k_move(0.0, 0.0, 0.0);
  p2k_rgr();
  p2k_ror();
  /* draw last tick without an axis segment */
  if (i != newnticks) p2k_draw(x0+realinterval, 0.0, 0.0);
  x0=x0+realinterval;
  }
/*
Draw label in fontsize=2*ticklabelsize
*/
p2k_move((newaxmax+newaxmin)/2.0, Y0, 0.0);
p2k_here();
p2k_grid(1.0, ((float)A4WIDTHmm)/2.0, 1.0);
if (signoftick < 0) p2k_move(0.0, -(TICKLEN+2.5*currentcharheight+3.0), 0.0);
if (signoftick > 0) p2k_move(0.0, (TICKLEN+currentcharheight+3.0), 0.0);
f=currentcharheight;
p2k_font(currentfont,1.5*f);
p2k_cstring(axislabel,nchars,0.0);
if (exponent != 0)
  {
  sprintf(tempstr," ( x by 10**%i)",exponent);
  p2k_tstring(tempstr, 0.0);
  }
p2k_font(currentfont,f);
p2k_rgr();
p2k_ror();
p2k_rtw();
p2k_ror();

}

/*****************************************************************************/
/* Draw an autogenerated pair of axes */
void p2k_axes(float axmin, float axmax, int nxticks,
              float aymin, float aymax, int nyticks,
              float X0, float Y0,
	      float *adjustedX0, float *adjustedY0,
              char *xlabel, int nxchars,
              char *ylabel, int nychars)
{
if (debug == TRUE) fprintf(stderr,"***p2k_axes\n");
p2k_axis(axmin,axmax,nxticks,X0,Y0,0.0,adjustedX0,xlabel,nxchars);
/*
temporarily set the xgrid to be that of the ygrid since an axis
is always drawn as an X-axis, rotated by 90 deg if it is to be a y-axis
*/
p2k_grid(gr[1][grdep], 1.0, 1.0);
p2k_axis(aymin,aymax,nyticks,X0,Y0,90.0,adjustedY0,ylabel,nychars);
p2k_rgr();
}

void p2k_axes_(float *axmin, float *axmax, int *nxticks,
               float *aymin, float *aymax, int *nyticks,
               float *X0, float *Y0,
               float *adjustedX0, float *adjustedY0,
               char *xlabel, int *nxchars,
               char *ylabel, int *nychars)
{
p2k_axes(*axmin, *axmax, *nxticks,
         *aymin, *aymax, *nyticks,
         *X0, *Y0,
         adjustedX0, adjustedY0,
         xlabel, *nxchars,
         ylabel, *nychars);
}

/*****************************************************************************/
/* draw a circle centred at current position, radius r */
void p2k_circle_(float *radius)
{p2k_circle(*radius);}

void p2k_circle(float radius)
{
if (debug == TRUE) fprintf(stderr,"***p2k_circle, radius %f\n",radius);
p2k_devdep_circle(radius*xfctr[0]);
}

/*****************************************************************************/
/* select new colour */
void p2k_colour_(int *colour)
{p2k_colour(*colour);}

void p2k_colour(int colour)
{
if (debug == TRUE) fprintf(stderr,"***p2k_colour\n");
if ( colour<0 || colour>MAXCOLOUR)
  {
  fprintf(stderr,"p2k: Illegal colour requested\n");
  return;
  }
p2k_devdep_colour(colour);
currentcolour=colour;
}

/*****************************************************************************/
/* define new colour */
void p2k_rgb_colour_(float *rr, float *rg, float *rb)
{p2k_rgb_colour(*rr, *rg, *rb);}

void p2k_rgb_colour(float rr, float rg, float rb)
{
if (debug == TRUE) fprintf(stderr,"***p2k_rgb_colour\n");
if ( rr<0.0 || rg<0.0 || rb<0.0 || rr>1.0 || rg>1.0 || rb>1.0  )
  {
  fprintf(stderr,"p2k: Illegal rgb colour requested: %f %f %f\n",rr,rg,rb);
  return;
  }
p2k_devdep_rgb_colour(rr,rg,rb);
currentcolour=0;
}

/*****************************************************************************/
/* set dash-line mode */
void p2k_dash_(float *l1, float *gap1, float *l2)
{p2k_dash(*l1, *gap1, *l2);}

void p2k_dash(float l1, float gap1, float l2)
{
/*
this routine sets up a dashed line specification array.
dashed lines consist of line1,gap,line2,gap where line1,
gap,line2 are user defined as fractions of the full
screen half-width
*/
if (debug == TRUE) fprintf(stderr,"***p2k_dash\n");
rdash[0]=l1;
rdash[1]=gap1;
rdash[2]=l2;
rdash[3]=gap1;
idash=4;
for (i=0; i<4; i++)
  {
  rtdash[i]=rdash[i];
  if (rtdash[i] != 0.0) idash=0;
  }
}

/*****************************************************************************/
/* draw a line from current position to x,y,z */
void p2k_draw_(float *x, float *y, float *z)
{p2k_draw(*x, *y, *z);}

void p2k_draw(float x, float y, float z)
{
float	r;
float	r1;
float	r2;
float	r3;
float	r4;
float	xinc;
float	xlen;
float	xnow;
float	yinc;
float	ylen;
float	ynow;

if (debug == TRUE) fprintf(stderr,"***p2k_draw %f %f %f %i\n", x, y, z, idash);
xnow=x2d;
ynow=y2d;
p2k_xyposn(x,y,z);
if (idash==4)
  {
  p2k_devdep_linto(x2d,y2d);
  return;
  }
r1=x2d-xnow;
r2=y2d-ynow;
r3=r1*r1+r2*r2;
r4=sqrt(r3);
if (r4==0.0) return;
xlen=0.0;
ylen=0.0;

while (TRUE)
  {
  xinc=rtdash[idash]*r1/r4*sc[scdep];
  yinc=rtdash[idash]*r2/r4*sc[scdep];
  xnow=xnow+xinc;
  ynow=ynow+yinc;
  xlen=xlen+xinc;
  ylen=ylen+yinc;
  r=xlen*xlen+ylen*ylen;
  if (r>r3)
    {
    if (idash==0 || idash==2)
      p2k_devdep_linto(x2d,y2d);
    else
      p2k_devdep_movto(x2d,y2d);
    rtdash[idash]=sqrt(r)-r4+0.000001;
    return;
    }
  rtdash[idash]=rdash[idash];
  if (idash==0 || idash==2)
    p2k_devdep_linto(xnow,ynow);
  else
    p2k_devdep_movto(xnow,ynow);
  idash=(idash+1) & 3;
  }
}

/*****************************************************************************/
/* draw an ellipse centred on current position, with given major and minor axes,
   at given angle (degrees) to horizontal */
void p2k_ellipse_(float *major, float *minor, float *angle)
{p2k_ellipse(*major, *minor, *angle);}

void p2k_ellipse(float major, float minor, float angle)
{
float	dtheta;
int	nseg;
float	theta;
if (debug == TRUE) fprintf(stderr,"***p2k_ellipse\n");
p2k_here();
p2k_grid(1.0/major, 1.0/minor, 1.0);
p2k_twist(angle, angle+90.0, 0.0);
p2k_move(0.5,0.0,0.0);
nseg=50*sqrt(major);
if (nseg<8) nseg=8;
dtheta=6.283192/nseg;
theta=0.0;
for (i=1; i<=nseg; i++)
  {
  theta=theta+dtheta;
  p2k_draw(0.5*cosf(theta), 0.5*sinf(theta), 0.0);
  }
p2k_rtw();
p2k_rgr();
p2k_ror();
}

/*****************************************************************************/
/* set the conversion factor for user-units to plot-units in x,y,z */
void p2k_grid_(float *p0, float *p1, float *p2)
{p2k_grid(*p0, *p1, *p2);}

void p2k_grid(float p0, float p1, float p2)
{
if (debug == TRUE) fprintf(stderr,"***p2k_grid %f %f %f\n",p0,p1,p2);
if (p0 == 0.0 || p1 == 0.0 || p2 == 0.0)
  {
  printf("Plot2000 error. p2k_grid: zero grid-size supplied. Aborted.\n");
  exit(0);
  }
p2k_push3(&grdep,&gr[0][0],p0,p1,p2);
}

/*****************************************************************************/
/* set an origin at the current position */
void p2k_here_(void)
{p2k_here();}

void p2k_here(void)
{
if (debug == TRUE) fprintf(stderr,"***p2k_here\n");
p2k_origin(0.0, 0.0, 0.0);
}

/*****************************************************************************/
/* initialise the origin,twist,grid and scale depths, and the line-style,
   line thickness, font-size and drawing-colour */
void p2k_home_(void)
{p2k_home();}

void p2k_home(void)
{
if (debug == TRUE) fprintf(stderr,"***p2k_home\n");
scdep=-1;
grdep=-1;
ordep=-1;
twdep=-1;
x2d=0;
y2d=0;
p2k_scale(1.0);
p2k_grid(1.0, 1.0, 1.0);
p2k_twist(0.0, 90.0, 30.0);
p2k_dash(0.0, 0.0, 0.0);
p2k_here();
p2k_lwidth(0.0);
p2k_colour(0);
p2k_font("Courier",4.0);
p2k_move(0.0,0.0,0.0);
}

/*****************************************************************************/
/* set the line-thickness (in mm) to the given value */
void p2k_lwidth_(float *widthmm)
{p2k_lwidth(*widthmm);}

void p2k_lwidth(float widthmm)
{
/* scale the linewidth before passing it to the devdep routine */
if (debug == TRUE) fprintf(stderr,"***p2k_lwidth\n");
p2k_devdep_lwidth(widthmm*sc[scdep]);
currentlinewidth=widthmm;
}

/*****************************************************************************/
void p2k_line_(float *x0, float *y0, float *z0, float *x1, float *y1, float *z1)
/* draw a line from the current position to x,y,z, using the current
   drawing colour, line-thickness and line-style */
{p2k_line(*x0, *y0, *z0, *x1, *y1, *z1);}

void p2k_line(float x0, float y0, float z0, float x1, float y1, float z1)
{
if (debug == TRUE) fprintf(stderr,"***p2k_line\n");
p2k_move(x0,y0,z0);
p2k_draw(x1,y1,z1);
}

/*****************************************************************************/
/* move to the given x,y,z */
void p2k_move_(float *x, float *y, float *z)
{p2k_move(*x, *y, *z);}

void p2k_move(float x, float y, float z)
{
if (debug == TRUE) fprintf(stderr,"***p2k_move\n");
p2k_xyposn(x,y,z);
p2k_devdep_movto(x2d,y2d);
}

/*****************************************************************************/
/* set an origin at the given x,y,z relative to the current position */
void p2k_origin_(float *x0, float *y0, float *z0)
{p2k_origin(*x0, *y0, *z0);}

void p2k_origin(float x0, float y0, float z0)
{
if (debug == TRUE) fprintf(stderr,"***p2k_origin %f %f %f\n",x0,y0,z0);
/* unscale current spot position before calculating origin shifts */
ox=x2d;
oy=y2d;
o[0]=x0;
o[1]=y0;
o[2]=z0;
for (i=0; i<3; i++)
  {
  a=(o[i]/gr[i][grdep])*sc[scdep];
  ox=ox+a*cosx[i][twdep];
  oy=oy+a*sinx[i][twdep];
  }
p2k_push3(&ordep,&or[0][0],ox,oy,0.0);
}

/*****************************************************************************/
/* set the name of the output file to contain the postscript code */
/* If this is an environment variable, use its value, otherwise */
/* use it 'as-is' */
void p2k_outfile_(char *filename, int *nchars)
{p2k_outfile(filename, *nchars);}

void p2k_outfile(char *filename, int nchars)
{
char tempstr[1024]="";
char *envvar;
strncat(tempstr,filename,nchars);
while ((envvar=getenv(tempstr)) != NULL)
  {
  strcpy(tempstr,envvar);
  }
if (debug == TRUE) fprintf(stderr,"***p2k_outfile\n");
/* next line mod needed by REDHAT 7.0 gcc 2.96 found by 
			Roberto Marabini - jms 8?
if (fd != (FILE *) NULL)
	fclose(fd);		/* close any open file */
fd=fopen(tempstr,"w");		/* open the new one */
inited=FALSE;
if (fd == NULL)
  {
  fprintf(stderr,"p2k: Cant open output file %s\n",filename);
  exit(0);
  }
}

/*****************************************************************************/
/* draw a point at x,y,z */
void p2k_point_(float *x0, float *y0, float *z0)
{p2k_point(*x0, *y0, *z0);}

void p2k_point(float x0, float y0, float z0)
{
if (debug == TRUE) fprintf(stderr,"***p2k_point\n");
p2k_xyposn(x0, y0, z0);
p2k_devdep_pntto(x2d,y2d);
}

/*****************************************************************************/
/* pop an item off the given stack stack */
void p2k_pop1(int* depth)
{
(*depth)--;
if (*depth < 0)
  {
  fprintf(stderr,"origin/twist/grid/scale stack underflow\n");
  *depth=0;
  }
p2k_setfac();
}

/*****************************************************************************/
/* initiate a page-print */
void p2k_page_(void)
{p2k_page();}

void p2k_page(void)
{
if (debug == TRUE) fprintf(stderr,"***p2k_page\n");
p2k_devdep_page();
/* restore current linewidth and colour settings */
p2k_lwidth(currentlinewidth);
p2k_colour(currentcolour);
}

/*****************************************************************************/
/* push a new set of scale-factors resulting from an origin/twist/grid/scale */
void p2k_push3(int *depth, float *list, float p0, float p1, float p2)
{
(*depth)++;
if (*depth > STACKLIM)
  {
  fprintf(stderr,"origin/twist/grid/scale stack overflow\n");
  *depth=STACKLIM;
  }
list[0*(STACKLIM+1)+*depth]=p0;
list[1*(STACKLIM+1)+*depth]=p1;
list[2*(STACKLIM+1)+*depth]=p2;
p2k_setfac();
}

/*****************************************************************************/
/* return to previous grid settings */
void p2k_rgr_(void)
{p2k_rgr();}

void p2k_rgr(void)
{
if (debug == TRUE) fprintf(stderr,"***p2k_rgr\n");
p2k_pop1(&grdep);
}

/*****************************************************************************/
/* return to previous origin settings */
void p2k_ror_(void)
{p2k_ror();}

void p2k_ror(void)
{
if (debug == TRUE) fprintf(stderr,"***p2k_ror\n");
p2k_pop1(&ordep);
}

/*****************************************************************************/
/* return to previous scale settings */
void p2k_rscale_(void)
{p2k_rscale();}

void p2k_rscale(void)
{
if (debug == TRUE) fprintf(stderr,"***p2k_rscale\n");
scdep--;
if (scdep < 0)
  {
  fprintf(stderr,"scale stack underflow\n");
  scdep=0;
  }
}

/*****************************************************************************/
/* return to previous twist settings */
void p2k_rtw_(void)
{p2k_rtw();}

void p2k_rtw(void)
{
if (debug == TRUE) fprintf(stderr,"***p2k_rtw\n");
p2k_pop1(&twdep);
}

/*****************************************************************************/
/* set a new scale factor. This will also scale font-size and line-thickness */
void p2k_scale_(float *p0)
{p2k_scale(*p0);}

void p2k_scale(float p0)
{
if (debug == TRUE) fprintf(stderr,"***p2k_scale\n");
scdep++;
if (scdep > STACKLIM)
  {
  fprintf(stderr,"scale stack overflow\n");
  scdep=STACKLIM;
  }
sc[scdep]=p0;
p2k_setfac();
}

/*****************************************************************************/
/* calculate a new set of factors to be applied to all line end-point calcs */
void p2k_setfac()
{
if (debug == TRUE) fprintf(stderr,"***p2k_setfac. depths=%i %i %i %i\n",
grdep,ordep,scdep,twdep);
if (grdep<0 || ordep<0 || twdep<0 || scdep<0) return;
for (i=0; i<3; i++)
  {
  xfctr[i]=(1.0/gr[i][grdep])*cosx[i][twdep]*sc[scdep];
  yfctr[i]=(1.0/gr[i][grdep])*sinx[i][twdep]*sc[scdep];
  }
xfctr[3]=or[0][ordep];
yfctr[3]=or[1][ordep];
}

/*****************************************************************************/
/* set a font and char size for subsequent string drawing */
void p2k_font_(char *fontname, float *charheight_mm)
{p2k_font(fontname, *charheight_mm);}

void p2k_font(char *fontname, float charheight_mm)
{
if (debug == TRUE) fprintf(stderr,"***p2k_font\n");
currentcharheight=charheight_mm;
strcpy(currentfont,fontname);
p2k_devdep_font(fontname,sc[scdep]*charheight_mm);
}

/*****************************************************************************/
void p2k_symbol_(int *symtype, float *size, float *angle)
{p2k_symbol(*symtype, *size, *angle);}

void p2k_symbol(int symtype, float size, float angle)
{
if (debug == TRUE) fprintf(stderr,"***p2k_symbol\n");
p2k_here();
/*p2k_grid(1.0/size,1.0/size,1.0);*/
p2k_grid(A4WIDTHmm/(2*size),A4WIDTHmm/(2*size),1.0);
p2k_twist(angle,angle+90.0,0.0);
switch (symtype)
  {
  case 1:	/* triangle */
    {
    p2k_line(-0.433,-0.25,0.0,0.0,0.5,0.0);
    p2k_draw(0.433,-0.25,0.0);
    p2k_draw(-0.433,-0.25,0.0);
    break;
    }
  case 2:	/* square */
    {
    p2k_line(-0.5,-0.5,0.0,-0.5,0.5,0.0);
    p2k_draw(0.5,0.5,0.0);
    p2k_draw(0.5,-0.5,0.0);
    p2k_draw(-0.5,-0.5,0.0);
    break;
    }
  case 3:	/* plus-sign */
    {
    p2k_line(-0.5,0.0,0.0, 0.5,0.0,0.0);
    p2k_line(0.0,-0.5,0.0, 0.0,0.5,0.0);
    break;
    }
  case 4:	/* open dot */
    {
    p2k_circle(0.5);
    break;
    }
  case 5:	/* X sign */
    {
    p2k_line(-0.5,-0.5,0.0, 0.5,0.5,0.0);
    p2k_line(-0.5,0.5,0.0, 0.5,-0.5,0.0);
    break;
    }
  case 6:	/* inverted triangle */
    {
    p2k_line(-0.433,0.25,0.0, 0.0,-0.5,0.0);
    p2k_draw(0.433,0.25,0.0);
    p2k_draw(-0.433,0.25,0.0);
    break;
    }
  case 7:	/* square with + inside */
    {
    p2k_line(-0.5,-0.5,0.0,-0.5,0.5,0.0);
    p2k_draw(0.5,0.5,0.0);
    p2k_draw(0.5,-0.5,0.0);
    p2k_draw(-0.5,-0.5,0.0);
    p2k_line(-0.5,0.0,0.0, 0.5,0.0,0.0);
    p2k_line(0.0,-0.5,0.0, 0.0,0.5,0.0);
    break;
    }
  case 8:	/* open dot with + inside */
    {
    p2k_circle(0.5);
    p2k_line(-0.5,0.0,0.0, 0.5,0.0,0.0);
    p2k_line(0.0,-0.5,0.0, 0.0,0.5,0.0);
    break;
    }
  case 9:	/* open dot with X inside */
    {
    p2k_circle(0.5);
    p2k_twist(45.0, 135.0, 0.0);
    p2k_line(-0.5,0.0,0.0, 0.5,0.0,0.0);
    p2k_line(0.0,-0.5,0.0, 0.0,0.5,0.0);
    p2k_rtw();
    break;
    }
  case 10:	/* square with X inside */
    {
    p2k_line(-0.5,-0.5,0.0,-0.5,0.5,0.0);
    p2k_draw(0.5,0.5,0.0);
    p2k_draw(0.5,-0.5,0.0);
    p2k_draw(-0.5,-0.5,0.0);
    p2k_line(-0.5,-0.5,0.0, 0.5,0.5,0.0);
    p2k_line(-0.5,0.5,0.0, 0.5,-0.5,0.0);
    break;
    }
  case 11:	/* square rotated by 45deg */
    {
    p2k_twist(45.0, 135.0, 0.0);
    p2k_line(-0.5,-0.5,0.0,-0.5,0.5,0.0);
    p2k_draw(0.5,0.5,0.0);
    p2k_draw(0.5,-0.5,0.0);
    p2k_draw(-0.5,-0.5,0.0);
    p2k_rtw();
    break;
    }
  case 12:	/* square with + inside rotated by 90deg */
    {
    p2k_twist(45.0, 135.0, 0.0);
    p2k_line(-0.5,-0.5,0.0,-0.5,0.5,0.0);
    p2k_draw(0.5,0.5,0.0);
    p2k_draw(0.5,-0.5,0.0);
    p2k_draw(-0.5,-0.5,0.0);
    p2k_line(-0.5,0.0,0.0, 0.5,0.0,0.0);
    p2k_line(0.0,-0.5,0.0, 0.0,0.5,0.0);
    p2k_rtw();
    break;
    }
  case 13:	/* square with X inside rotated by 90deg*/
    {
    p2k_twist(45.0, 135.0, 0.0);
    p2k_line(-0.5,-0.5,0.0,-0.5,0.5,0.0);
    p2k_draw(0.5,0.5,0.0);
    p2k_draw(0.5,-0.5,0.0);
    p2k_draw(-0.5,-0.5,0.0);
    p2k_line(-0.5,-0.5,0.0, 0.5,0.5,0.0);
    p2k_line(-0.5,0.5,0.0, 0.5,-0.5,0.0);
    p2k_rtw();
    break;
    }
  }
  
p2k_rtw();
p2k_rgr();
p2k_ror();
}

/*****************************************************************************/
/* draw the given string in the currently selected font and colour at the given
   angle to the horizontal. tstring expects a null-terminated string */
void p2k_string_(char *string, int *nchars, float *angle)
{p2k_string(string, *nchars, *angle);}

void p2k_tstring_(char *string, float *angle)
{p2k_tstring(string, *angle);}

void p2k_string(char *string, int nchars, float angle)
{
char	tempstr[1024];
if (debug == TRUE) fprintf(stderr,"***p2k_string\n");
strncpy(tempstr,string,nchars);
tempstr[nchars]=0;
p2k_devdep_string(tempstr, angle+total_xtwist());
}

void p2k_tstring(char *string, float angle)
{
if (debug == TRUE) fprintf(stderr,"***p2k_text\n");
p2k_devdep_string(string, angle+total_xtwist());
}

/*****************************************************************************/
/* draw the given string in the currently selected font and colour at the given
   angle to the horizontal, centred on the current position.
   tcstring expects a null-terminated string */
void p2k_cstring_(char *string, int *nchars, float *angle)
{p2k_cstring(string, *nchars, *angle);}

void p2k_tcstring_(char *string, float *angle)
{p2k_tcstring(string, *angle);}

void p2k_cstring(char *string, int nchars, float angle)
{
char	tempstr[1024];
if (debug == TRUE) fprintf(stderr,"***p2k_cstring\n");
strncpy(tempstr,string,nchars);
tempstr[nchars]=0;
p2k_devdep_cstring(tempstr, angle+total_xtwist());
}

void p2k_tcstring(char *string, float angle)
{
if (debug == TRUE) fprintf(stderr,"***p2k_ctstring\n");
p2k_devdep_cstring(string, angle+total_xtwist());
}

/*****************************************************************************/
/* set the angles (in deg) of the plot-system x,y and z axes relative to
   the current x-axis (initially the current xaxis)*/
void p2k_twist_(float *p0, float *p1, float *p2)
{p2k_twist(*p0, *p1, *p2);}

void p2k_twist(float p0, float p1, float p2)
{
if (debug == TRUE) fprintf(stderr,"***p2k_twist\n");
p[0]=p0+tw[0][twdep];
p[1]=p1+tw[0][twdep];
p[2]=p2+tw[0][twdep];
twdep++;
for (i=0; i<3; i++)
  {
  tw[0][twdep]=p0;
  tw[1][twdep]=p1;
  tw[2][twdep]=p2;
  angle=p[i]/180*PI;
  cosx[i][twdep]=cosf(angle);
  sinx[i][twdep]=sinf(angle);
  }
p2k_setfac();
}

/*****************************************************************************/
/* calculate the 2-d position on the paper of the given 3-d position */
void p2k_xyposn(float p0, float p1, float p2)
{
if (debug == TRUE) fprintf(stderr,"***p2k_xyposn\n");
x2d=p0*xfctr[0]+p1*xfctr[1]+p2*xfctr[2]+xfctr[3];
y2d=p0*yfctr[0]+p1*yfctr[1]+p2*yfctr[2]+yfctr[3];
}

/*****************************************************************************
 Device-dependent Postscript routines
******************************************************************************/

/*****************************************************************************/
void p2k_devdep_circle(float radius)
{
if (debug == TRUE) fprintf(stderr,"***p2k_devdep_circle\n");
p2k_devdep_init();
/* stroke any path before starting a new one */
if (pathstarted != 0)
  {
  fprintf(fd,"S\n");
  pathstarted=0;
  }
fprintf(fd,"currentpoint\nnewpath\n%7.2f 0 360 arc\nclosepath\n",
radius*A4scale);
fprintf(fd,"S\n");
pathstarted=0;
}

/*****************************************************************************/
void p2k_devdep_close(void)
/* close the current plot output stream */
{
if (debug == TRUE) fprintf(stderr,"***p2k_devdep_close\n");
if (inited==FALSE) return;
fclose(fd);
inited=FALSE;
fd=NULL;
}

/*****************************************************************************/
void p2k_devdep_colour(int colour)
{
if (debug == TRUE) fprintf(stderr,"***p2k_devdep_colour\n");
p2k_devdep_init();
/* stroke any path in the current colour before changing colours */
if (pathstarted != 0)
  {
  fprintf(fd,"S\n");
  pathstarted=0;
  }
fprintf(fd,"%5.3f %5.3f %5.3f %5.3f C\n",
        cmyk[colour][0],cmyk[colour][1],cmyk[colour][2],cmyk[colour][3]);
}

/*****************************************************************************/
void p2k_devdep_cmyk_colour(float rc, float rm, float ry, float rk)
{
if (debug == TRUE) fprintf(stderr,"***p2k_devdep_cmyk_colour\n");
p2k_devdep_init();
/* stroke any path in the current colour before changing colours */
if (pathstarted != 0)
  {
  fprintf(fd,"S\n");
  pathstarted=0;
  }
fprintf(fd,"%5.3f %5.3f %5.3f %5.3f C\n",
        rc,rm,ry,rk);
}

/*****************************************************************************/
void p2k_devdep_rgb_colour(float rr, float rg, float rb)
{
if (debug == TRUE) fprintf(stderr,"***p2k_devdep_rgb_colour\n");
p2k_devdep_init();
/* stroke any path in the current colour before changing colours */
if (pathstarted != 0)
  {
  fprintf(fd,"S\n");
  pathstarted=0;
  }
fprintf(fd,"%5.3f %5.3f %5.3f R\n",
        rr,rg,rb);
}

/*****************************************************************************/
void p2k_devdep_init()
{
if (inited==TRUE) return;
if (debug == TRUE) fprintf(stderr,"***p2k_devdep_init\n");
if (fd == NULL) fd=stdout;
fprintf(fd,"%%!PS\ninitgraphics\n");
fprintf(fd,"/C {setcmykcolor} def\n");
fprintf(fd,"/R {setrgbcolor} def\n");
fprintf(fd,"/M {moveto} def\n");
fprintf(fd,"/L {lineto} def\n");
fprintf(fd,"/S {currentpoint stroke moveto} def\n");
inited=TRUE;
pathstarted=0;
}

/*****************************************************************************/
void p2k_devdep_linto(float x, float y)
{
if (debug == TRUE) fprintf(stderr,"***p2k_devdep_linto\n");
p2k_devdep_init();
fprintf(fd,"%7.2f %7.2f L\n",x*A4scale+A4xoffset,y*A4scale+A4yoffset);
pathstarted++;
if (pathstarted > 1000)
  {
  fprintf(fd,"S\n");
  pathstarted=0;
  }
}

/*****************************************************************************/
void p2k_devdep_lwidth(float widthmm)
{
if (debug == TRUE) fprintf(stderr,"***p2k_devdep_lwidth\n");
p2k_devdep_init();
/* stroke any outstanding path before changing linewidth */
if (pathstarted != 0)
  {
  fprintf(fd,"S\n");
  pathstarted=0;
  }
/* force an absolute scale so we can set the line width  */
fprintf(fd,"[0 0 0 0 0 0] currentmatrix\n");
fprintf(fd,"[0 0 0 0 0 0] defaultmatrix setmatrix\n");
fprintf(fd,"%f %f scale\n",points_per_mm,points_per_mm);
fprintf(fd,"%f setlinewidth\nsetmatrix\n",widthmm);
}

/*****************************************************************************/
void p2k_devdep_movto(float x, float y)
{
if (debug == TRUE) fprintf(stderr,"***p2k_devdep_movto\n");
p2k_devdep_init();
fprintf(fd,"%7.2f %7.2f M\n",x*A4scale+A4xoffset,y*A4scale+A4yoffset);
pathstarted++;
}

/*****************************************************************************/
void p2k_devdep_page(void)
{
if (debug == TRUE) fprintf(stderr,"***p2k_devdep_page\n");
if (pathstarted != 0)
  {
  fprintf(fd,"S\n");
  pathstarted=0;
  }
/* save any context (colour and linethickness)*/
fprintf(fd,"showpage\n");
pathstarted=0;
}

/*****************************************************************************/
void p2k_devdep_pntto(float x, float y)
{
if (debug == TRUE) fprintf(stderr,"***p2k_devdep_pntto\n");
p2k_devdep_init();
fprintf(stderr,"p2k_devdep_pntto not implemented\n");
pathstarted++;
}

/*****************************************************************************/
void p2k_devdep_font(char *fontname, float charheight_mm)
{
if (debug == TRUE) fprintf(stderr,"***p2k_devdep_font\n");
fprintf(fd, "/%s %f selectfont\n",fontname,charheight_mm*points_per_mm);
}

/*****************************************************************************/
void p2k_devdep_string(char *string, float angle)
{
int i;
if (debug == TRUE) fprintf(stderr,"***p2k_devdep_string\n");
fprintf(fd,"%7.2f rotate\n(",angle);
/* all ( and ) chars in the string must be escaped  with a \ */
for (i=0; i<(int)strlen(string); i++)
  {
  if (string[i]=='(' || string[i]==')' || string[i]=='\\') fprintf(fd,"\\");
  fprintf(fd,"%c",(int)string[i]);
  }
fprintf(fd,") show\n");
fprintf(fd,"%7.2f rotate\n",-angle);
pathstarted++;
}

/*****************************************************************************/
void p2k_devdep_cstring(char *string, float angle)
{
int i;
if (debug == TRUE) fprintf(stderr,"***p2k_devdep_cstring\n");
fprintf(fd,"%7.2f rotate\n",angle);
/* output the string so that Postscript can calculate its length */
/* all \ ( and ) chars in the string must be escaped  with a \ */
fprintf(fd,"(");
for (i=0; i<(int)strlen(string); i++)
  {
  if (string[i]=='(' || string[i]==')' || string[i]=='\\') fprintf(fd,"\\");
  fprintf(fd,"%c",(int)string[i]);
  }
fprintf(fd,") stringwidth\n");
/* halve the offsets returned by stringwidth and move back by these */
fprintf(fd,"0.5 mul neg exch 0.5 mul neg exch rmoveto\n");
/*and re-issue the string for writing */
fprintf(fd,"(");
for (i=0; i<(int)strlen(string); i++)
  {
  if (string[i]=='(' || string[i]==')' || string[i]=='\\') fprintf(fd,"\\");
  fprintf(fd,"%c",(int)string[i]);
  }
fprintf(fd,") show\n");
fprintf(fd,"%7.2f rotate\n",-angle);
pathstarted++;
}

