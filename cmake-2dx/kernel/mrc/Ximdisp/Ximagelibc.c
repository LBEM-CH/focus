/**************************************************************************/
/*	Ximagelibc.c	v9.41				15.07.98	  */
/*	Library of c routines called by the fortran library Ximagelibf.f  */
/*	to display a byte map, with colour tables, draw lines, circles,   */
/*	vectors. Mouse buttons operate as :				  */
/*	lh      : read cursor position					  */
/*	middle  : down starts to pan the image				  */
/*		  up concludes the pan operation			  */
/*	rh	: zooms an area about the cursor position		  */
/*	ctrl/m  : re-displays a hidden menu				  */
/*	ctrl/l  : re-displays a hidden label				  */
/*	ctrl/s  : re-displays a hidden slider				  */
/*	ctrl/btn1down : starts rubberbanding line			  */
/*	ctrl/btn1up : pauses rubberbanding line and adds it to vector     */
/*      list								  */
/*      btn1 double click finishes rubberbanding			  */
/*									  */
/*	Control Widgets are managed as follows :			  */
/*	label or iobox top left corner 					  */
/*	slider bar for adjusting contrast of image below label or iobox	  */
/*	menu beneath label/iobox and slider				  */
/*									  */
/*      Multiple pixmaps have the ability to be scrolled fast but 	  */
/*	pan/zoom disabled						  */
/*									  */
/* Note :								  */
/*	Three areas concerned with map display ,			  */
/*	1) image_area - window filled with the map - variable size to	  */
/*			   fit the input map				  */
/*	2) display window - device dependant screen size or smaller	  */
/*	3) screen pixmap - variable size, but never less than screen size */
/*			   has map drawn into it			  */
/*	map size parameters are as follows :				  */
/*	window_width, window_height are the actual window size		  */
/*	max_window_width,max_window_height     "       "    "		  */
/*	map_width,map_height requested map size				  */
/*	image_width,image_height set to maximum of window size or 	  */
/*	requested map size. 						  */
/*									  */
/**************************************************************************/
/*
 *  So that we can use fprintf:
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* 
 * Standard Toolkit include files:
 */
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xproto.h>
#include <X11/cursorfont.h>

/*
 * Public include files for widgets :
 */
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Simple.h>
#include <X11/Xaw/Text.h>

#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/AsciiSrc.h>
#include <X11/Xaw/AsciiSink.h>
#include <X11/Xaw/Text.h>
#include <X11/Xaw/TextSrc.h>
#include <X11/Xaw/TextSink.h>


/******************************************************************/
/*
 * count declarations 
 */
int		i;
int		j;
int		k;
int		n;

int		imin_ (int i1,int i2);
int		imax_ (int i1,int i2);
/*
 * Window declarations :
 */

XtAppContext 	app_context;
/*Cardinal	zero=0;*/
int		zero=0;
Display		*display_id;
Visual		*visual_id;
Window		topLevel_window_id, 
		colour_bar1_window_id,
		colour_bar2_window_id,
		ioBox_window_id,
		label_window_id,
		sliderBox_window_id,
		image_area_window_id,
		menuBox_window_id,
		overlay_area_window_id,
		zoom_area_window_id,
		root_id;
int        	event_true;
int             expose_flag;
Dimension	border_size = 50;
Dimension	iobox_height;
Dimension	iobox_width = 800;
Dimension	label_width = 790;

/* window sizes */
unsigned int    window_width;
unsigned int    window_height;
int 		root_x;
int 		root_y;

/*
 * machine info
*/
int		msbfirst;

/*
 * Widget declarations
 */

Widget	topLevel, 
        mainForm, 
        image_area, 
	colour_bar1,
	colour_bar2,
        menuBox,
	label,
        ioBox,
	ioBox_label,
	ioBox_text,
	overlay_area,
	slider1,
	slider2,
	sliderBox,
	slider_label,
	zoom_area;

/*
 * Action declarations :
 */

static char Trans[] = "Ctrl<Btn1Up>: action_rubber_stop()\n\
                       Ctrl<Btn1Down>: action_rubber_read()\n\
                       <Btn1Down>(2+): action_rubber_finish()\n\
                       <Btn1Down>: action_read_pointer()\n\
		       <Btn2Down>: action_pan_start()\n\
		       <Btn2Up>: action_pan_finish()\n\
		       <Btn3Down>: action_zoom_init()\n\
		       Ctrl<Key>m: actionmenudisplay_()\n\
                       Ctrl<Key>l: action_label_display_()\n\
                       Ctrl<Key>s: action_slider_display_()";

XtTranslations trans_table;

static char Io_trans[] = "<Key>Return: action_iobox()";
XtTranslations iobox_trans_table;

/*
 * callback declarations :
   icallback_value = -1 no event
                      0 read pointer
                      1 - 99 menu item
                     100 iobox
                     101 slider1
		     102 slider2
 		     103 zoom
                     104 rubber on
                     105 rubber off
 */
int		icallback_value;

/*
 * colour table declarations
 */
Colormap	colourmap;
XColor		colours[256];
int		colourmap_size;
unsigned long	colour_white;
unsigned long	colour_red;
unsigned long	colour_black;
unsigned long	colour_green;
unsigned long	colour_blue;
int		ncolours;
int 		colour_start;
int 		colour_entries;

/*
 * Graphics context :
 */ 

GC		gc_draw_vector[2];
GC		gc_remove_vector;
GC		gc_fill_rectangle;
int		scr_num;
int		max_boxes = 10000;
int		max_circles = 10000;
int		max_elipses = 10000;
int		max_points = 10000;
int		max_lines = 16384;
int		max_text = 4096;
unsigned int	line_width = 0;
int		cap_style = CapButt;
int		join_style = JoinMiter;
int		dash_style;
int		style;

/*
 * Cursor :
 */

Cursor          cursor;
Pixmap		cursor_shape_pixmap;
Pixmap		cursor_mask_pixmap;
static char	cursor_shape[30];
static char	cursor_mask[30];
unsigned int	cursor_size = 15;
unsigned int	cursor_depth = 1;
XColor		cursor_background;
XColor		cursor_foreground;
unsigned int	cursor_x = 7;
unsigned int	cursor_y = 7;
int		arrow = XC_top_left_arrow;
int		cross = XC_crosshair;
int		circle = XC_circle;
int		hand = XC_hand2;
int		bird = XC_gobbler;
int		skull = XC_pirate;
int		crosshair_x;
int		crosshair_y;


/*
 * Font :
 */

XFontStruct	*font_default;
XFontStruct	*font_text;
Font		font_id;
char		fontname1[] = "*helvetica-bold-r-normal-*-17-120*100*";
char		fontname2[] = "*fixed-bold-r-normal-*-13-120-*-75-c-70*";
XCharStruct	min_bounds;
XCharStruct	max_bounds;
short int	ascent;
short int	descent;
short int	font_height;
/* extra font declarations for font choice */
char		*fonts[100];
char		**courier_fonts;
char		**fixed_fonts;
char		**helvetica_fonts;
char		**times_fonts;
char		courier_font[] = "*courier-bold-*";
char		fixed_font[] = "*fixed-bold-*";
char		helvetica_font[] = "*helvetica-bold-*";
char		times_font[] = "*times-bold-*";
int		font_number;
int		font_length = 64;
int		font_spacer = 4;
int		max_fonts = 20;
int		maxfontsperstyle = 20;
int		no_fonts;
int		total_fonts;


/* LSB server -  fine crosshair cursor */

static char cursor_shape1_lsb[] =
	{
   0xfe, 0xbf, 0xfd, 0xdf, 0xfb, 0xef, 0xf7, 0xf7, 0xef, 0xfb, 0xdf, 0xfd,
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xdf, 0xfd, 0xef, 0xfb, 0xf7, 0xf7,
   0xfb, 0xef, 0xfd, 0xdf, 0xfe, 0xbf
	};

static char cursor_mask1_lsb[] =
	{
   0xfe, 0xbf, 0xfd, 0xdf, 0xfb, 0xef, 0xf7, 0xf7, 0xef, 0xfb, 0xdf, 0xfd,
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xdf, 0xfd, 0xef, 0xfb, 0xf7, 0xf7,
   0xfb, 0xef, 0xfd, 0xdf, 0xfe, 0xbf
	};

/* MSB server fine crosshair cursor */

static char cursor_shape1_msb[] =
	{
   0x01, 0x40, 0x02, 0x20, 0x04, 0x10, 0x08, 0x08, 0x10, 0x04, 0x20, 0x02,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20, 0x02, 0x10, 0x04, 0x08, 0x08,
   0x04, 0x10, 0x02, 0x20, 0x01, 0x40
	};

static char cursor_mask1_msb[] = 
	{
   0x01, 0x40, 0x02, 0x20, 0x04, 0x10, 0x08, 0x08, 0x10, 0x04, 0x20, 0x02,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20, 0x02, 0x10, 0x04, 0x08, 0x08,
   0x04, 0x10, 0x02, 0x20, 0x01, 0x40
	};

/* LSB server -  box crosshair */

static char cursor_shape2_lsb[] =
	{
   0x3f, 0x7e, 0x3f, 0x7e, 0x3f, 0x7e, 0x3f, 0x7e, 0x0f, 0x78, 0xef, 0x7b,
   0xe0, 0x03, 0xe0, 0x03, 0xe0, 0x03, 0xef, 0x7b, 0x0f, 0x78, 0x3f, 0x7e,
   0x3f, 0x7e, 0x3f, 0x7e, 0x3f, 0x7e
	};

static char cursor_mask2_lsb[] =
	{
   0x1f, 0x7c, 0x1f, 0x7c, 0x1f, 0x7c, 0x1f, 0x7c, 0x0f, 0x78, 0xe0, 0x03,
   0xe0, 0x03, 0xe0, 0x03, 0xe0, 0x03, 0xe0, 0x03, 0x0f, 0x78, 0x1f, 0x7c,
   0x1f, 0x7c, 0x1f, 0x7c, 0x1f, 0x7c
	};

/* MSB server -  box crosshair cursor */

static char cursor_shape2_msb[] =
	{
   0xc0, 0x81, 0xc0, 0x81, 0xc0, 0x81, 0xc0, 0x81, 0xf0, 0x87, 0x10, 0x84,
   0x1f, 0xfc, 0x1f, 0xfc, 0x1f, 0xfc, 0x10, 0x84, 0xf0, 0x87, 0xc0, 0x81,
   0xc0, 0x81, 0xc0, 0x81, 0xc0, 0x81
	};

static char cursor_mask2_msb[] = 
	{
   0xc0, 0x81, 0xc0, 0x81, 0xc0, 0x81, 0xc0, 0x81, 0xf0, 0x87, 0x10, 0x84,
   0x1f, 0xfc, 0x9f, 0xfc, 0x1f, 0xfc, 0x10, 0x84, 0xf0, 0x87, 0xc0, 0x81,
   0xc0, 0x81, 0xc0, 0x81, 0xc0, 0x81
	};

/* LSB server -  centre giant crosshair cursor */

static char cursor_shape3_lsb[] =
	{
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	};

static char cursor_mask3_lsb[] =
	{
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
	};

/* MSB server centre giant crosshair cursor */

static char cursor_shape3_msb[] =
	{
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	};

static char cursor_mask3_msb[] = 
	{
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	};

/*
 * Box arrays
*/

int		box_x[4096];
int		box_y[4096];
unsigned int	box_width[4096];
unsigned int	box_height[4096];
int		box_style[4096];
int		nboxes = 0;

/*
 * Circle arrays
 */

int		irad;
int		circle_x[4096];
int		circle_y[4096];
int		circle_diam[4096];
int		circle_style[4096];
int		circle_angle = 360 * 64;
int		ncircles = 0;

/*
 * Elipse arrays
 */

int             iradx;
int             irady;
int             elipse_x[4096];
int             elipse_y[4096];
int             elipse_diamx[4096];
int             elipse_diamy[4096];
int             elipse_ang1[4096];
int             elipse_style[4096];
int             nelipses = 0;
float		cs[361];
float		sn[361];

/*
 * Line arrays
 */

int		line_x1[16384];
int		line_y1[16384];
int		line_x2[16384];
int		line_y2[16384];
int		line_style[16384];
XSegment	segments[32768];
int		nlines = 0;

/*
 * Text arrays
 */

int		text_x[4096];
int		text_y[4096];
char		text_string[4096][80];
int		text_lengths[4096];
Font		text_font[4096];
int		ntext = 0;
int		string_length = 80;

/*
 * Point arrays
 */

int		point_x[4096];
int		point_y[4096];
int		npoints = 0;

/*
 * overlay area declarations
 */
static char	overlay_pointer[512*514];
int		overlay_flag;
XImage		*overlay_map;
Pixmap		overlay_pixmap;
int		overlay_height;
int		overlay_width;
int		overlay_pointer_x;
int		overlay_pointer_y;
int		overlay_window_x;
int		overlay_window_y;


/*
 * zoom area declarations 
 */
int		zoom_height;
int		zoom_width;
XImage		*zoom_map;
static char	zoom_area_pointer[1034*1034];
int		zoom_factor_x;
int		zoom_factor_y;
int		zoom_pointer_x;
int		zoom_pointer_y;
Pixmap		zoom_pixmap;
int		zoom_window_x1;
int		zoom_window_y1;
int		zoom_window_x2;
int		zoom_window_y2;
int		zoom_flag;
int		zoom_x1;
int		zoom_y1;
int		zoom_x2;
int		zoom_y2;
int		zoom_centre_x;
int		zoom_centre_y;
unsigned int	zoom_diam_x;
unsigned int	zoom_diam_y;

/*
 * Event declarations 
 */
XEvent		event_return;
int		event_flag = 0;
int		event_proc = 0;

/*
 * external fortran declarations :
*/
extern  int ximagecallback_();
extern  int ximagedrawlines_();
extern  int ximagemenudisplay_();
extern  int ximagemenuhide_();

/*
 * iobox declarations
 */
char			store_string[80] = {" "};
int			expose_iobox_flag;
int			iobox_flag;
Dimension               iobox_gap = 20;


/*
 * label declarations
 */
int			expose_label_flag;
int			label_flag;

/*
 * menu declarations
 */
static	char		*menu_pointer[100];
int			expose_menu_flag;
int			menu_flag;

/*
 * map declarations
 */
/*Cand*/
/*static char	image_pointer[8192*8192];*/
static char	image_pointer[4*8192*8192];
unsigned int	image_depth;
unsigned int	image_height;
unsigned int	image_width;
unsigned int	map_height;
unsigned int	map_width;
int		map_size;
int		image_format;
int		image_offset = 0;
int		image_bitmap_pad = 8;
int		image_bytes_per_line = 0;
int 		image_source_x = 0;
int 		image_source_y = 0;
int 		image_dest_x;
int 		image_dest_y;
XImage		*image;
Pixmap		image_pixmap;
int     	image_pointer_x;
int	        image_pointer_y;
int		image_flag;
XImage		*readimage;
static char	*dataline;

/* colour bar declarations */
XImage		*colour_bar_image;
int		colour_bar_flag;
static char	colour_bar_pointer[25*128];
char		colour_bar_map[25][128];

/* pan pixmap declarations */
Pixmap		screen_pixmap[200];
int        	no_maps;
int             map_no;
int		max_maps = 200;
int		pan_pointer_x1;
int		pan_pointer_y1;
int		pan_pointer_x2;
int		pan_pointer_y2;
int		pan_x;
int		pan_y;
int		pan_min = 10;
int		pan_flag;

/* pointer tracking declarations */
int		ichange_origin;
int		pointer_flag;
int		pointer_text_length;
int		pointer_x;
int		pointer_y;
int		pointer_shift_max = 10;
int		pointer_shift_x;
int		pointer_shift_y;
int		pointer_save_x;
int		pointer_save_y;
char		pointer_text[20];

/* rubber banding declarations */
int		rubber_circle_x;
int		rubber_circle_y;
int		rubber_circle_diam;
int		rubber_elipse_x;
int		rubber_elipse_y;
int		rubber_elipse_diamx;
int		rubber_elipse_diamy;
int		rubber_elipse_angle;
int		rubber_flag;
int		rubber_min_x;
int		rubber_min_y;
int		rubber_max_x;
int		rubber_max_y;
int		rubber_size_x;
int		rubber_size_y;
int		rubber_start_x;
int		rubber_start_y;
int		rubber_finish_x;
int		rubber_finish_y;
int		rubber_type;

/* slider bar declarations */
float   	slider_position;
int		expose_slider_flag;
int		ksliders;
int		slider_flag;
float		slider1_start_position;
float		slider2_start_position;
float		slider_size = 0.0;
Dimension	slider_height = 25;
Dimension	slider_width = 128;

/******************************************************************/
/* list of functions */
/******************************************************************/
void	convert_to_image (int x_zoom, int y_zoom,
			  int *x_image, int *y_image);
void	convert_to_zoom (int x_image,int y_image,
			 int *x_zoom,int *y_zoom);
void    expose_iobox ();
void    expose_label ();
void    expose_menu ();
void    expose_slider ();
void	giant_cursor_draw ();
void	giant_cursor_remove ();
void	pointer_track ();
void	pointertrackon_ (int *point_shift_x, int *point_shift_y, 
		 int *iturn_origin);
void	pointertrackoff_ ();
void    ReDraw_image ();
void 	ReDraw_overlay ();
void	ReDraw_vectors ();
void 	ReDraw_zoom ();
void 	readpointer_ (int *image_pointer_x, int *image_pointer_y);
void 	rubberenable_ (int *rflag);
void 	rubberdrawbox ();
void 	rubberdrawcircle ();
void 	rubberdrawelipse ();
void 	rubberdrawline ();
void 	rubberread_ (int *ix1, int *iy1, int *ix2, int *iy2);
void 	rubberremoveline_ (int *ix1, int *iy1, int *ix2, int *iy2);
void 	readslider_ (float *slider_percent);
void	action_iobox ();
void	action_label_display_ ();
void	actionmenudisplay_ ();
void 	action_read_pointer ();
void    setpan_(int *x, int *y);
void 	action_pan_start ();
void 	action_pan_finish ();
void 	action_rubber_read ();
void 	action_rubber_stop ();
void 	action_rubber_finish ();
void	action_slider_display_ ();
void	action_slider_callback (Widget scrollbar, XtPointer client_data,
			  XtPointer percent);
void 	action_zoom_init ();
void    changecursor_ (int *icurs);
void	changefont_ (int *fontnumber);
void	changezoom_ (int *zoom_fx, int *zoom_fy,
		 unsigned int *zoom_w, unsigned int *zoom_h);
void	checkimage_ (int *nx, int *ny, int *nmaps);
void	clearimage_ ();
void	colourbarinit();
void	colourbarhide();
void	delay_ (int *idelay);
void	drawbox_ (int *ix1, int *iy1, int *ix2, int *iy2);
void 	drawcircle_ (int *ix, int *iy, float *rad);
void    drawelipse_ (int *ix, int *iy, float *radx, float *rady, float *rang);
void 	drawlines_  (int *ix1, int *iy1, int *ix2, 
		      int *iy2, int *ilines);

void	drawimage_  (int *nx, int *ny, int *isec, char *map, int *cflag);
void 	drawpixmap_ (int *isec);
void	drawpoint_   (int *ix, int *iy);
void 	drawtext_   (int *ix, int *iy, 
		char *text_pointer, int *length);
int	error_trap(Display *error_id, XErrorEvent *p_error);
void 	event_get_next_ (int *event_true);
void 	eventdispatch_ ();
void	init_ (int *max_window_width,  int *max_window_height,
	       int *max_zoom_size, char *fontlist, int *nfonts);
void	ioboxdisplay_ (char *out_string, char *edit_string, int *istrings);
void	ioboxreadstring_ (char *in_string);
void	ioboxhide_ ();
void	labeldisplay_ (char *label_string);
void	labelhide_ ();
void	menu_callback(Widget w, XtPointer client_data,
			XawListReturnStruct *call_data);
void	menuevent_();
void 	menuhide_ ();
void	menuinit_ (int *nitems, int *max_length, char *menulist);
void	overlayhide_ ();
void	overlayinit_ (int *ix, int *iy, int *overlay_w, int *overlay_h,
			char *omap);
void	readimage_  (int *icol, int *irow, int *ipixmap, char *pixels);
void	setcolourtable_ 
	(int *index_start, int *nentries, 
	int *red_pointer, int *green_pointer, int *blue_pointer);
void	quit_ ();
void	removebox_ (int *ix1, int *iy1, int *ix2, int *iy2);
void 	removecircle_ 
	(int *ix, int *iy,float *rad);
void 	removeelipse_ 
        (int *ix, int *iy, float *radx, float *rady, float *rang);
void 	removelines_ 
	(int *ix1, int *iy1, int *ix2, int *iy2, int *ilines);
void	removepoint_ (int *ix, int *iy);
void	removetext_ (int *ix, int *iy, char *text_pointer, int *length);
void 	removevectors_ ();
void	setdash_ ();
void	sliderhide_ ();
void	sliderinit_ (int *nsliders, float *slider1_start, 
                                     float *slider2_start);
void	setsolid_ ();
void	zoominit_ ();
void 	zoom_display ();
void 	zoomhide_ ();
/*********************************************************************/
/*	routines to be called before action routines */
/*********************************************************************/
/*********************************************************************/
/* change font type */
/*********************************************************************/
void	changefont_ (int *fontnumber)
{
	if((font_text = 
		XLoadQueryFont(display_id, fonts[*fontnumber])) == NULL)
		{
		fprintf(stderr,"Cannot load font number %d\n",*fontnumber);
		*fontnumber = -*fontnumber;
		return;
		}
	font_id = font_text->fid;
	XSetFont(display_id, gc_draw_vector[0], font_id);
	XSetFont(display_id, gc_draw_vector[1], font_id);
}
/*********************************************************************/
/* change zoom parameters */
/*********************************************************************/
void	changezoom_ (int *zoom_fx, int *zoom_fy, 
		unsigned int *zoom_w, unsigned int *zoom_h)
{
/* hide and destroy the zoom window if already present */
	if(zoom_flag != 0) 
		zoomhide_();

/* initialize */
	zoom_width = *zoom_w;
	zoom_height = *zoom_h;
	zoom_factor_x = *zoom_fx;
	zoom_factor_y = *zoom_fy;

/* force even zoom factors, integral odd number in zoom area
	width and height */
	if(zoom_factor_x != 1)
	{
		zoom_factor_x = (zoom_factor_x / 2) * 2;
		zoom_width = ((((zoom_width / zoom_factor_x) / 2) * 2) + 1) 
			* zoom_factor_x;
	}

	if(zoom_factor_y != 1)
	{
		zoom_factor_y = (zoom_factor_y / 2) * 2;
		zoom_height = ((((zoom_height / zoom_factor_y) / 2) * 2) + 1) 
			* zoom_factor_y;
	}

/* create image */
	zoom_map = XCreateImage(display_id,visual_id,
		     image_depth, image_format, image_offset,
		     &zoom_area_pointer[0], 
		     (unsigned int)zoom_width, (unsigned int)zoom_height,
		     image_bitmap_pad, image_bytes_per_line);

/* create pixmap */
	zoom_pixmap = XCreatePixmap(display_id,zoom_area_window_id,
		(unsigned int)zoom_width, (unsigned int)zoom_height, 
		image_depth);

/* test resources are sufficient by synchronizing calls to server
	and calling up error trap if create pixmap fails */
	XSync(display_id, False);
	if (icallback_value == 999) return;

/* copy pixmap into image */
	XPutImage(display_id, zoom_pixmap,
		  gc_fill_rectangle,zoom_map,image_source_x, image_source_y,
		  image_source_x, image_source_y, 1,1);

	zoom_flag = 1;
	zoomhide_();
}
/*********************************************************************/
/*	Hide zoom area */
/*********************************************************************/
void	zoomhide_ ()
{
/* Hide the zoom window if present */
	if(zoom_flag == 1)
	{
		zoom_map->data = 0;
		XDestroyImage(zoom_map);	
		XFreePixmap(display_id, zoom_pixmap);
		XtUnmanageChild(zoom_area);
		XSync(display_id, False);
	}
	zoom_flag = 0;
}
/*********************************************************************/
/* Convert zoom coords to image */
/*********************************************************************/
void convert_to_image (int x_zoom, int y_zoom, 
		       int *x_image, int *y_image)
/* to zoom an area, a pixel position is chosen to become the 'centre'.
   It lies one pixel to the right and down from the true box centre
   which must be between pixels as only even zoom factors are allowed. */
{
	int	shift;

/* calculate shift according to whether coord lies in right or 
   left side of box. Note pixel coords start at 0,0 */

	if(x_zoom > zoom_width / 2) 
		shift = zoom_factor_x / 2;
	else
		shift = -zoom_factor_x / 2 + 1;

	*x_image = zoom_pointer_x
		- (zoom_width/2 - x_zoom - shift) / zoom_factor_x;

/* calculate shift according to whether coord lies in top or 
	bottom part of box */

	if(y_zoom > zoom_height / 2) 
		shift = zoom_factor_y / 2;
	else
		shift = -zoom_factor_y / 2 + 1;

	*y_image = zoom_pointer_y
		- (zoom_height/2 - y_zoom - shift) / zoom_factor_y;
}
/*********************************************************************/
/* Convert image coords to zoom */
/*********************************************************************/
void convert_to_zoom (int x_image, int y_image,
		      int *x_zoom, int *y_zoom) 
{
	*x_zoom = zoom_width / 2 
		+ zoom_factor_x * (x_image - zoom_pointer_x + pan_x);

	*y_zoom = zoom_height / 2 
		+ zoom_factor_y * (y_image - zoom_pointer_y + pan_y);
}
/*********************************************************************/
/* Handle iobox exposure */
/*********************************************************************/
void 	expose_iobox ()
{
	expose_iobox_flag = 1;
}
/*********************************************************************/
/* Handle label exposure */
/*********************************************************************/
void 	expose_label ()
{
	expose_label_flag = 1;
}
/*********************************************************************/
/* Handle menu exposure */
/*********************************************************************/
void 	expose_menu ()
{
	expose_menu_flag = 1;
}
/*********************************************************************/
/* Handle slider exposure */
/*********************************************************************/
void 	expose_slider ()
{
	expose_slider_flag = 1;
}
/*********************************************************************/
/* Draw giant crosshair cursor */
/*********************************************************************/
void	giant_cursor_draw ()
{

	crosshair_x = image_pointer_x + pan_x;
	crosshair_y = image_pointer_y + pan_y;

	XDrawLine
	(display_id, image_area_window_id, gc_draw_vector[dash_style],
	 0, crosshair_y, crosshair_x-5, crosshair_y);
	XDrawLine
	(display_id, image_area_window_id, gc_draw_vector[dash_style],
	 crosshair_x+5, crosshair_y, window_width, crosshair_y);

	XDrawLine
	(display_id, image_area_window_id, gc_draw_vector[dash_style],
	crosshair_x, 0, crosshair_x, crosshair_y-5);
	XDrawLine
	(display_id, image_area_window_id, gc_draw_vector[dash_style],
	crosshair_x, crosshair_y+5, crosshair_x, window_height);

/* reset crosshair coords without pan for screen pixmap */
	crosshair_x = image_pointer_x;
	crosshair_y = image_pointer_y;

	XDrawLine
	(display_id, screen_pixmap[map_no], gc_draw_vector[dash_style],
	 0, crosshair_y, crosshair_x-5, crosshair_y);
	XDrawLine
	(display_id, screen_pixmap[map_no], gc_draw_vector[dash_style],
	 crosshair_x+5, crosshair_y, window_width, crosshair_y);

	XDrawLine
	(display_id, screen_pixmap[map_no], gc_draw_vector[dash_style],
	crosshair_x, 0, crosshair_x, crosshair_y-5);
	XDrawLine
	(display_id, screen_pixmap[map_no], gc_draw_vector[dash_style],
	crosshair_x, crosshair_y+5, crosshair_x, window_height);

/* draw new crosshair in zoom window */
	if(zoom_flag == 1)
	{
		convert_to_zoom
		(crosshair_x, crosshair_y, &zoom_x1, &zoom_y1);

		XDrawLine
		(display_id, zoom_area_window_id, gc_draw_vector[dash_style],
		 0, zoom_y1, zoom_x1-5, zoom_y1);
		XDrawLine
		(display_id, zoom_area_window_id, gc_draw_vector[dash_style],
		 zoom_x1+5, zoom_y1, zoom_width, zoom_y1);

		XDrawLine
		(display_id, zoom_area_window_id, gc_draw_vector[dash_style],
		 zoom_x1 , 0, zoom_x1, zoom_y1-5);
		XDrawLine
		(display_id, zoom_area_window_id, gc_draw_vector[dash_style],
		 zoom_x1, zoom_y1+5, zoom_x1, zoom_height);

		XDrawLine
		(display_id, zoom_pixmap, gc_draw_vector[dash_style],
		 0, zoom_y1, zoom_x1-5, zoom_y1);
		XDrawLine
		(display_id, zoom_pixmap, gc_draw_vector[dash_style],
		 zoom_x1+5, zoom_y1, zoom_width, zoom_y1);

		XDrawLine
		(display_id, zoom_pixmap, gc_draw_vector[dash_style],
		 zoom_x1 , 0, zoom_x1, zoom_y1-5);
		XDrawLine
		(display_id, zoom_pixmap, gc_draw_vector[dash_style],
		 zoom_x1, zoom_y1+5, zoom_x1, zoom_height);
			 
	}
}
/*********************************************************************/
/* Remove giant crosshair cursor */
/*********************************************************************/
void	giant_cursor_remove ()
{
	crosshair_x = crosshair_x + pan_x;
	crosshair_y = crosshair_y + pan_y;

	XDrawLine
	(display_id, image_area_window_id, gc_remove_vector,
	 0, crosshair_y, crosshair_x-5, crosshair_y);
	XDrawLine
	(display_id, image_area_window_id, gc_remove_vector,
	 crosshair_x+5, crosshair_y, window_width, crosshair_y);

	XDrawLine
	(display_id, image_area_window_id, gc_remove_vector,
	crosshair_x, 0, crosshair_x, crosshair_y-5);
	XDrawLine
	(display_id, image_area_window_id, gc_remove_vector,
	crosshair_x, crosshair_y+5, crosshair_x, window_height);

/* reset crosshair coords without pan for screen pixmap */
	crosshair_x = crosshair_x - pan_x;
	crosshair_y = crosshair_y - pan_y;

	XDrawLine
	(display_id, screen_pixmap[map_no], gc_remove_vector,
	 0, crosshair_y, crosshair_x-5, crosshair_y);
	XDrawLine
	(display_id, screen_pixmap[map_no], gc_remove_vector,
	 crosshair_x+5, crosshair_y, window_width, crosshair_y);

	XDrawLine
	(display_id, screen_pixmap[map_no], gc_remove_vector,
	crosshair_x, 0, crosshair_x, crosshair_y-5);
	XDrawLine
	(display_id, screen_pixmap[map_no], gc_remove_vector,
	crosshair_x, crosshair_y+5, crosshair_x, window_height);

/* repeat operation in zoom window */
	if (zoom_flag == 1) 
	{
		convert_to_zoom
		(crosshair_x, crosshair_y, &zoom_x1, &zoom_y1);

		XDrawLine
		(display_id, zoom_area_window_id, gc_remove_vector,
		 0, zoom_y1, zoom_x1-5, zoom_y1);
		XDrawLine
		(display_id, zoom_area_window_id, gc_remove_vector,
		 zoom_x1+5, zoom_y1, zoom_width, zoom_y1);

		XDrawLine
		(display_id, zoom_area_window_id, gc_remove_vector,
		 zoom_x1, 0, zoom_x1, zoom_y1-5);
		XDrawLine
		(display_id, zoom_area_window_id, gc_remove_vector,
		 zoom_x1, zoom_y1+5, zoom_x1, zoom_height);

		XDrawLine
		(display_id, zoom_pixmap, gc_remove_vector,
		 0, zoom_y1, zoom_x1-5, zoom_y1);
		XDrawLine
		(display_id, zoom_pixmap, gc_remove_vector,
		 zoom_x1+5, zoom_y1, zoom_width, zoom_y1);

		XDrawLine
		(display_id, zoom_pixmap, gc_remove_vector,
		 zoom_x1 , 0, zoom_x1, zoom_y1-5);
		XDrawLine
		(display_id, zoom_pixmap, gc_remove_vector,
		 zoom_x1, zoom_y1+5, zoom_x1, zoom_height);
	}


/* redraw vector list if crosshair moved by more than 5 pixels */
	if(abs(crosshair_x - pointer_save_x) > pointer_shift_max ||
	   abs(crosshair_y - pointer_save_y) > pointer_shift_max)
		{
		ReDraw_vectors ();
		pointer_save_x = crosshair_x;
		pointer_save_y = crosshair_y;
		}
}
/*********************************************************************/
/* Draw pointer position to screen */
/*********************************************************************/
void 	pointer_track ()
{
/* note : pointer_flag = 0 - no pointer tracking
		       = 1 - reflects coords top rh corner
		       = 2 - draws giant crosshair cursor
		       = 3 - reflects coords and draws cursor
*/
	int pointer_coord_x;
	int pointer_coord_y;

	if(pointer_flag == 0) return;

/* remove existing coordinates from top rh screen */
	if(pointer_flag == 1 || pointer_flag == 3)
	{
		XDrawString(display_id,image_area_window_id,
			gc_remove_vector, pointer_x, pointer_y,
			pointer_text, pointer_text_length);
	}


/* read pointer position */
	readpointer_ (&image_pointer_x, &image_pointer_y);

	
/* apply x shift to reflect actual map coordinates */
	if(pointer_flag == 1 || pointer_flag == 3)
	{
		pointer_coord_x = image_pointer_x + pointer_shift_x;

/* apply y shift and y origin flip if specified */
		if(ichange_origin != 0) 
			pointer_coord_y = ichange_origin - image_pointer_y
					+ pointer_shift_y;
		else
			pointer_coord_y = image_pointer_y + pointer_shift_y;
/* convert coords to character string */
		sprintf(pointer_text,"%d %d",
				pointer_coord_x,pointer_coord_y);
		pointer_text_length = strlen(pointer_text);

/* draw to screen */
		XDrawString(display_id,image_area_window_id,
			gc_draw_vector[0], pointer_x, pointer_y,
			pointer_text, pointer_text_length);
	}

/* remove old crosshair  and draw new one */
	if(pointer_flag > 1) 
		{
		giant_cursor_remove ();
		giant_cursor_draw ();
		}
}
/*********************************************************************/
/* Enable pointer tracking */
/*********************************************************************/
void	pointertrackon_
		(int *point_shift_x, int *point_shift_y, 
		 int *iturn_origin)
{
	XtAddEventHandler(image_area, PointerMotionMask, False,
			  (XtEventHandler)pointer_track,0);
	XtAddEventHandler(zoom_area, PointerMotionMask, False,
			  (XtEventHandler)pointer_track,0);
	pointer_flag = pointer_flag + 1;
	ichange_origin = *iturn_origin;
	pointer_shift_x = *point_shift_x;
	pointer_shift_y = *point_shift_y;
	pointer_save_x = *point_shift_x;
	pointer_save_y = *point_shift_y;
}
/*********************************************************************/
/* Disable pointer tracking */
/*********************************************************************/
void 	pointertrackoff_ ()
{
	if(pointer_flag == 1 || pointer_flag == 3)
	{
/* remove existing coordinates from screen */
		XDrawString(display_id,image_area_window_id,
			gc_remove_vector, pointer_x, pointer_y,
			pointer_text, pointer_text_length);
		pointer_flag = pointer_flag - 1;
	}
/* remove event handler */
	if(pointer_flag == 0)
	{
		XtRemoveEventHandler(image_area, PointerMotionMask, 
				False, (XtEventHandler)pointer_track,0);
		XtRemoveEventHandler(zoom_area, PointerMotionMask, 
				False, (XtEventHandler)pointer_track,0);
	}
}
/*********************************************************************/
/* Redraw main image */
/*********************************************************************/
void 	ReDraw_image ()
{
        drawpixmap_ (&map_no);
}
/*********************************************************************/
/* Redraw overlay area */
/*********************************************************************/
void 	ReDraw_overlay ()
{

/* copy the overlay map into the overlay pixmap */

	if(overlay_flag == 1)
	  {
	XCopyArea(display_id, overlay_pixmap, overlay_area_window_id,
		  gc_fill_rectangle, 0, 0, 
	          (unsigned int)overlay_width, 
		  (unsigned int)overlay_height, 0, 0);

		XSync(display_id, False);
	}
}
/*********************************************************************/
/* Redraw vectors */
/*********************************************************************/
void 	ReDraw_vectors ()
/*********************************************************************/
{
	short		line_x[16384];
	short		line_y[16384];
	int		mlines;
	int		nvectors;
	int		ix;
        int		iy;
	float		rad;
	float		radx;
	float		rady;
        float		rang;
	int		rline_x1[16384];
	int		rline_y1[16384];
	int		rline_x2[16384];
	int		rline_y2[16384];
	int		ixtemp;
	int		iytemp;

/* Draw over all boxes in list */
	nvectors = nboxes;
	nboxes = 0;
	for ( n = 0; n < nvectors; n++)
	{
	ix = box_x[n];
	iy = box_y[n];
	ixtemp = ix + abs(box_width[n]);
	iytemp = iy + abs(box_height[n]);
	drawbox_ (&ix, &iy, &ixtemp, &iytemp);
	}

/* Draw over all circles in list */
	nvectors = ncircles;
	ncircles = 0;
	for ( n = 0; n < nvectors; n++)
	{
	rad = (float)circle_diam[n] * 0.5;
	ix = circle_x[n] + (int)rad;
	iy = circle_y[n] + (int)rad;
	drawcircle_(&ix,&iy,&rad);
	}

/* Draw over all elipses in list */
        nvectors = nelipses;
        nelipses = 0;
        for ( n = 0; n < nvectors; n++)
        {
        radx = (float)elipse_diamx[n] * 0.5;
        rady = (float)elipse_diamy[n] * 0.5;
        rang = (float)elipse_ang1[n];
        ix = elipse_x[n] + (int)radx;
        iy = elipse_y[n] + (int)rady;
        drawelipse_(&ix,&iy,&radx,&rady,&rang);
        }

/* Draw over all lines in list */
	for (n = 0; n < nlines; n++)
		{
		rline_x1[n] = line_x1[n];
		rline_y1[n] = line_y1[n];
		rline_x2[n] = line_x2[n];
		rline_y2[n] = line_y2[n];
		}
	mlines = nlines;
	nlines = 0;
	drawlines_ (rline_x1, rline_y1, rline_x2, rline_y2, &mlines);

/* Draw over all points in list */
	nvectors = npoints;
	npoints = 0;
	for ( n = 0; n < nvectors; n++)
	drawpoint_ (&point_x[n], &point_y[n]);

/* Draw over all text in list */
	nvectors = ntext;
	ntext = 0;
	for ( n = 0; n < nvectors; n++)
	{
	ixtemp = text_x[n];
	iytemp = text_y[n];
	drawtext_ (&ixtemp, &iytemp, 
		      &text_string[n][0], &text_lengths[n]);
	}

/* copy pixmap to image area */
	if(image_flag == 1)
	  drawpixmap_ (&map_no);

/* clear zoom window */
	if(zoom_flag == 1)
	{
	XClearWindow(display_id, zoom_area_window_id);
	XCopyArea(display_id, zoom_pixmap, zoom_area_window_id,
		  gc_fill_rectangle, 0, 0, 
                  (unsigned int)zoom_width, (unsigned int)zoom_height, 0, 0);
	}
}
/*********************************************************************/
/* Redraw zoom area */
/*********************************************************************/
void 	ReDraw_zoom ()
{

/* copy the zoom map into the zoom pixmap */

	if(zoom_flag == 1)
	  {
	XCopyArea(display_id, zoom_pixmap, zoom_area_window_id,
		  gc_fill_rectangle, 0, 0, 
	          (unsigned int)zoom_width, (unsigned int)zoom_height, 0, 0);

		XSync(display_id, False);
	}
}
/*********************************************************************/
/* Read Pointer position */
/*********************************************************************/
void 	readpointer_ (int *image_pointer_x, int *image_pointer_y)
{
	Window		child_id;
	int		window_x;
	int		window_y;
	unsigned int 	keys_buttons;

/* read pointer coords wrt top left corner of window(not image) */
	XQueryPointer
	(display_id,
	 image_area_window_id,
	 &root_id,
	 &child_id,
	 &root_x,
	 &root_y,
	 &window_x,
	 &window_y,
	 &keys_buttons);
/* check to see if coordinates in zoom area, if so translate coords */
	if(zoom_flag == 1 &&
	   window_x >= zoom_window_x1 && window_x <= zoom_window_x2 &&
	   window_y >= zoom_window_y1 && window_y <= zoom_window_y2)

/* convert zoom to image coordinates */
		{
	XQueryPointer
	(display_id,
	 zoom_area_window_id,
	 &root_id,
	 &child_id,
	 &root_x,
	 &root_y,
	 &window_x,
	 &window_y,
	 &keys_buttons);
		convert_to_image(window_x, 
				 window_y,
				 &zoom_x1, &zoom_y1);
	 	*image_pointer_x = zoom_x1 - pan_x;	 
	 	*image_pointer_y = zoom_y1 - pan_y;	 
		}
	else
/* return coordinates origin top left image, not window */
		{
		 *image_pointer_x = window_x - pan_x;	 
		 *image_pointer_y = window_y - pan_y;	 
		}
}
/*********************************************************************/
/* start read rubber band coord */
/*********************************************************************/
void 	rubberenable_ (int *rflag)
{
/* rubber_type = 1 for lines, 2 for box, 3 for circle */
	rubber_flag = 1;
	rubber_type = *rflag;
}
/*********************************************************************/
/* Read rubber band coord */
/*********************************************************************/
void 	rubberread_ (int *ix1, int *iy1, int *ix2, int *iy2)
{
	*ix1 = rubber_start_x;
	*iy1 = rubber_start_y;
	*ix2 = rubber_finish_x;
	*iy2 = rubber_finish_y;
}
/*********************************************************************/
/* Draw rubber band box */
/*********************************************************************/
void 	rubberdrawbox ()
{
	rubber_min_x = imin_ (rubber_start_x,rubber_finish_x);
	rubber_min_y = imin_ (rubber_start_y,rubber_finish_y);
	rubber_max_x = imax_ (rubber_start_x,rubber_finish_x);
	rubber_max_y = imax_ (rubber_start_y,rubber_finish_y);
	rubber_size_x = rubber_max_x - rubber_min_x ;
	rubber_size_y = rubber_max_y - rubber_min_y ;

/* remove old box from image area and screen pixmap */
	XDrawRectangle
	(display_id, image_area_window_id, gc_remove_vector,
	rubber_min_x + pan_x, rubber_min_y + pan_y, 
		rubber_size_x, rubber_size_y);
	XDrawRectangle
	(display_id, screen_pixmap[map_no], gc_remove_vector,
	rubber_min_x, rubber_min_y, rubber_size_x, rubber_size_y);


	rubber_min_x = imin_ (rubber_start_x,image_pointer_x);
	rubber_min_y = imin_ (rubber_start_y,image_pointer_y);
	rubber_max_x = imax_ (rubber_start_x,image_pointer_x);
	rubber_max_y = imax_ (rubber_start_y,image_pointer_y);
	rubber_size_x = rubber_max_x - rubber_min_x ;
	rubber_size_y = rubber_max_y - rubber_min_y ;

/* draw new box to image area and screen pixmap */
	XDrawRectangle
	(display_id, image_area_window_id, gc_draw_vector[dash_style],
	rubber_min_x + pan_x, rubber_min_y + pan_y, 
		rubber_size_x, rubber_size_y);
	XDrawRectangle
	(display_id, screen_pixmap[map_no], gc_draw_vector[dash_style],
	rubber_min_x, rubber_min_y, rubber_size_x, rubber_size_y);

/* repeat both operations in zoom window */
	if (zoom_flag == 1) 
	{
		convert_to_zoom
		(rubber_start_x,rubber_start_y, &zoom_x1, &zoom_y1);
		convert_to_zoom	
		(rubber_finish_x, rubber_finish_y, &zoom_x2, &zoom_y2);
		rubber_min_x = imin_ (zoom_x1, zoom_x2);	
		rubber_min_y = imin_ (zoom_y1, zoom_y2);
		rubber_max_x = imax_ (zoom_x1, zoom_x2);	
		rubber_max_y = imax_ (zoom_y1, zoom_y2);
		rubber_size_x = rubber_max_x - rubber_min_x ;
		rubber_size_y = rubber_max_y - rubber_min_y ;

/* remove old zoom box */
		XDrawRectangle
		(display_id, zoom_area_window_id, gc_remove_vector,
		rubber_min_x, rubber_min_y, rubber_size_x, rubber_size_y);

		XDrawRectangle
		(display_id, zoom_pixmap, gc_remove_vector,
		rubber_min_x, rubber_min_y, rubber_size_x, rubber_size_y);

		convert_to_zoom
		(image_pointer_x,image_pointer_y, &zoom_x2, &zoom_y2);
		rubber_min_x = imin_ (zoom_x1, zoom_x2);	
		rubber_min_y = imin_ (zoom_y1, zoom_y2);
		rubber_max_x = imax_ (zoom_x1, zoom_x2);	
		rubber_max_y = imax_ (zoom_y1, zoom_y2);
		rubber_size_x = rubber_max_x - rubber_min_x ;
		rubber_size_y = rubber_max_y - rubber_min_y ;

/* draw new zoom box */
		XDrawRectangle
		(display_id, zoom_area_window_id, gc_draw_vector[dash_style],
		rubber_min_x, rubber_min_y, rubber_size_x, rubber_size_y);

		XDrawRectangle
		(display_id, zoom_pixmap, gc_draw_vector[dash_style],
		rubber_min_x, rubber_min_y, rubber_size_x, rubber_size_y);
	}
}
/*********************************************************************/
/* Draw rubber band circle */
/*********************************************************************/
void 	rubberdrawcircle ()
{
/* remove old circle from image area and screen pixmap */
/* draw new circle to image area and screen pixmap */
/* repeat both operations in zoom area */

	rubber_size_x = rubber_finish_x - rubber_start_x;
	rubber_size_y = rubber_finish_y - rubber_start_y;
	rubber_size_x = rubber_size_x * rubber_size_x;
	rubber_size_y = rubber_size_y * rubber_size_y;
	rubber_circle_diam = 
		(int)(sqrt((double)(rubber_size_x + rubber_size_y)) * 2.0);
	rubber_circle_x = rubber_start_x - (float)rubber_circle_diam * 0.5;
	rubber_circle_y = rubber_start_y - (float)rubber_circle_diam * 0.5;

/* remove old circle in image area */
	XDrawArc
	(display_id, image_area_window_id, gc_remove_vector,
	rubber_circle_x + pan_x, rubber_circle_y + pan_y,
	rubber_circle_diam, rubber_circle_diam,
	circle_angle, circle_angle);

/* remove old circle into pixmap */
	XDrawArc
	(display_id, screen_pixmap[map_no], gc_remove_vector,
	rubber_circle_x + pan_x, rubber_circle_y + pan_y,
	rubber_circle_diam, rubber_circle_diam,
	circle_angle, circle_angle);

	rubber_size_x = image_pointer_x - rubber_start_x;
	rubber_size_y = image_pointer_y - rubber_start_y;
	rubber_size_x = rubber_size_x * rubber_size_x;
	rubber_size_y = rubber_size_y * rubber_size_y;
	rubber_circle_diam = 
		(int)(sqrt((double)(rubber_size_x + rubber_size_y)) * 2.0);
	rubber_circle_x = rubber_start_x - (float)rubber_circle_diam * 0.5;
	rubber_circle_y = rubber_start_y - (float)rubber_circle_diam * 0.5;

/* draw circle in image area */
	XDrawArc
	(display_id, image_area_window_id, gc_draw_vector[dash_style],
	rubber_circle_x + pan_x, rubber_circle_y + pan_y,
	rubber_circle_diam, rubber_circle_diam,
	circle_angle, circle_angle);

/* draw circle into pixmap */
	XDrawArc
	(display_id, screen_pixmap[map_no], gc_draw_vector[dash_style],
	rubber_circle_x + pan_x, rubber_circle_y + pan_y,
	rubber_circle_diam, rubber_circle_diam,
	circle_angle, circle_angle);

/* remove and draw circle in zoom window if present */
	if(zoom_flag == 1)
	{
		convert_to_zoom
		(rubber_start_x,rubber_start_y, &zoom_x1, &zoom_y1);
		convert_to_zoom	
		(rubber_finish_x, rubber_finish_y, &zoom_x2, &zoom_y2);

		rubber_size_x = zoom_x2 - zoom_x1;
		rubber_size_y = zoom_y2 - zoom_y1;
		rubber_size_x = rubber_size_x * rubber_size_x;
		rubber_size_y = rubber_size_y * rubber_size_y;
		rubber_circle_diam = 
		(int)(sqrt((double)(rubber_size_x + rubber_size_y)) * 2.0);
		rubber_circle_x = zoom_x1 - (float)rubber_circle_diam * 0.5;
		rubber_circle_y = zoom_y1 - (float)rubber_circle_diam * 0.5;

/* remove zoomed circle into zoom window */
		XDrawArc
		(display_id, zoom_area_window_id, gc_remove_vector,
		rubber_circle_x, rubber_circle_y,
		rubber_circle_diam, rubber_circle_diam,
		circle_angle, circle_angle);

/* remove zoomed circle into zoom pixmap */
		XDrawArc
		(display_id, zoom_pixmap, gc_remove_vector,
		rubber_circle_x, rubber_circle_y,
		rubber_circle_diam, rubber_circle_diam,
		circle_angle, circle_angle);

		convert_to_zoom	
		(image_pointer_x, image_pointer_y, &zoom_x2, &zoom_y2);
		rubber_size_x = zoom_x2 - zoom_x1;
		rubber_size_y = zoom_y2 - zoom_y1;
		rubber_size_x = rubber_size_x * rubber_size_x;
		rubber_size_y = rubber_size_y * rubber_size_y;
		rubber_circle_diam = 
		(int)(sqrt((double)(rubber_size_x + rubber_size_y)) * 2.0);
		rubber_circle_x = zoom_x1 - (float)rubber_circle_diam * 0.5;
		rubber_circle_y = zoom_y1 - (float)rubber_circle_diam * 0.5;

/* draw zoomed circle into zoom window */
		XDrawArc
		(display_id, zoom_area_window_id, gc_draw_vector[dash_style],
		rubber_circle_x, rubber_circle_y,
		rubber_circle_diam, rubber_circle_diam,
		circle_angle, circle_angle);

/* draw zoomed circle into zoom pixmap */
		XDrawArc
		(display_id, zoom_pixmap, gc_draw_vector[dash_style],
		rubber_circle_x, rubber_circle_y,
		rubber_circle_diam, rubber_circle_diam,
		circle_angle, circle_angle);
	}

	XSync(display_id, False);

}
/*********************************************************************/
/* Draw rubber band elipse */
/*********************************************************************/
void 	rubberdrawelipse ()
{
/* remove old elipse from image area and screen pixmap */
/* draw new elipse to image area and screen pixmap */
/* repeat both operations in zoom area */

	rubber_size_x = rubber_finish_x - rubber_start_x;
	rubber_size_y = rubber_finish_y - rubber_start_y;
	rubber_size_x = rubber_size_x * rubber_size_x;
	rubber_size_y = rubber_size_y * rubber_size_y;
	rubber_elipse_diamx = 
		(int)(sqrt((double)(rubber_size_x + rubber_size_y)) * 2.0);
	rubber_elipse_diamy = 
		(int)(sqrt((double)(rubber_size_x + rubber_size_y)) * 2.0);
        rubber_elipse_angle = 0.0*64;
	rubber_elipse_x = rubber_start_x - (float)rubber_elipse_diamx * 0.5;
	rubber_elipse_y = rubber_start_y - (float)rubber_elipse_diamy * 0.5;

/* remove old elipse in image area */
	XDrawArc
	(display_id, image_area_window_id, gc_remove_vector,
	rubber_elipse_x + pan_x, rubber_elipse_y + pan_y,
	rubber_elipse_diamx, rubber_elipse_diamy,
	rubber_elipse_angle, circle_angle);

/* remove old elipse into pixmap */
	XDrawArc
	(display_id, screen_pixmap[map_no], gc_remove_vector,
	rubber_elipse_x + pan_x, rubber_elipse_y + pan_y,
	rubber_elipse_diamx, rubber_elipse_diamy,
	rubber_elipse_angle, circle_angle);

	rubber_size_x = image_pointer_x - rubber_start_x;
	rubber_size_y = image_pointer_y - rubber_start_y;
	rubber_size_x = rubber_size_x * rubber_size_x;
	rubber_size_y = rubber_size_y * rubber_size_y;
	rubber_elipse_diamx = 
		(int)(sqrt((double)(rubber_size_x + rubber_size_y)) * 2.0);
	rubber_elipse_diamy = 
		(int)(sqrt((double)(rubber_size_x + rubber_size_y)) * 2.0);
	rubber_elipse_x = rubber_start_x - (float)rubber_elipse_diamx * 0.5;
	rubber_elipse_y = rubber_start_y - (float)rubber_elipse_diamy * 0.5;

/* draw elipse in image area */
	XDrawArc
	(display_id, image_area_window_id, gc_draw_vector[dash_style],
	rubber_elipse_x + pan_x, rubber_elipse_y + pan_y,
	rubber_elipse_diamx, rubber_elipse_diamy,
	rubber_elipse_angle, circle_angle);

/* draw elipse into pixmap */
	XDrawArc
	(display_id, screen_pixmap[map_no], gc_draw_vector[dash_style],
	rubber_elipse_x + pan_x, rubber_elipse_y + pan_y,
	rubber_elipse_diamx, rubber_elipse_diamy,
	rubber_elipse_angle, circle_angle);

/* remove and draw elipse in zoom window if present */
	if(zoom_flag == 1)
	{
		convert_to_zoom
		(rubber_start_x,rubber_start_y, &zoom_x1, &zoom_y1);
		convert_to_zoom	
		(rubber_finish_x, rubber_finish_y, &zoom_x2, &zoom_y2);

		rubber_size_x = zoom_x2 - zoom_x1;
		rubber_size_y = zoom_y2 - zoom_y1;
		rubber_size_x = rubber_size_x * rubber_size_x;
		rubber_size_y = rubber_size_y * rubber_size_y;
		rubber_elipse_diamx = 
		(int)(sqrt((double)(rubber_size_x + rubber_size_y)) * 2.0);
		rubber_elipse_diamy = 
		(int)(sqrt((double)(rubber_size_x + rubber_size_y)) * 2.0);
		rubber_elipse_x = zoom_x1 - (float)rubber_elipse_diamx * 0.5;
		rubber_elipse_y = zoom_y1 - (float)rubber_elipse_diamy * 0.5;

/* remove zoomed elipse into zoom window */
		XDrawArc
		(display_id, zoom_area_window_id, gc_remove_vector,
		rubber_elipse_x, rubber_elipse_y,
		rubber_elipse_diamx, rubber_elipse_diamy,
		rubber_elipse_angle, circle_angle);

/* remove zoomed elipse into zoom pixmap */
		XDrawArc
		(display_id, zoom_pixmap, gc_remove_vector,
		rubber_elipse_x, rubber_elipse_y,
		rubber_elipse_diamx, rubber_elipse_diamy,
		rubber_elipse_angle, circle_angle);

		convert_to_zoom	
		(image_pointer_x, image_pointer_y, &zoom_x2, &zoom_y2);
		rubber_size_x = zoom_x2 - zoom_x1;
		rubber_size_y = zoom_y2 - zoom_y1;
		rubber_size_x = rubber_size_x * rubber_size_x;
		rubber_size_y = rubber_size_y * rubber_size_y;
		rubber_elipse_diamx = 
		(int)(sqrt((double)(rubber_size_x + rubber_size_y)) * 2.0);
		rubber_elipse_diamy = 
		(int)(sqrt((double)(rubber_size_x + rubber_size_y)) * 2.0);
		rubber_elipse_x = zoom_x1 - (float)rubber_elipse_diamx * 0.5;
		rubber_elipse_y = zoom_y1 - (float)rubber_elipse_diamy * 0.5;

/* draw zoomed elipse into zoom window */
		XDrawArc
		(display_id, zoom_area_window_id, gc_draw_vector[dash_style],
		rubber_elipse_x, rubber_elipse_y,
		rubber_elipse_diamx, rubber_elipse_diamy,
		rubber_elipse_angle, circle_angle);

/* draw zoomed elipse into zoom pixmap */
		XDrawArc
		(display_id, zoom_pixmap, gc_draw_vector[dash_style],
		rubber_elipse_x, rubber_elipse_y,
		rubber_elipse_diamx, rubber_elipse_diamy,
		rubber_elipse_angle, circle_angle);
	}

	XSync(display_id, False);

}
/*********************************************************************/
/* Draw rubber band line */
/*********************************************************************/
void 	rubberdrawline ()
{
/* remove old line */
	XDrawLine
	(display_id, image_area_window_id, gc_remove_vector,
	rubber_start_x + pan_x, rubber_start_y + pan_y,
	rubber_finish_x + pan_x, rubber_finish_y + pan_y);

	XDrawLine
	(display_id, screen_pixmap[map_no], gc_remove_vector,
	rubber_start_x, rubber_start_y,	rubber_finish_x, rubber_finish_y);

/* draw new line */
	XDrawLine
	(display_id, image_area_window_id, gc_draw_vector[dash_style],
	rubber_start_x + pan_x, rubber_start_y + pan_y,	
	image_pointer_x + pan_x, image_pointer_y + pan_y);

	XDrawLine
	(display_id, screen_pixmap[map_no], gc_draw_vector[dash_style],
	rubber_start_x,rubber_start_y, image_pointer_x, image_pointer_y);

/* repeat operation in zoom window */
	if (zoom_flag == 1) 
	{
		convert_to_zoom
		(rubber_start_x,rubber_start_y, &zoom_x1, &zoom_y1);
		convert_to_zoom	
		(rubber_finish_x,rubber_finish_y, &zoom_x2, &zoom_y2);

/* remove old zoom line */
		XDrawLine
		(display_id, zoom_area_window_id, gc_remove_vector,
		 zoom_x1, zoom_y1, zoom_x2, zoom_y2);

		XDrawLine
		(display_id, zoom_pixmap, gc_remove_vector,
		 zoom_x1, zoom_y1, zoom_x2, zoom_y2);
/* draw new zoom line */
		convert_to_zoom
		(image_pointer_x,image_pointer_y, &zoom_x2, &zoom_y2);
		XDrawLine
		(display_id, zoom_area_window_id, gc_draw_vector[dash_style],
		 zoom_x1, zoom_y1, zoom_x2, zoom_y2);

		XDrawLine
		(display_id, zoom_pixmap, gc_draw_vector[dash_style],
		 zoom_x1, zoom_y1, zoom_x2, zoom_y2);
	}
}
/*********************************************************************/
/* Remove rubber band line */
/*********************************************************************/
void 	rubberremoveline_ (int *ix1, int *iy1, int *ix2, int *iy2)
{
/* remove old line */
	XDrawLine
	(display_id, image_area_window_id, gc_remove_vector,
	rubber_start_x + pan_x, rubber_start_y + pan_y,
        rubber_finish_x + pan_x, rubber_finish_y + pan_y);

	XDrawLine
	(display_id, screen_pixmap[map_no], gc_remove_vector,
	rubber_start_x,rubber_start_y,rubber_finish_x,rubber_finish_y);

/* repeat operation in zoom window */
	if (zoom_flag == 1) 
	{
		convert_to_zoom
		(rubber_start_x,rubber_start_y, &zoom_x1, &zoom_y1);
		convert_to_zoom
		(rubber_finish_x,rubber_finish_y, &zoom_x2, &zoom_y2);

/* remove old zoom line */
		XDrawLine
		(display_id, zoom_area_window_id, gc_remove_vector,
		 zoom_x1, zoom_y1, zoom_x2, zoom_y2);
		XDrawLine
		(display_id, zoom_pixmap, gc_remove_vector,
		 zoom_x1, zoom_y1, zoom_x2, zoom_y2);
	}

/* move line out of arrays */
	nlines = nlines - 1;
	rubber_start_x = line_x1[nlines];
	rubber_start_y = line_y1[nlines];
	rubber_finish_x = line_x2[nlines];
	rubber_finish_y = line_y2[nlines];
}
/*********************************************************************/
/* Read slider position */
/*********************************************************************/
void 	readslider_ (float *slider_percent)
{
/* dummy routine to return slider position */
        *slider_percent = slider_position;
}
/*********************************************************************/
/*	Action routines */
/*********************************************************************/

/*********************************************************************/
/* io box callback */
/*********************************************************************/
void	action_iobox ()
{
	icallback_value = 100;
	ximagecallback_ (&icallback_value);
}
/*********************************************************************/
/* display label */
/*********************************************************************/
void	action_label_display_ ()
{
/* position at top of screen */
	XtVaSetValues( label,
		XtNfromVert, NULL,
		XtNvertDistance,4,
        	NULL);                
/* if slider present, position wrt slider */
	if(slider_flag >= 1)
		XtVaSetValues(sliderBox,
		XtNfromVert, label,
		XtNvertDistance,1,
        	NULL);                

/* if menu present, reposition it wrt slider */
	if(slider_flag >= 1 && menu_flag == 1)
	{
		XtUnmanageChild(menuBox);
		XtVaSetValues(menuBox,
		XtNfromVert, sliderBox,
		XtNvertDistance,1,
        	NULL);                

		XRaiseWindow(display_id,menuBox_window_id);
		XtManageChild(menuBox);
	}
/* reposition menu wrt label if no slider */
	else if(slider_flag == 0 && menu_flag == 1)
	{
		XtUnmanageChild(menuBox);
		XtVaSetValues(menuBox,
		XtNfromVert, label,
		XtNvertDistance,1,
        	NULL);                

		XRaiseWindow(display_id,menuBox_window_id);
		XtManageChild(menuBox);
	}

	XtManageChild(label);
	XRaiseWindow(display_id,label_window_id);
	while (expose_label_flag == 0)
	 eventdispatch_ ();

	XSync(display_id, False);
	expose_label_flag = 0;
	label_flag = 1;
}

/*********************************************************************/
/* display menu */
/*********************************************************************/
void	actionmenudisplay_ ()
{
/* if slider present, set menu position with respect to slider */
	if(slider_flag >= 1)
		{
		XtVaSetValues(menuBox,
			XtNfromVert, sliderBox,
			XtNvertDistance,1,
	        	NULL);                   /* argument list*/
		}
/* if label present, set menu position with respect to label */
	else if(label_flag == 1)
		{
		XtVaSetValues(menuBox,
			XtNfromVert, label,
			XtNvertDistance,1,
	        	NULL);                   /* argument list*/
		}
/* if no label or slider, set menu position with respect to top of window */
	else
		XtVaSetValues(menuBox,
			XtNfromVert, NULL,
			XtNvertDistance,4,
        	NULL);  
              
/* manage menu widget */
	XtManageChild(menuBox);

	if(event_proc == 0)
	{
		XtAddEventHandler
		(menuBox,ExposureMask,False,(XtEventHandler)expose_menu,0);
		while (expose_menu_flag == 0)
		 eventdispatch_ ();
	}
/*	else
	{
           event_get_next_(&event_true);
           if(event_true != 0) eventdispatch_ ();
	}*/

	XSync(display_id, False);
	expose_menu_flag = 0;
	menu_flag = 1;
}

/*Andreas*/
void setpan_(int *x, int *y)
{
	zoomhide_ ();
	pan_x = *x;
	pan_y = *y;
	ReDraw_image();
}


/*********************************************************************/
/* Start Pan function callback */
/*********************************************************************/
void 	action_pan_start ()
{
        int 	pan_xy;
/* disable panning for multi-pixmap images */
	if(no_maps > 1) return;
	zoomhide_ ();
	pan_flag = 1;
	readpointer_(&image_pointer_x, &image_pointer_y);

	pan_pointer_x1 = image_pointer_x + pan_x;
	pan_pointer_y1 = image_pointer_y + pan_y;

	while(pan_flag == 1)
	{
/* read pointer coordinates */
		readpointer_(&image_pointer_x, &image_pointer_y);
		pan_pointer_x2 = image_pointer_x + pan_x;
		pan_pointer_y2 = image_pointer_y + pan_y;

/* test to see if pointer has shifted enough to pan */
		pan_xy = abs(pan_pointer_x2 - pan_pointer_x1)
		       + abs(pan_pointer_y2 - pan_pointer_y1);

		if(pan_xy > pan_min)
		{
			pan_x = pan_x + pan_pointer_x2 - pan_pointer_x1;
			pan_y = pan_y + pan_pointer_y2 - pan_pointer_y1;
			ReDraw_image ();
			pan_pointer_x1 = pan_pointer_x2;
			pan_pointer_y1 = pan_pointer_y2;
		}

	           event_get_next_(&event_true);
	           if(event_true != 0) eventdispatch_ ();
	}
}
/*********************************************************************/
/* Finish Pan function callback */
/*********************************************************************/
void 	action_pan_finish ()
{
/* disable panning for multi-pixmap images */
	if(no_maps > 1) return;
/* read pointer coordinates */
	readpointer_(&image_pointer_x, &image_pointer_y);

	pan_pointer_x2 = image_pointer_x + pan_x;
	pan_pointer_y2 = image_pointer_y + pan_y;

/* reset pan dimensions */
	pan_x = pan_x + pan_pointer_x2 - pan_pointer_x1;
	pan_y = pan_y + pan_pointer_y2 - pan_pointer_y1;

	pan_flag = 0;
	ReDraw_image ();
}
/*********************************************************************/
/* rubber band start */
/*********************************************************************/
void 	action_rubber_read ()
{

/* if first line, read first pointer position, store it */
	if(rubber_flag == 0)
		return;
	else if(rubber_flag <= 2)
	{
		readpointer_(&image_pointer_x, &image_pointer_y);
		rubber_start_x = image_pointer_x;
		rubber_start_y = image_pointer_y;
		rubber_finish_x = image_pointer_x;
		rubber_finish_y = image_pointer_y;
	}
	else if (rubber_flag == 3)
	{
		rubber_start_x = line_x2[nlines-1];
		rubber_start_y = line_y2[nlines-1];
		rubber_finish_x = line_x2[nlines-1];
		rubber_finish_y = line_y2[nlines-1];
	}

	rubber_flag = 2;

/* start loop to continually read pointer and update line */
	while (rubber_flag == 2)
	{
		readpointer_(&image_pointer_x, &image_pointer_y);

/* move giant crosshair, update tracked coords if flag set */
		if(pointer_flag > 0) pointer_track ();

/* draw rubber shape */
		if(rubber_type == 1) 
			rubberdrawline ();
		else if(rubber_type == 2) 
			rubberdrawbox ();
		else if(rubber_type == 3) 
			rubberdrawcircle ();
		else if(rubber_type == 4) 
			rubberdrawelipse ();

		XSync(display_id, False);

/* store new endpoint for next time */
		rubber_finish_x = image_pointer_x;
		rubber_finish_y = image_pointer_y;

		event_get_next_(&event_true);
		if(event_true != 0) eventdispatch_ ();

	}
}
/*********************************************************************/
/* rubber band pause between lines */
/*********************************************************************/
void 	action_rubber_stop ()
{
/* return if flag 0 */
	if(rubber_flag == 0)
		return;

/* finish rubberbanding if box or circle */
	if(rubber_type > 1)
	{
		action_rubber_finish ();
		rubber_type = 0;
		return;
	}

/* pause between lines for line rubberbanding */
	rubber_flag = 3;

/* add vector to list */
	line_x1[nlines] = rubber_start_x;
	line_y1[nlines] = rubber_start_y;
	line_x2[nlines] = rubber_finish_x;
	line_y2[nlines] = rubber_finish_y;
	nlines++;

/* callback to fortran */
	icallback_value = 104;
	ximagecallback_ (&icallback_value);
}
/*********************************************************************/
/* rubber band end */
/*********************************************************************/
void 	action_rubber_finish ()
{
	if(rubber_flag == 0)
		return;

	readpointer_(&image_pointer_x, &image_pointer_y);

/* move giant crosshair + update tracked coords if flag set */
	if(pointer_flag > 0) pointer_track ();

/* test for single point specification */
	if(rubber_flag == 1)
	{
		rubber_start_x = image_pointer_x;
		rubber_start_y = image_pointer_y;

	}

/* draw final line, box or circle */
	if(rubber_type == 1)
		rubberdrawline  ();
	else if(rubber_type == 2)
		rubberdrawbox  ();
	else if(rubber_type == 3)
		rubberdrawcircle ();
/*	else if(rubber_type == 4)
		rubberdrawelipse ();
*/

	rubber_finish_x = image_pointer_x;
	rubber_finish_y = image_pointer_y;

/* add final vector(s) to list */
	if(rubber_type == 1)
	{
		if(rubber_flag >= 2)
		{
			line_x1[nlines] = rubber_start_x;
			line_y1[nlines] = rubber_start_y;
			line_x2[nlines] = rubber_finish_x;
			line_y2[nlines] = rubber_finish_y;
			nlines++;
		}
	}
/* box */
	else if(rubber_type == 2)
	{
		box_x[nboxes] = imin_ (rubber_start_x,rubber_finish_x);
		box_y[nboxes] = imin_ (rubber_start_y,rubber_finish_y);
		box_width[nboxes] = abs(rubber_finish_x - rubber_start_x);
		box_height[nboxes] = abs(rubber_finish_y - rubber_start_y);
		nboxes++;
	}
/* circle */
	else if(rubber_type == 3)
	{
		rubber_size_x = rubber_finish_x - rubber_start_x;
		rubber_size_y = rubber_finish_y - rubber_start_y;
		rubber_size_x = rubber_size_x * rubber_size_x;
		rubber_size_y = rubber_size_y * rubber_size_y;
		rubber_circle_diam = 
		(int)(sqrt((double)(rubber_size_x + rubber_size_y)) * 2.0);
		rubber_circle_x = rubber_start_x - 
					(float)rubber_circle_diam * 0.5;
		rubber_circle_y = rubber_start_y - 
					(float)rubber_circle_diam * 0.5;

		circle_x[ncircles] = rubber_circle_x;
		circle_y[ncircles] = rubber_circle_y;
		circle_diam[ncircles] = rubber_circle_diam;
		circle_style[ncircles] = dash_style;
		ncircles++;
	}
	
/* elipse 
        else if(rubber_type == 4)
        {
                rubber_size_x = rubber_finish_x - rubber_start_x;
                rubber_size_y = rubber_finish_y - rubber_start_y;
                rubber_size_x = rubber_size_x * rubber_size_x;
                rubber_size_y = rubber_size_y * rubber_size_y;
                rubber_elipse_diamx =
                (int)(sqrt((double)(rubber_size_x + rubber_size_y)) * 2.0);
                rubber_elipse_diamy =
                (int)(sqrt((double)(rubber_size_x + rubber_size_y)) * 2.0);
                rubber_elipse_x = rubber_start_x -
                                        (float)rubber_elipse_diamx * 0.5;
                rubber_elipse_y = rubber_start_y -
                                        (float)rubber_elipse_diamy * 0.5;

                elipse_x[nelipses] = rubber_elipse_x;
                elipse_y[nelipses] = rubber_elipse_y;
                elipse_diamx[nelipses] = rubber_elipse_diamx;
                elipse_diamy[nelipses] = rubber_elipse_diamy;
                elipse_ang1[nelipses] = rubber_elipse_angle;
                elipse_style[nelipses] = dash_style;
                nelipses++;
        }
*/

/* callback to fortran */
	icallback_value = 105;
	ximagecallback_ (&icallback_value);
	rubber_flag = 0;
}
/*********************************************************************/
/* display slider                                                    */
/*********************************************************************/
void	action_slider_display_ ()
{
	slider_flag = ksliders;
/* position wrt label if present */
	if(label_flag == 1)
		XtVaSetValues(sliderBox,
			XtNfromVert, label,
			XtNvertDistance,1,
	        	NULL);

/* position wrt iobox if present */
	else if(iobox_flag == 1)
		XtVaSetValues(sliderBox,
			XtNfromVert, ioBox,
			XtNvertDistance,1,
	        	NULL);

/* if menu present reposition it */
	if(menu_flag == 1)
		{
		XtUnmanageChild(menuBox);
		XtVaSetValues(menuBox,
			XtNfromVert, sliderBox,
			XtNvertDistance,1,
	        	NULL);                   /* argument list*/
		XtManageChild(menuBox);
		XRaiseWindow(display_id,menuBox_window_id);
		}

/* position slider1 in the centre and add callback */
	XawScrollbarSetThumb(slider1, slider1_start_position, slider_size);
	XtAddCallback(slider1, XtNjumpProc, 
		(XtCallbackProc)action_slider_callback, (XtPointer)1);

/* reset geometry for single or double slider */
	if(slider_flag == 1)
	{
		XtUnmanageChild(slider2);
		XtUnmanageChild(colour_bar2);
		XtVaSetValues(slider1,
		XtNfromHoriz, slider_label,
		XtNhorizDistance, 4,
		XtNfromVert, NULL,
		XtNvertDistance, 4,
		NULL);

		XtVaSetValues(colour_bar1,
		XtNfromHoriz, slider1,
		XtNhorizDistance, 4,
		XtNfromVert, NULL,
		XtNvertDistance, 4,
		NULL);
	}
/* position slider 2 in the centre and add callback */
	else if(slider_flag == 2)
	{
		XawScrollbarSetThumb(slider2, slider2_start_position, 
                                                        slider_size);
		XtAddCallback(slider2, XtNjumpProc, 
			(XtCallbackProc)action_slider_callback, (XtPointer)2);
/* reset slider 1*/
		XtVaSetValues(slider1,
		XtNfromHoriz, NULL,
		XtNfromVert, slider_label,
		XtNvertDistance, 4,
		NULL);
/* reset colour bar 1*/
		XtVaSetValues(colour_bar1,
		XtNfromHoriz, slider1,
		XtNhorizDistance, 4,
		XtNfromVert, slider_label,
		XtNvertDistance, 4,
		NULL);
/* reset slider 2 */
		XtVaSetValues(slider2,
		XtNfromHoriz, colour_bar1,
		XtNhorizDistance,4,
		XtNfromVert, slider_label,
		XtNvertDistance, 4,
		NULL);
/* reset colour bar 2 */
		XtVaSetValues(colour_bar2,
		XtNfromHoriz, slider2,
		XtNhorizDistance, 4,
		XtNfromVert, slider_label,
		XtNvertDistance, 4,
		NULL);
		XtManageChild(slider2);
		XtManageChild(colour_bar2);
	}

/* push slider in front */
	XtManageChild(sliderBox);
	XRaiseWindow(display_id, sliderBox_window_id);

/* synchronise */
	XSync(display_id, False);
	while (expose_slider_flag == 0)
	 eventdispatch_ ();

	XSync(display_id, False);
	expose_slider_flag = 0;
}
/*********************************************************************/
/* slider callback */
/*********************************************************************/
void	action_slider_callback (Widget scrollbar, XtPointer client_data,
			  XtPointer percent)
{
	slider_position = *(float*)percent * 100.0;
	if((int)client_data == 1)
		icallback_value = 101;
	else if((int)client_data == 2)
		icallback_value = 102;
	ximagecallback_ (&icallback_value);
}
/*********************************************************************/
/* Start Zoom function callback */
/*********************************************************************/
void 	action_zoom_init ()
{
        icallback_value = 103;
/* if scrolling pixmaps, start zoom centred */
	if(no_maps <= 1) 
	{
	readpointer_(&image_pointer_x, &image_pointer_y);
	zoom_display ();
	}
}
/*********************************************************************/
/* Read Pointer callback */
/*********************************************************************/
void 	action_read_pointer ()
{
	icallback_value = 0;

/* pass values to ximagecallback */
	ximagecallback_ (&icallback_value);
}
/*****************************************************************/
/*  set up action lists */
/*****************************************************************/
static XtActionsRec actlist[] = {
			{"action_read_pointer",(XtActionProc)
						action_read_pointer},
			{"action_label_display_",(XtActionProc)
						action_label_display_},
			{"actionmenudisplay_",(XtActionProc)
						actionmenudisplay_},
			{"action_slider_display_",(XtActionProc)
						action_slider_display_},
			{"action_pan_start",(XtActionProc)
						action_pan_start},
			{"action_pan_finish",(XtActionProc)
						action_pan_finish},
			{"action_rubber_read",(XtActionProc)
						action_rubber_read},
			{"action_rubber_stop",(XtActionProc)
						action_rubber_stop},
			{"action_rubber_finish",(XtActionProc)
						action_rubber_finish},
			{"action_zoom_init",(XtActionProc)
						action_zoom_init},
			{"action_iobox",(XtActionProc)action_iobox}
};
/************************************************************************/
/* Change cursor type                                                   */
/************************************************************************/
void    changecursor_ (int *icurs)
{
/* reset msbfirst if bit pattern incorrect */
	if(*icurs < 0)
	{
		msbfirst = abs(msbfirst - 1);
		*icurs = abs(*icurs);
	}

        XUndefineCursor(display_id,image_area_window_id);

/* reset pointer flag */
	if(pointer_flag > 1) 
	{
		giant_cursor_remove ();
		pointer_flag = pointer_flag - 2;
	}
	if(pointer_flag == 0)
	{
		XtRemoveEventHandler(image_area, PointerMotionMask, 
				False, (XtEventHandler)pointer_track,0);
		XtRemoveEventHandler(zoom_area, PointerMotionMask, 
				False, (XtEventHandler)pointer_track,0);
	}

        if(*icurs == 0)
/* default arrow cursor */
	  {
	  cursor = XCreateFontCursor(display_id,arrow);
	  XDefineCursor(display_id,image_area_window_id,cursor);
	  XDefineCursor(display_id,zoom_area_window_id,cursor);
	  return;
	  }
/* crosshair cursor built from bit shape and mask */
	else if(*icurs <= 2)
	{
/* set up cursor colour */
		cursor_foreground.pixel = colour_green;
		cursor_foreground.flags = DoRed | DoGreen | DoBlue;
		cursor_foreground.red = 0;
		cursor_foreground.green = 65535;
		cursor_foreground.blue = 0;

		cursor_background.pixel = colour_black;
		cursor_background.flags = DoRed | DoGreen | DoBlue;
		cursor_background.red = 0;
		cursor_background.green = 0;
		cursor_background.blue = 0;

		if(*icurs == 1)
/* simple crosshair */
		{
/* MSB first server */
			if(msbfirst == 1)
			{
			memcpy(cursor_shape,cursor_shape1_msb,30);
			memcpy(cursor_mask,cursor_mask1_msb,30);
			}
/* LSB first server */
			else
			{
			memcpy(cursor_shape,cursor_shape1_lsb,30);
			memcpy(cursor_mask,cursor_mask1_lsb,30);
			}
		}
		else

/* boxed crosshair */
		{
/* MSB first server */
			if(msbfirst == 1)
			{
			memcpy(cursor_shape,cursor_shape2_msb,30);
			memcpy(cursor_mask,cursor_mask2_msb,30);
			}
/* LSB first server */
			else
			{
			memcpy(cursor_shape,cursor_shape2_lsb,30);
			memcpy(cursor_mask,cursor_mask2_lsb,30);
			}
		}
	}

/* giant crosshair */
	else if (*icurs == 3)
	{

/* enable pointer tracking if not already done */
		if(pointer_flag == 0)
		{
			XtAddEventHandler(image_area, PointerMotionMask, 
			False, (XtEventHandler)pointer_track,0);
			XtAddEventHandler(zoom_area, PointerMotionMask, 
			False,(XtEventHandler)pointer_track,0);

		}

/* MSB first server */
			if(msbfirst == 1)
			{
			memcpy(cursor_shape,cursor_shape3_msb,30);
			memcpy(cursor_mask,cursor_mask3_msb,30);
			}

/* LSB first server */
			else
			{
			memcpy(cursor_shape,cursor_shape3_lsb,30);
			memcpy(cursor_mask,cursor_mask3_lsb,30);
			}

		pointer_flag = pointer_flag + 2;
	}
/* cross */
	else if (*icurs == 4)
		cursor = XCreateFontCursor(display_id,cross);
/* circle */
	else if (*icurs == 5)
		cursor = XCreateFontCursor(display_id,circle);
/* hand */
	else if (*icurs == 6)
		cursor = XCreateFontCursor(display_id,hand);
/* bird */
	else if (*icurs == 7)
		cursor = XCreateFontCursor(display_id,bird);
/* skull */
	else if (*icurs == 8)
		cursor = XCreateFontCursor(display_id,skull);

/* set pixmaps for designer cursors */
	if (*icurs < 4)
	{
		cursor_shape_pixmap = 
			XCreatePixmapFromBitmapData
			(display_id,topLevel_window_id,
			cursor_shape, cursor_size, cursor_size, 
			colour_green, colour_black, cursor_depth);

		cursor_mask_pixmap = 
			XCreatePixmapFromBitmapData
			(display_id,topLevel_window_id,
			cursor_mask, cursor_size, cursor_size, 
			colour_green, colour_black, cursor_depth);

		cursor = XCreatePixmapCursor(display_id,
			cursor_shape_pixmap, cursor_mask_pixmap,
			&cursor_foreground, &cursor_background,
			cursor_x, cursor_y);
	}

	XDefineCursor(display_id,image_area_window_id,cursor);
	XDefineCursor(display_id,zoom_area_window_id,cursor);
}
/************************************************************************/
/* Check resources can create a pixmap this size, called from fortran   */
/************************************************************************/
void	checkimage_ (int *nx, int *ny, int *nmaps)
{
	no_maps = *nmaps;
	if( no_maps >= max_maps)
	  {
		icallback_value = 999;
		ximagecallback_ (&icallback_value);
	  }

	image_width = *nx;
	image_height = *ny;
/* remove pixmap,image */
	if(image_flag != 0) XFreePixmap(display_id, screen_pixmap[map_no]);

/* Create pixmaps to hold image */
	for (i = 0; i < no_maps; i++)
	  {
	    screen_pixmap[i] = XCreatePixmap(display_id, image_area_window_id,
			image_width, image_height, image_depth);
/* check resources sufficient, else call error trap and return */
	    XSync(display_id, False);
	    if (icallback_value == 999) return;
	  }

/* free pixmaps if successful */
	for (i = 0; i < no_maps; i++)
	  {
	    XFreePixmap(display_id, screen_pixmap[i]);
	  }
	XSync(display_id, False);
}
/*********************************************************************/
/* clear image from the window */
/*********************************************************************/
void	clearimage_ ()
{
	int	i;
	if(image_flag != 0)
	{
         pan_x = 0;
         pan_y = 0;

/* fill screen pixmap with blanks */
		for (i = 0; i < no_maps; i++)
		{
		XFillRectangle(display_id, screen_pixmap[i], gc_fill_rectangle, 
		image_source_x, image_source_y,
		image_width, image_height);

/* copy image to pixmap */
		XCopyArea(display_id, screen_pixmap[i], image_area_window_id,
		  gc_fill_rectangle, image_source_x, image_source_y,
		  image_width, image_height,
	          image_dest_x, image_dest_y);

		XFreePixmap(display_id, screen_pixmap[i]);
		}

/* hide the zoom window */
	zoomhide_ ();
	image_flag = 0;
	}
}
/*********************************************************************/
/* Initialize colour bar */
/*********************************************************************/
void colourbarinit_ ()
{
	if(slider_flag == 0)
		{
		fprintf(stderr,"Slider not initialized");
	        return;
		}

/* create colour bar image */
	colour_bar_image = XCreateImage(display_id,visual_id,
		     image_depth, image_format, image_offset,
		     &colour_bar_pointer[0], slider_width, slider_height, 
		     image_bitmap_pad, image_bytes_per_line);

/* copy colour bar to image 1 ready for display */
	XPutImage(display_id,colour_bar1_window_id,
		  gc_fill_rectangle, colour_bar_image, 
		  image_source_x, image_source_y,
		  image_source_x, image_source_y, slider_width, slider_height);

	colour_bar_flag = 1;
	XtManageChild(colour_bar1);
/* copy colour bar to image 2 ready for display */
	if(slider_flag == 2)
	{
		XPutImage(display_id,colour_bar2_window_id,
		  gc_fill_rectangle, colour_bar_image, 
		  image_source_x, image_source_y,
		  image_source_x, image_source_y, slider_width, slider_height);

		colour_bar_flag = 2;
		XtManageChild(colour_bar2);
	}
	XSync(display_id, False);
}
/*********************************************************************/
/* Hide colour bar */
/*********************************************************************/
void colourbarhide_ ()
{
	if(slider_flag >= 1) sliderhide_ ();
	if(colour_bar_flag >= 1)
	{
		colour_bar_image->data = 0;
		XDestroyImage(colour_bar_image);	
		XSync(display_id, False);
	}
	colour_bar_flag = 0;
}
/*********************************************************************/
/* Set delay in microseconds                                         */
/*********************************************************************/
void	delay_ (int *idelay)
{
	unsigned delay;
	delay = *idelay;
	sleepdelay_ (&delay);
}
/*********************************************************************/
/* Draw a rectangular box, called from the call-back routine */
/*********************************************************************/
void	drawbox_ (int *ix1, int *iy1, int *ix2, int *iy2)
{
	if ( nboxes >= max_boxes )
	{
		printf ("Too many boxes, max_boxes too small.\n");
	        nboxes = max_boxes;
		return;
	}
	box_x[nboxes] = imin_ (*ix1, *ix2);
	box_y[nboxes] = imin_ (*iy1, *iy2);
	box_width[nboxes] = abs(*ix2 - *ix1);
	box_height[nboxes] = abs(*iy2 - *iy1);
	box_style[nboxes] = dash_style;

/* draw box in image area */
	XDrawRectangle
	(display_id, image_area_window_id, gc_draw_vector[dash_style],
	box_x[nboxes] + pan_x, box_y[nboxes] + pan_y, 
	box_width[nboxes], box_height[nboxes]);

/* draw box into pixmap */
	XDrawRectangle
	(display_id, screen_pixmap[map_no], gc_draw_vector[dash_style],
	box_x[nboxes], box_y[nboxes], 
	box_width[nboxes], box_height[nboxes]);

/* Draw box in zoom window if present */
	if(zoom_flag == 1)
	{
		zoom_diam_x = (int)box_width[nboxes] * zoom_factor_x;
		zoom_diam_y = (int)box_height[nboxes] * zoom_factor_y;
		convert_to_zoom(box_x[nboxes], box_y[nboxes],
			        &zoom_centre_x, &zoom_centre_y);
/* draw zoomed box in zoom area */
		XDrawRectangle
		(display_id, zoom_area_window_id, gc_draw_vector[dash_style],
	 	zoom_centre_x, zoom_centre_y, zoom_diam_x, zoom_diam_y);

/* draw box into zoom pixmap */
		XDrawRectangle
		(display_id, zoom_pixmap, gc_draw_vector[dash_style],
		 zoom_centre_x, zoom_centre_y, zoom_diam_x, zoom_diam_y);
	}

	XSync(display_id, False);

	nboxes++;
}
/*********************************************************************/
/* Draw a circle in the window, called from the call-back routine */
/*********************************************************************/
void drawcircle_ (int *ix, int *iy, float *rad)
{
	if ( ncircles >= max_circles )
	{
		printf ("Too many circles, max_circles too small.\n");
	        ncircles = max_circles;
		return;
	}
	irad = (int)*rad;
	circle_x[ncircles] = *ix - irad;
	circle_y[ncircles] = *iy - irad;
	circle_diam[ncircles] = 2.0  * *rad;
	circle_style[ncircles] = dash_style;

/* draw circle in image area */
	XDrawArc
	(display_id, image_area_window_id, gc_draw_vector[dash_style],
	circle_x[ncircles] + pan_x, circle_y[ncircles] + pan_y,
	circle_diam[ncircles], circle_diam[ncircles],
	circle_angle, circle_angle);

/* draw circle into pixmap */
	XDrawArc
	(display_id, screen_pixmap[map_no], gc_draw_vector[dash_style],
	circle_x[ncircles], circle_y[ncircles],
	circle_diam[ncircles], circle_diam[ncircles],
	circle_angle, circle_angle);

/* Draw circle in zoom window if present */
	if(zoom_flag == 1)
	{
	zoom_diam_x = circle_diam[ncircles] * zoom_factor_x;
	zoom_diam_y = circle_diam[ncircles] * zoom_factor_y;

/* convert coordinates to lie in zoom area */
	convert_to_zoom(circle_x[ncircles]+circle_diam[ncircles]/2,
			circle_y[ncircles]+circle_diam[ncircles]/2,
			&zoom_centre_x, &zoom_centre_y);
	zoom_centre_x = zoom_centre_x - zoom_diam_x / 2;
	zoom_centre_y = zoom_centre_y - zoom_diam_y / 2;

/* draw zoomed circle into zoom window */
	XDrawArc
	(display_id, zoom_area_window_id, gc_draw_vector[dash_style],
	zoom_centre_x, zoom_centre_y,
	zoom_diam_x, zoom_diam_y,
	circle_angle, circle_angle);

/* draw zoomed circle into zoom pixmap */
	XDrawArc
	(display_id, zoom_pixmap, gc_draw_vector[dash_style],
	zoom_centre_x, zoom_centre_y,
	zoom_diam_x, zoom_diam_y,
	circle_angle, circle_angle);
	}

	XSync(display_id, False);

	ncircles++;
}
/*********************************************************************/
/* Draw an elipse in the window, called from the call-back routine */
/*********************************************************************/
void drawelipse_ (int *ix, int *iy, float *radx, float *rady, float *rang)
{
	int iang,icount;
	float rx,ry;
	short sx,sy;
	XPoint points1[361];
	XPoint points2[361];
	XPoint points3[361];

        if ( nelipses >= max_elipses )
        {
                printf ("Too many elipses, max_elipses too small.\n");
                nelipses = max_elipses;
                return;
        }
        iradx = (int)*radx;
        irady = (int)*rady;
        elipse_x[nelipses] = *ix - iradx;
        elipse_y[nelipses] = *iy - irady;
        elipse_diamx[nelipses] = (int)(2.0  * *radx);
        elipse_diamy[nelipses] = (int)(2.0  * *rady);
        elipse_style[nelipses] = dash_style;
        elipse_ang1[nelipses] = *rang;

	/* Prepare a polygon to draw the elipse as lines */
        iang=(int)*rang;
 	icount = 0;
	for(i=0;i<361;i=i+5)
	{ rx = *radx * cs[i];
	  ry = *rady * sn[i];
	  sx =   cs[iang]*rx + sn[iang]*ry;
	  sy = - sn[iang]*rx + cs[iang]*ry;
	  points1[icount].x = sx + *ix + pan_x;
	  points1[icount].y = sy + *iy + pan_y;
	  points2[icount].x = sx + *ix;
	  points2[icount].y = sy + *iy;
	  icount++;
	}

	XDrawLines(display_id,image_area_window_id, gc_draw_vector[dash_style],
	points1,icount,CoordModeOrigin);
	
	XDrawLines(display_id,screen_pixmap[map_no], gc_draw_vector[dash_style],
	points2,icount,CoordModeOrigin);

        XSync(display_id, False);

        nelipses++;
}
/*********************************************************************/
/* Draw lines in the window */
/*********************************************************************/
void drawlines_ 
	(int *ix1, int *iy1, int *ix2, int *iy2, int *ilines)
{
	for (i = 0; i < *ilines; i++)
	{
	line_x1[nlines] = ix1[i];
	line_y1[nlines] = iy1[i];
	line_x2[nlines] = ix2[i];
	line_y2[nlines] = iy2[i];
	line_style[nlines] = dash_style;
	segments[i].x1 = ix1[i] + pan_x;
	segments[i].y1 = iy1[i] + pan_y;
	segments[i].x2 = ix2[i] + pan_x;
	segments[i].y2 = iy2[i] + pan_y;
	nlines++;
	if ( nlines >= max_lines )
		{
			printf ("Too many lines, max_lines too small.\n");
			nlines = max_lines;
			return;
		}
	}


/* draw lines in image area */
	XDrawSegments
	(display_id, image_area_window_id, gc_draw_vector[dash_style],
	 segments, *ilines);
	XSync(display_id, False);

	for (i = 0; i < *ilines; i++)
	{
	segments[i].x1 = ix1[i];
	segments[i].y1 = iy1[i];
	segments[i].x2 = ix2[i];
	segments[i].y2 = iy2[i];
	}

/* draw line in pixmap */
	XDrawSegments
	(display_id, screen_pixmap[map_no], gc_draw_vector[dash_style],
	 segments, *ilines);
	XSync(display_id, False);

	if (zoom_flag == 1) 
	{
	 	for (i = 0; i < *ilines; i++)
		{
		convert_to_zoom
		(ix1[i], iy1[i], &zoom_x1, &zoom_y1);
		segments[i].x1 = zoom_x1;
		segments[i].y1 = zoom_y1;

		convert_to_zoom
		(ix2[i], iy2[i], &zoom_x2, &zoom_y2);
		segments[i].x2 = zoom_x2;
		segments[i].y2 = zoom_y2;
		}

/* draw line in zoomed area window */
		XDrawSegments
		(display_id, zoom_area_window_id, gc_draw_vector[dash_style],
	 	segments, *ilines);

/* draw line in zoom pixmap */
		XDrawSegments
		(display_id, zoom_pixmap, gc_draw_vector[dash_style],
	 	segments, *ilines);
	}

	XSync(display_id, False);

}
/*********************************************************************/
/* Draw a map in the window, called from the fortran                 */
/*********************************************************************/
void	drawimage_ (int *nx, int *ny, int *isec, char *map, int *cflag)
{
	unsigned int	ncheck;
	int		row;
	int		col;
	static char	*pixel;

/* initialize */
	pan_flag = 0;

	pan_x = 0;
	pan_y = 0;

	image_dest_x = 0;
	image_dest_y = 0;
	map_width = *nx;
	map_height = *ny;
	map_no = *isec;
	map_size = map_width * map_height;

/* set number of maps to 1 if not set */
	if(no_maps == 0) no_maps = 1;

/* set image height and width */
	image_width = map_width;
	image_height = map_height;

/* calculate map size */
	if(no_maps == 1)
	{
	if(image_width < window_width)
		image_width = window_width;

	if(image_height < window_height)
		image_height = window_height;
	}
	else
	  {
	   image_dest_x = (window_width - map_width) / 2;
	   image_dest_y = (window_height - map_height) / 2;
	 }

/* if not first image, clear window */
	if(image_flag == 1) 
	{
		image->data = 0;
		XDestroyImage(image);
		XFreePixmap(display_id, screen_pixmap[map_no]);
		XSync(display_id, False);
		if (icallback_value == 999) return;
	}

/* Create pixmap to hold image */
	screen_pixmap[map_no] = XCreatePixmap(display_id, image_area_window_id,
			image_width, image_height, image_depth);

/* check resources sufficient, else trap and return */
	XSync(display_id, False);
	if (icallback_value == 999) return;

/* fill pixmap with blanks */
	XFillRectangle(display_id, screen_pixmap[map_no], gc_fill_rectangle, 
			image_source_x, image_source_y,
			image_width, image_height);

/* copy image to pointer array */
	memcpy(&image_pointer[0],map,(unsigned int)map_size);

/* create new image */
	image = XCreateImage(display_id,visual_id,
		     image_depth, image_format, image_offset,
		     &image_pointer[0], map_width, map_height, 
		     image_bitmap_pad, image_bytes_per_line);
		
/* copy map into pixmap ready for display */
	XPutImage(display_id,screen_pixmap[map_no],
		  gc_fill_rectangle, image, image_source_x, image_source_y,
		  image_source_x, image_source_y, map_width, map_height);

	XSync(display_id, False);
	if (icallback_value == 999) return;

/* start consistency check to see if pixmap can be read back correctly */
	j = 0;
	i = 0;
	*cflag = 0;

/* find first nonzero entry */
	if(image_pointer[0] == 0)
	{
		while (j == 0)
		{
			if(image_pointer[i] == 0)
			{
				i++;
				if(i > map_size)
				{
					*cflag = 2;
					goto allzero;
				}
			}
			else
			        j = i;
		}
	}

	row = j / map_width;
	col = j - row * map_width;

/* read back first 50 non-zero pixels */
	if(map_width >= 50)
		ncheck = 50;
	else
		ncheck = map_width;

	if(map_width - col < 50)
		ncheck = map_width - col;

	readimage = XGetImage(display_id,screen_pixmap[map_no],
			col, row, ncheck, 1, -1, image_format);

	if(readimage == NULL)
		{
		printf("XGetImage in screen read back failed\n");
		exit(0);
		}

	pixel = readimage->data;

/* compare newly read pixmap entries with those written */
	for (i = j; i < j + ncheck; i++)
	  {
	     if(*pixel != image_pointer[i]) 
			*cflag = 1;
	     pixel++;
	   }

	XDestroyImage(readimage);
	XSync(display_id, False);
	if (icallback_value == 999) return;

/* display image in window */
allzero:if(map_no == no_maps - 1)
	{
	image_flag = 1;
        drawpixmap_ (&map_no);
	}

}
/*********************************************************************/
/* draw pixmap in window */
/*********************************************************************/
void 	drawpixmap_ (int *isec)
{
	int	text_length = 0;
	map_no = *isec;

/* Draw pixmap */
	if (image_flag != 0)
	{
	XCopyArea(display_id, screen_pixmap[map_no], image_area_window_id,
		  gc_fill_rectangle, -pan_x, -pan_y, 
		  image_width, image_height,
	          image_dest_x, image_dest_y);

	}

/* process waiting events and flush the remainder */ 
	XFlush(display_id);
	event_get_next_(&event_true);
	if(event_true != 0) eventdispatch_ ();
	expose_flag = 1;

}
/*********************************************************************/
/* Draw a point in the window, called from the call-back routine */
/*********************************************************************/
void drawpoint_ (int *ix, int *iy)
{
	if ( npoints >= max_points )
	{
		printf ("Too many points, max_points too small.\n");
		npoints = max_points;
		return;
	}
	point_x[npoints] = *ix;
	point_y[npoints] = *iy;

/* draw point in image area */
	XDrawPoint
	(display_id, image_area_window_id, gc_draw_vector[0],
	 point_x[npoints] + pan_x, point_y[npoints] + pan_y);

/* draw point in pixmap */
	XDrawPoint
	(display_id, screen_pixmap[map_no], gc_draw_vector[0],
	 point_x[npoints], point_y[npoints]);

/* draw point in zoomed area */
	if (zoom_flag == 1) 
	{
	convert_to_zoom(point_x[npoints],point_y[npoints],&zoom_x1, &zoom_y1);

/* draw into zoom window */
	XDrawPoint(display_id, zoom_area_window_id, gc_draw_vector[0], 
		zoom_x1, zoom_y1);

/* draw into zoom pixmap */
	XDrawPoint(display_id, zoom_pixmap, gc_draw_vector[0], 
		zoom_x1, zoom_y1);
	}

	XSync(display_id, False);

	npoints++;
}
/*********************************************************************/
/* Draw a text string in the window, called from the call-back routine */
/*********************************************************************/
void drawtext_ (int *ix, int *iy, char *text_pointer, int *length)
{
	if ( ntext >= max_text )
	{
		printf ("Too many text strings, max_text too small.\n");
	        ntext = max_text;
		return;
	}

	text_x[ntext] = *ix;
	text_y[ntext] = *iy;
	strcpy(&text_string[ntext][0],text_pointer);
	text_lengths[ntext] = *length;
	text_font[ntext] = font_id;

/* draw string in image area */
	XDrawString
	(display_id, image_area_window_id, gc_draw_vector[0], 
	text_x[ntext] + pan_x, text_y[ntext] + pan_y,
	&text_string[ntext][0], text_lengths[ntext]);

/* draw string in pixmap */
	XDrawString
	(display_id, screen_pixmap[map_no], gc_draw_vector[0], 
	text_x[ntext], text_y[ntext],
	&text_string[ntext][0], text_lengths[ntext]);

/* draw in zoom window if present */
	if (zoom_flag == 1)

	{
	convert_to_zoom(text_x[ntext], text_y[ntext], &zoom_x1, &zoom_y1);

	XDrawString(display_id, zoom_area_window_id, gc_draw_vector[0], 
	zoom_x1, zoom_y1,
	&text_string[ntext][0], text_lengths[ntext]);

/* draw into zoom pixmap */
	XDrawString(display_id, zoom_pixmap, gc_draw_vector[0], 
	zoom_x1, zoom_y1,
	&text_string[ntext][0], text_lengths[ntext]);
	}

	XSync(display_id, False);

	ntext++;
}
/*****************************************************************/
/* error handler */
/*********************************************************************/
int	error_trap(Display *error_id, XErrorEvent *p_error)
{
	char	error_message[80];
	icallback_value = 0;
	if(p_error->error_code == BadAlloc &&
		p_error->request_code == X_CreatePixmap)
	{
		printf("error trapped \n");
		icallback_value = 999;
		ximagecallback_ (&icallback_value);
	}
	else
	{
		XGetErrorText(error_id,p_error->error_code,
			error_message, 80);
		fprintf(stderr," Error detected:\n %s\n", error_message);
		fprintf(stderr," Error code: %d\n", p_error->request_code);
		exit(1);
	}
	return((int) 0);
}
/*****************************************************************/
/* get next event */
/*********************************************************************/
void 	event_get_next_ (int *event_true)
{
	XtInputMask	event_mask;

	event_mask = XtAppPending(app_context);
	if (event_mask == 0)
	{
		*event_true = 0;
		return;
	}
	*event_true = 1;
}
/*********************************************************************/
/* get the event and dispatch it */
/*********************************************************************/
void 	eventdispatch_ ()
{
	XtAppNextEvent(app_context, &event_return);
	XtDispatchEvent(&event_return);
}
/*********************************************************************/
/* Initialize and display a window and basic menu on the screen */
/*********************************************************************/

void	init_ (int *max_window_width, int *max_window_height,
	       int *max_zoom_size, char *fontlist, int *nfonts)
{
        XVisualInfo vTemplate;
	XVisualInfo *visualList;
	int         visualsMatched;

	int		width;
	int		height;
	int		i;


/* set error handler */

	XSetErrorHandler(error_trap);

/* initialize variables*/
	map_no = 0;
	no_maps = 0;
	colour_bar_flag = 0;
	expose_flag = 0;
	expose_iobox_flag = 0;
	expose_label_flag = 0;
	expose_menu_flag = 0;
	expose_slider_flag = 0;
	image_flag = 0;
	label_flag = 0;
	iobox_flag = 0;
	menu_flag = 0;
	pointer_flag = 0;
	overlay_flag = 0;
	rubber_flag = 0;
	rubber_type = 0;
	slider_flag = 0;
	overlay_width = 128;
	overlay_height = 128;
	pan_x = 0;
	pan_y = 0;
	zoom_factor_x = 8;
	zoom_factor_y = 8;
	zoom_width = 128;
	zoom_height = 128;
	zoom_flag = 0;
	zoom_window_x1 = 0;
	zoom_window_y1 = 0;
	zoom_window_x2 = 0;
	zoom_window_y2 = 0;

	for(i=0;i<361;i++)
	{ cs[i]=cos((double)i*M_PI/180.0);
	  sn[i]=sin((double)i*M_PI/180.0);
	}

/* force integral,odd number in zoom area width, height */
	zoom_width = ((((zoom_width / zoom_factor_x) / 2) * 2) + 1) 
			* zoom_factor_x;
	zoom_height = ((((zoom_height / zoom_factor_y) / 2) * 2) + 1) 
			* zoom_factor_y;

/* Initialize top level widget */
	topLevel = XtVaAppInitialize(
        &app_context,       /* Application context */
        "Xdraw",            /* application class name */
        NULL, 0,            /* command line option list */
        &zero,NULL,         /* command line args */
        NULL,               /* for missing app-defaults file */
        NULL);              /* terminate varargs list */

/* set variables dependent on toplevel */
	display_id = XtDisplay(topLevel);
	scr_num = DefaultScreen(display_id);
	colourmap_size = DisplayCells(display_id,scr_num);
	colour_white = colourmap_size - 1;
	colour_black = colourmap_size - 2;
	colour_red = colourmap_size - 3;
	colour_blue = colourmap_size - 4;
	colour_green = colourmap_size - 5;

/* set font */
	if((font_default = XLoadQueryFont(display_id, fontname1)) == NULL)
		{
		fprintf(stderr,"Cannot load font: %s\n",fontname1);
		if((font_default = XLoadQueryFont(display_id, fontname2))
								  == NULL)
		{
			fprintf(stderr,"Cannot load font: %s\n",fontname2);
			exit(1);
		}
		}
	font_id = font_default->fid;
	XSetFont(display_id,DefaultGC(display_id,scr_num),font_id);
	text_font[0] = font_id;
	font_height = (short int)font_default->max_bounds.ascent +
			(short int)font_default->max_bounds.descent;
	iobox_height = font_height + 1;

/* get font list for courier */
	courier_fonts = XListFonts(display_id, courier_font,
				maxfontsperstyle, &no_fonts);
	for (i = 0; i < no_fonts && i < max_fonts; i++)
	{
		fonts[i] = courier_fonts[i];
		strcpy(fontlist,courier_fonts[i]);
		fontlist = fontlist + font_length;
	}
	total_fonts = no_fonts;

/* get font list for fixed - under test */
	fixed_fonts = XListFonts(display_id, fixed_font,
				maxfontsperstyle, &no_fonts);
	for (i = 0; i < no_fonts && i+total_fonts < max_fonts; i++)
	{
		fonts[i+total_fonts] = fixed_fonts[i];
		strcpy(fontlist,fixed_fonts[i]);
		fontlist = fontlist + font_length;
	}
	total_fonts = total_fonts + no_fonts;

/* get font list for helvetica - under test */
	helvetica_fonts = XListFonts(display_id, helvetica_font,
				maxfontsperstyle, &no_fonts);
	for (i = 0; i < no_fonts && i+total_fonts < max_fonts; i++)
	{
		fonts[i+total_fonts] = helvetica_fonts[i];
		strcpy(fontlist,helvetica_fonts[i]);
		fontlist = fontlist + font_length;
	}
	total_fonts = total_fonts + no_fonts;

/* get font list for times - under test */
	times_fonts = XListFonts(display_id, times_font,
				maxfontsperstyle, &no_fonts);
	for (i = 0; i < no_fonts && i+total_fonts < max_fonts; i++)
	{ 
		fonts[i+total_fonts] = times_fonts[i];
		strcpy(fontlist,times_fonts[i]);
		fontlist = fontlist + font_length;
	}
	total_fonts = total_fonts + no_fonts;
	if(total_fonts > max_fonts) total_fonts = max_fonts;
	*nfonts = total_fonts;

/* Create form widget to manage all the others */
	mainForm = XtVaCreateManagedWidget(
        "mainForm",              /* widget name */
        formWidgetClass,         /* widget class */
        topLevel,               /* parent widget*/
        NULL);                   /* argument list*/

/* get screen size */
	window_width = DisplayWidth(display_id, scr_num) - border_size;
	window_height = DisplayHeight(display_id, scr_num) - border_size;
	width = *max_window_width;
	height = *max_window_height;
	if (width < 0 || height < 0)
		{
		if (abs(width)  <= window_width &&
		    abs(height) <= window_height)
			{
			window_width  = abs(width);
			window_height = abs(height);
			}
		else
			{
			printf("Requested window size too large\n");
			return;
			}
		}
	*max_window_height = window_height;
	*max_window_width = window_width;
	*max_zoom_size = window_width * window_height;

/* set pointer tracking coordinate positions */
	pointer_x = window_width - border_size * 2;
	pointer_y = border_size;

/* server bit order */
	if(BitmapBitOrder(display_id) == LSBFirst)
		msbfirst = 0;
	else
		msbfirst = 1;

/* add image area widget */
/* Note : XtNbackground, (Pixel)colour_black,
	changed to next line since cursor tracking coordinates
        corrupted when pixmap moved 11.01.96 */

       image_area = XtVaCreateManagedWidget(
        "image_area",	
        simpleWidgetClass,
        mainForm,	
	XtNwidth, (Dimension)window_width,
	XtNheight, (Dimension)window_height,
	XtNbackground, None,
	XtNborderColor, (Pixel)colour_red,
        NULL);

/* add zoom area widget */
    zoom_area = XtVaCreateWidget(
        "zoom_area",		/* widget name */
        simpleWidgetClass,		/* widget class */
        mainForm,		/* parent widget*/
	XtNwidth, (Dimension)zoom_width,
	XtNheight, (Dimension)zoom_height,
	XtNborderColor, (Pixel)colour_red,
        NULL			/* argument list*/
        );

/* add overlay area widget */
    overlay_area = XtVaCreateWidget(
        "overlay_area",		/* widget name */
        simpleWidgetClass,		/* widget class */
        mainForm,		/* parent widget*/
	XtNwidth, (Dimension)overlay_width,
	XtNheight, (Dimension)overlay_height,
	XtNborderColor, (Pixel)colour_red,
        NULL			/* argument list*/
        );

/* add label widget */
    label = XtVaCreateWidget(
	"label",              
	labelWidgetClass,     
	mainForm,
	XtNfont, font_default,
	XtNborderColor, (Pixel)colour_red,
	XtNforeground, (Pixel)colour_black,
	XtNbackground, (Pixel)colour_white,
        NULL			/* argument list*/
        );

/* Create box widget to manage ioBox */
    ioBox = XtVaCreateWidget(
	"ioBox",              /* widget name */
	boxWidgetClass,         
	mainForm,               /* parent widget*/
	XtNheight, iobox_height,
	XtNwidth, iobox_width,
	XtNborderColor, (Pixel)colour_red,
	XtNbackground, (Pixel)colour_white,
	XtNforeground, (Pixel)colour_black,
	NULL);      

/* create label part of ioBox widget */
    ioBox_label = XtVaCreateWidget(
	"ioBox_label",              /* widget name */
	labelWidgetClass,         /* widget class */
	ioBox,               /* parent widget*/
	XtNfont, font_default,
	XtNjustify, XtJustifyLeft,
	XtNwidth, label_width,
	XtNborderColor, (Pixel)colour_red,
	XtNforeground, (Pixel)colour_black,
	XtNbackground, (Pixel)colour_white,
	NULL);      

/* create text part of ioBox widget */
	strcpy(store_string," ");
    ioBox_text = XtVaCreateWidget(
	"ioBox_text",              /* widget name */
	asciiTextWidgetClass,         /* widget class */
	ioBox,               		/* parent widget*/
	XtNuseStringInPlace, (Boolean)True,
	XtNstring, store_string,
	XtNeditType, XawtextEdit,
	XtNfont, font_default,
	XtNwidth, label_width,
	XtNlength, string_length,
	XtNborderColor, (Pixel)colour_red,
	XtNbackground, (Pixel)colour_white,
	XtNforeground, (Pixel)colour_black,
	NULL);      

/* Create box widget to manage slider */
    sliderBox = XtVaCreateWidget(
	"sliderBox",              /* widget name */
	formWidgetClass,         /* widget class */
	mainForm,               /* parent widget*/
	XtNborderColor, (Pixel)colour_red,
	XtNbackground, (Pixel)colour_white,
	XtNforeground, (Pixel)colour_black,
	NULL); 
     
/* create slider bar label */
     slider_label = XtVaCreateManagedWidget(
        "slider_label",
	labelWidgetClass,         /* widget class */
        sliderBox,
	XtNfont, font_default,
	XtNfromHoriz, NULL,
	XtNlabel, 
	"Centre mouse button moves slider,Click left button in map to quit",
	XtNforeground, (Pixel)colour_black,
	XtNbackground, (Pixel)colour_white,
	NULL);

/* create slider bar 1 widget */
     slider1 = XtVaCreateManagedWidget(
        "slider1",
        scrollbarWidgetClass,
        sliderBox,
        XtNwidth, slider_width,
        XtNheight, slider_height,
        XtNorientation, XtorientHorizontal,
	XtNforeground, (Pixel)colour_black,
	XtNbackground, (Pixel)colour_white,
	NULL);

/* add colour bar 1 widget */
       colour_bar1 = XtVaCreateManagedWidget(
        "colour_bar1",	
        simpleWidgetClass,
        sliderBox,
	XtNwidth, slider_width,
	XtNheight, slider_height,
	XtNbackground, (Pixel)colour_black,
	XtNborderColor, (Pixel)colour_red,
        NULL);

/* create slider bar 2 widget */
     slider2 = XtVaCreateManagedWidget(
        "slider2",
        scrollbarWidgetClass,
        sliderBox,
        XtNwidth, slider_width,
        XtNheight, slider_height,
        XtNorientation, XtorientHorizontal,
	XtNforeground, (Pixel)colour_black,
	XtNbackground, (Pixel)colour_white,
	NULL);

/* add colour bar 2 widget */
       colour_bar2 = XtVaCreateManagedWidget(
        "colour_bar2",	
        simpleWidgetClass,
        sliderBox,
	XtNwidth, slider_width,
	XtNheight, slider_height,
	XtNbackground, (Pixel)colour_black,
	XtNborderColor, (Pixel)colour_red,
        NULL);

	XtRealizeWidget(topLevel);

/* Add actions to read pointer position and re-display menu */
	XtAppAddActions(app_context, actlist, XtNumber(actlist));
	trans_table = XtParseTranslationTable(Trans);
	XtAugmentTranslations(mainForm,trans_table);

/* Add actions to return string after <cr> in ioBox */
	iobox_trans_table = XtParseTranslationTable(Io_trans);
	XtOverrideTranslations(ioBox_text,iobox_trans_table);


/* Add event handlers for expose events */
	XtAddEventHandler
	(image_area,ExposureMask,False,(XtEventHandler)ReDraw_image,0);
	XtAddEventHandler
	(overlay_area,ExposureMask,False,(XtEventHandler)ReDraw_overlay,0);
	XtAddEventHandler
	(zoom_area,ExposureMask,False,(XtEventHandler)ReDraw_zoom,0);
	XtAddEventHandler
	(ioBox,ExposureMask,False,(XtEventHandler)expose_iobox,0);
	XtAddEventHandler
	(label,ExposureMask,False,(XtEventHandler)expose_label,0);
	XtAddEventHandler
	(sliderBox,ExposureMask,False,(XtEventHandler)expose_slider,0);

/* initialize window ids etc. note : topLevel_window_id can only be
        set after realizing the widget */

	topLevel_window_id = XtWindow(topLevel);
	image_area_window_id = XtWindow(image_area);
	zoom_area_window_id = XtWindow(zoom_area);
	colour_bar1_window_id = XtWindow(colour_bar1);
	colour_bar2_window_id = XtWindow(colour_bar2);
	label_window_id = XtWindow(label);
	ioBox_window_id = XtWindow(ioBox);
	overlay_area_window_id = XtWindow(overlay_area);
	sliderBox_window_id = XtWindow(sliderBox);
	image_depth = DefaultDepth(display_id,DefaultScreen(display_id));
	image_format = ZPixmap;

/* set cursor */
	cursor = XCreateFontCursor(display_id,arrow);
	XDefineCursor(display_id,image_area_window_id,cursor);
	XDefineCursor(display_id,overlay_area_window_id,cursor);
	XDefineCursor(display_id,zoom_area_window_id,cursor);


/* set up visual, 8 bits, pseudocolour */
	vTemplate.screen = scr_num;
	vTemplate.depth = image_depth;
	vTemplate.bits_per_rgb = 8;
	vTemplate.class = PseudoColor;
	visualList = XGetVisualInfo(display_id,
		VisualScreenMask | VisualDepthMask
             | VisualClassMask | VisualBitsPerRGBMask,
			    &vTemplate, &visualsMatched);

/* try 6 bits */
	if (visualsMatched < 1)
	{
		vTemplate.bits_per_rgb = 6;
		visualList = XGetVisualInfo(display_id,
		VisualScreenMask | VisualDepthMask
        	     | VisualClassMask | VisualBitsPerRGBMask,
			    &vTemplate, &visualsMatched);
		printf("Trying 6 bit colour, may not work.\n");
	}

/* return if no matching visual available */
	if (visualsMatched < 1)
	{
		fprintf(stderr,"No matching visual available\n");
		return;
	}

	visual_id = visualList[0].visual;


/* create colour map */
	colourmap = XCreateColormap(display_id, image_area_window_id, 
		    visual_id, AllocAll);

/* create graphics contexts */
	gc_draw_vector[0] = XCreateGC
		(display_id,image_area_window_id,0,NULL);
	gc_draw_vector[1] = XCreateGC
		(display_id,image_area_window_id,0,NULL);
	gc_remove_vector = XCreateGC
		(display_id,image_area_window_id,0,NULL);
	gc_fill_rectangle = XCreateGC
		(display_id,image_area_window_id,0,NULL);
	dash_style = 0;

/* set default font */
	XSetFont(display_id, gc_draw_vector[0], font_id);
	XSetFont(display_id, gc_draw_vector[1], font_id);
	XSetFont(display_id, gc_remove_vector, font_id);

/* load colour bar map with densities */
	for (n = 0; n < slider_width; n++)
		{
		for (i = 0; i < slider_height; i++)
			colour_bar_map[i][n] = n;
		}

/* copy colour bar map to pointer array */
	memcpy(&colour_bar_pointer[0],colour_bar_map,
		slider_width * slider_height);
}

/*********************************************************************/
/* display io box */
/*********************************************************************/
void	ioboxdisplay_ (char *out_string, char *edit_string, int *istrings)
{
        int     	nstrings;

/* unmanage iobox if present */
	if (iobox_flag == 1)
		ioboxhide_ ();

/* unmanage label if present */
	if(label_flag == 1)
		labelhide_ ();
	label_flag = 0;

/* reset ioBox values to control label and text widgets*/
	nstrings = *istrings;

/* position iobox wrt top of window */
	XtVaSetValues(ioBox,
			XtNheight,
			(Dimension)(iobox_height*(nstrings+1)
						+ iobox_gap + font_spacer),
			XtNfromVert, NULL,
			XtNvertDistance, font_spacer,
	        	NULL);      
	XtManageChild(ioBox);

/* reset values for label part of iobox */
	XtVaSetValues(ioBox_label,
			XtNheight,
			(Dimension)(iobox_height * nstrings + font_spacer),
			XtNlabel,out_string,
	        	NULL);      
	XtManageChild(ioBox_label);

/* reset values for text part of iobox, extra height needed or text will
		not display */

	strcpy(store_string,edit_string);
	XtVaSetValues(ioBox_text,
		        XtNheight, iobox_height + font_spacer,
			XtNstring, store_string,
	        	NULL);      

/* focus keyboard so that the pointer can be anywhere */
	XtSetKeyboardFocus(mainForm,ioBox_text);

	XtManageChild(ioBox_text);
	XRaiseWindow(display_id,ioBox_window_id);

/* next display slider if present */
	if(slider_flag >= 1)
	{
		XtUnmanageChild(sliderBox);
		XtVaSetValues(sliderBox,
		XtNfromVert, ioBox,
		XtNvertDistance,1,
        	NULL);                

		XRaiseWindow(display_id,sliderBox_window_id);
		XtManageChild(sliderBox);
	}

/* if menu present, reposition it wrt slider */
	if(slider_flag >= 1 && menu_flag == 1)
	{
		XtUnmanageChild(menuBox);
		XtVaSetValues(menuBox,
		XtNfromVert, sliderBox,
		XtNvertDistance,1,
        	NULL);                

		XRaiseWindow(display_id,menuBox_window_id);
		XtManageChild(menuBox);
	}
/* if menu but no slider, reposition wrt iobox */
	else if(slider_flag == 0 && menu_flag == 1)
	{
		XtUnmanageChild(menuBox);
		XtVaSetValues(menuBox,
		XtNfromVert, ioBox,
		XtNvertDistance,1,
        	NULL);                

		XRaiseWindow(display_id,menuBox_window_id);
		XtManageChild(menuBox);
	}

	while (expose_iobox_flag == 0)
	 eventdispatch_ ();

	XSync(display_id, False);
	expose_iobox_flag = 0;
	iobox_flag = 1;
}
/*********************************************************************/
/* hide iobox */
/*********************************************************************/
void	ioboxhide_ ()
{
	if(iobox_flag == 1)
	{
		XtUnmanageChild(ioBox);
		iobox_flag = 0;
		XtSetKeyboardFocus(mainForm,mainForm);

/* if slider and menu reposition menu wrt slider */
		if(slider_flag >= 1 && menu_flag == 1)
		{
			XtUnmanageChild(sliderBox);
			XtVaSetValues(sliderBox,
			XtNfromVert, NULL,
			XtNvertDistance,1,
	        	NULL);                

			XRaiseWindow(display_id,sliderBox_window_id);
			XtManageChild(sliderBox);
/* reposition menu */
			XtUnmanageChild(menuBox);
			XtVaSetValues(menuBox,
			XtNfromVert, sliderBox,
			XtNvertDistance,1,
	        	NULL);                
			XRaiseWindow(display_id,menuBox_window_id);
			XtManageChild(menuBox);
		}
/* reposition menu to top of window */
		else if(slider_flag == 0 && menu_flag == 1)
		{
			XtUnmanageChild(menuBox);
			XtVaSetValues(menuBox,
			XtNfromVert, NULL,
			XtNvertDistance,4,
	        	NULL);  
			XRaiseWindow(display_id,menuBox_window_id);
			XtManageChild(menuBox);
		}
	XSync(display_id, False);
	}
}
/*********************************************************************/
/* return io box */
/*********************************************************************/
void	ioboxreadstring_ (char *in_string)
{
	strcpy(in_string,store_string);
}
/*********************************************************************/
/* display label */
/*********************************************************************/
void	labeldisplay_ (char *label_string)
{

/* unmanage label if present */
	if(label_flag == 1)
		labelhide_ ();

/* unmanage iobox if present */
	if (iobox_flag == 1)
		ioboxhide_ ();
	iobox_flag = 0;

/* position label at top of screen */
	XtVaSetValues(label,
	XtNlabel, label_string,
	XtNfromVert, NULL,
	XtNvertDistance,4,
       	NULL);                

/* position slider below it if present */
	if(slider_flag >= 1)
	{
		XtUnmanageChild(sliderBox);
		XtVaSetValues(sliderBox,
		XtNfromVert, label,
		XtNvertDistance,1,
        	NULL);                

		XRaiseWindow(display_id,sliderBox_window_id);
		XtManageChild(sliderBox);
	}

/* if menu present, reposition it wrt slider */
	if(slider_flag >= 1 && menu_flag == 1)
	{
		XtUnmanageChild(menuBox);
		XtVaSetValues(menuBox,
		XtNfromVert, sliderBox,
		XtNvertDistance,1,
        	NULL);                

		XRaiseWindow(display_id,menuBox_window_id);
		XtManageChild(menuBox);
	}
/* if menu but no slider, reposition wrt label */
	else if(slider_flag == 0 && menu_flag == 1)
	{
		XtUnmanageChild(menuBox);
		XtVaSetValues(menuBox,
		XtNfromVert, label,
		XtNvertDistance,1,
        	NULL);                

		XRaiseWindow(display_id,menuBox_window_id);
		XtManageChild(menuBox);
	}

/* manage label widget */
	XtManageChild(label);
	XRaiseWindow(display_id,label_window_id);
	while (expose_label_flag == 0)
	 eventdispatch_ ();

	XSync(display_id, False);
	expose_label_flag = 0;
	label_flag = 1;
}
/*********************************************************************/
/* hide label */
/*********************************************************************/
void	labelhide_ ()
{
/* if slider and menu reposition slider to top & menu wrt slider */
	if(slider_flag >= 1 && menu_flag == 1)
	{
		XtUnmanageChild(sliderBox);
		XtVaSetValues(sliderBox,
		XtNfromVert, NULL,
		XtNvertDistance,1,
        	NULL);                

		XRaiseWindow(display_id,sliderBox_window_id);
		XtManageChild(sliderBox);

/* reposition menu wrt slider */
		XtUnmanageChild(menuBox);
		XtVaSetValues(menuBox,
		XtNfromVert, sliderBox,
		XtNvertDistance,1,
        	NULL);                
		XRaiseWindow(display_id,menuBox_window_id);
		XtManageChild(menuBox);
	}
/* reposition menu to top of window */
	else if(slider_flag == 0 && menu_flag == 1)
	{
		XtUnmanageChild(menuBox);
		XtVaSetValues(menuBox,
		XtNfromVert, NULL,
		XtNvertDistance,4,
        	NULL);  
		XRaiseWindow(display_id,menuBox_window_id);
		XtManageChild(menuBox);
	}

/* hide label */
	if(label_flag == 1)
	{
	XtUnmanageChild(label);
	XSync(display_id, False);
	label_flag = 0;
	}
}
/*********************************************************************/
/* Initialize menu */
/*********************************************************************/
void	menu_callback(Widget w, XtPointer client_data,
			XawListReturnStruct *call_data)
{
/* unhighlight item */
	XawListUnhighlight(w);

/* set dummy arguments */
	icallback_value = call_data->list_index + 1;
	ximagecallback_ (&icallback_value);
}
/*********************************************************************/
/* set flag for continuous processing 				     */
/*********************************************************************/
void	menuevent_ ()
{
	event_proc = 1;
}
/*********************************************************************/
/* hide the menu */
/*********************************************************************/
void 	menuhide_ ()
{
	if(menu_flag == 1)
	{
	XtUnmanageChild(menuBox);

/* remove event handler if waiting for events */
	if(event_proc == 0)
		XtRemoveEventHandler
		(menuBox,ExposureMask,False,(XtEventHandler)expose_menu,0);

	event_proc = 0;

	ReDraw_image ();
	XSync(display_id, False);
	menu_flag = 0;
	}
}
/**************************************************************************/
/* initialize and display menu                                            */
/**************************************************************************/
void	menuinit_ (int *nitems, int *max_length, char *menulist)
{
	int 		nlist;
	int 		length;

	nlist = *nitems;
	length = *max_length;

/* If menu already present, destroy old widget and remove event handler */
	if(menu_flag == 1) 
		{
		XtDestroyWidget(menuBox);
		menu_flag = 0;
		}

	for (i = 0; i < nlist; i++)
	{
		menu_pointer[i] = menulist;
		menulist = menulist + length;
	}
    
/* slider bar present, position beneath it */
	if(slider_flag >= 1)
	        menuBox = XtVaCreateManagedWidget(
        		"menuBox",              /* widget name */
        		listWidgetClass,         /* widget class */
	        	mainForm,               /* parent widget*/
			XtNfont, font_default,
			XtNdefaultColumns,1,
			XtNlist, menu_pointer,
			XtNnumberStrings, nlist,
			XtNborderColor, (Pixel)colour_red,
			XtNforeground, (Pixel)colour_black,
			XtNbackground, (Pixel)colour_white,
			XtNheight, (nlist)*30,
			XtNfromVert, sliderBox,
			XtNvertDistance,1,
	        	NULL);                   
/* if label present, create menu below it */
	else if(label_flag == 1)
	        menuBox = XtVaCreateManagedWidget(
        		"menuBox",              /* widget name */
        		listWidgetClass,         /* widget class */
	        	mainForm,               /* parent widget*/
			XtNfont, font_default,
			XtNdefaultColumns,1,
			XtNlist, menu_pointer,
			XtNnumberStrings, nlist,
			XtNborderColor, (Pixel)colour_red,
			XtNforeground, (Pixel)colour_black,
			XtNbackground, (Pixel)colour_white,
			XtNheight, (nlist)*30,
			XtNfromVert, label,
			XtNvertDistance,1,
	        	NULL);                   
/* no slider or label, position directly under top of window */
	else
	        menuBox = XtVaCreateManagedWidget(
         		"menuBox",              /* widget name */
        		listWidgetClass,         /* widget class */
	        	mainForm,               /* parent widget*/
			XtNfont, font_default,
			XtNdefaultColumns,1,
			XtNlist, menu_pointer,
			XtNnumberStrings, nlist,
			XtNborderColor, (Pixel)colour_red,
			XtNforeground, (Pixel)colour_black,
			XtNbackground, (Pixel)colour_white,
			XtNheight, (nlist)*30,
			XtNfromVert, NULL,
			XtNvertDistance,4,
        		NULL);                   /* argument list*/

/* Add callback for menu item */
		XtAddCallback(menuBox, XtNcallback, 
		(XtCallbackProc)menu_callback, (XtPointer)0);

		menuBox_window_id = XtWindow(menuBox);
	        XSetWindowColormap(display_id,menuBox_window_id,colourmap);

/* add event handler except where continuous event processing is done */
	if(event_proc == 0)
	{
		XtAddEventHandler
		(menuBox,ExposureMask,False,(XtEventHandler)expose_menu,0);

		while (expose_menu_flag == 0)
		 eventdispatch_ ();
	}
/*	else
	{
		event_get_next_(&event_true);
	        if(event_true != 0) eventdispatch_ ();
	}*/

	XSync(display_id, False);
	expose_menu_flag = 0;
	menu_flag = 1;

}
/*********************************************************************/
/* hide overlay window */
/*********************************************************************/
void	overlayhide_ ()
{
/* Hide the overlay window if present */
	if(overlay_flag == 1)
	{
		overlay_map->data = 0;
		XDestroyImage(overlay_map);	
		XFreePixmap(display_id, overlay_pixmap);
		XtUnmanageChild(overlay_area);
		XSync(display_id, False);
	}
	overlay_flag = 0;
}
/*********************************************************************/
/* initialize overlay window */
/*********************************************************************/
void	overlayinit_ (int *ix, int *iy, int *overlay_w, int *overlay_h,
			char *omap)
{
	overlay_pointer_x = *ix;
	overlay_pointer_y = *iy;
	overlay_width = *overlay_w;
	overlay_height = *overlay_h;

/* copy image to pointer array */
	memcpy(&overlay_pointer[0],omap,overlay_width * overlay_height);

/* hide overlay window if present */
	if(overlay_flag == 1) overlayhide_ ();

/* create overlay map */
	overlay_map = XCreateImage(display_id,visual_id,
		     image_depth, image_format, image_offset,
		     &overlay_pointer[0], 
		     (unsigned int)overlay_width, (unsigned int)overlay_height,
		     image_bitmap_pad, image_bytes_per_line);

/* create overlay pixmap */
	overlay_pixmap = XCreatePixmap(display_id,overlay_area_window_id,
		(unsigned int)overlay_width, (unsigned int)overlay_height, 
		image_depth);

	XSync(display_id, False);
	if (icallback_value == 999) return;

/* fill overlay pixmap with zeroes */
	XFillRectangle(display_id, overlay_pixmap, gc_fill_rectangle, 
		image_source_x, image_source_y,
		(unsigned int)overlay_width, (unsigned int)overlay_height);

/* write overlay map into overlay pixmap */
	XPutImage(display_id, overlay_pixmap,
		  gc_fill_rectangle,overlay_map,image_source_x, image_source_y,
		  image_source_x, image_source_y, 
		  (unsigned int)overlay_width, (unsigned int)overlay_height);

/* move overlay widget to correct place on window and set colours */
	overlay_window_x = overlay_pointer_x - overlay_width / 2 + pan_x;
	overlay_window_y = overlay_pointer_y - overlay_height / 2 + pan_y;

	XtVaSetValues( overlay_area,
			XtNfromHoriz, NULL,
			XtNfromVert, NULL,
			XtNwidth, (Dimension)overlay_width,
			XtNheight, (Dimension)overlay_height,
			XtNhorizDistance, overlay_window_x,
			XtNvertDistance, overlay_window_y,
			XtNborderColor, (Pixel)colour_red,
			XtNforeground, (Pixel)colour_black,
			XtNbackground, (Pixel)colour_white,
			NULL);

	XtManageChild(overlay_area);

/* put overlay window in front of image area */
	XRaiseWindow(display_id,overlay_area_window_id);
	overlay_flag = 1;
}
/*********************************************************************/
/* read line of image window */
/*********************************************************************/
void	readimage_  (int *icol, int *irow, int *ipixmap, char *pixels)
{
/* ipixmap 0 read from main screen pixmap
   ipixmap 1 read from overlay pixmap */

	if(*ipixmap == 0)
		readimage = XGetImage(display_id,screen_pixmap[map_no],
			(unsigned int)*icol, (unsigned int)*irow,
			image_width,1,
			-1,image_format);
	else if(*ipixmap == 1)
		readimage = XGetImage(display_id,overlay_pixmap,
			(unsigned int)*icol, (unsigned int)*irow,
			overlay_width,1,
			-1,image_format);

	XSync(display_id, False);
	if (icallback_value == 999) return;

	if(readimage == NULL)
		{
		printf("XGetImage failed\n");
		exit(0);
		}

/* transfer data to *pixels */
	dataline = readimage->data;
	if(*ipixmap == 0)
		for (i = 0; i < image_width; i++)
			*pixels++ = *dataline++;
	else if(*ipixmap == 1)
		for (i = 0; i < overlay_width; i++)
			*pixels++ = *dataline++;

	XDestroyImage(readimage);
}
/*********************************************************************/
/* set up colour table */
/*********************************************************************/
void	setcolourtable_ 
	(int *index_start, int *nentries, 
	 int *red_pointer, int *green_pointer, int *blue_pointer)
{
	char		icontinue;
	colour_entries = *nentries;
	colour_start = *index_start;

/* set users colour table */
	ncolours = colour_start + colour_entries - 1;
	for( i=colour_start; i < ncolours+1; i++)
	{
		colours[i].pixel = (unsigned long) i;
		colours[i].flags = DoRed | DoGreen | DoBlue;
		colours[i].red = (unsigned short) *red_pointer;
		colours[i].green = (unsigned short) *green_pointer;
		colours[i].blue = (unsigned short) *blue_pointer;
		*red_pointer++;
		*green_pointer++;
		*blue_pointer++;
	}

	for( i = ncolours; i < colourmap_size-6; i++)
	{
		colours[i].pixel = (unsigned long) i;
		colours[i].flags = DoRed | DoGreen | DoBlue;
		colours[i].red = 0;
		colours[i].green = 65535;
		colours[i].blue = 0;
	}

/* set green for drawing vectors */
	colours[colour_green].pixel = colour_green;
	colours[colour_green].flags = DoRed | DoGreen | DoBlue;
	colours[colour_green].red = 0;
	colours[colour_green].green = 65535;
	colours[colour_green].blue = 0;

/* set blue */
	colours[colour_blue].pixel = colour_blue;
	colours[colour_blue].flags = DoRed | DoGreen | DoBlue;
	colours[colour_blue].red = 0;
	colours[colour_blue].green = 0;
	colours[colour_blue].blue = 65535;

/* set red for drawing border */
	colours[colour_red].pixel = colour_red;
	colours[colour_red].flags = DoRed | DoGreen | DoBlue;
	colours[colour_red].red = 65535;
	colours[colour_red].green = 0;
	colours[colour_red].blue = 0;

/* set black for foreground */
	colours[colour_black].pixel = colour_black;
	colours[colour_black].flags = DoRed | DoGreen | DoBlue;
	colours[colour_black].red = 0;
	colours[colour_black].green = 0;
	colours[colour_black].blue = 0;

/* set white for background, pixel 0 reserved for dialog box background */
	colours[colour_white].pixel = colour_white;
	colours[colour_white].flags = DoRed | DoGreen | DoBlue;
	colours[colour_white].red = 65535;
	colours[colour_white].green = 65535;
	colours[colour_white].blue = 65535;

	XStoreColors(display_id, colourmap, colours, colourmap_size);

/* pause for sun workstations to catch up */

        while (expose_flag == 0)
	    {
	           event_get_next_(&event_true);
	           if(event_true != 0) eventdispatch_ ();
	      }

	XSetWindowColormap(display_id,topLevel_window_id,colourmap);
	XSetWindowColormap(display_id,XtWindow(mainForm),colourmap);
	XSetWindowColormap(display_id,image_area_window_id,colourmap);
	XSetWindowColormap(display_id,label_window_id,colourmap);
	XSetWindowColormap(display_id,ioBox_window_id,colourmap);
	XSetWindowColormap(display_id,XtWindow(ioBox_label),colourmap);
	XSetWindowColormap(display_id,XtWindow(ioBox_text),colourmap);
	XSetWindowColormap(display_id,overlay_area_window_id,colourmap);
	XSetWindowColormap(display_id,sliderBox_window_id,colourmap);
	XSetWindowColormap(display_id,zoom_area_window_id,colourmap);
	XSetWindowColormap(display_id,XtWindow(slider_label),colourmap);
	XSetWindowColormap(display_id,XtWindow(slider1),colourmap);
	XSetWindowColormap(display_id,XtWindow(slider2),colourmap);
	XSetWindowColormap(display_id,colour_bar1_window_id,colourmap);
	XSetWindowColormap(display_id,colour_bar2_window_id,colourmap);

/* set foreground colours for graphics contexts */
	XSetForeground
	(display_id, gc_fill_rectangle, colours[colour_black].pixel);
	XSetForeground
	(display_id, gc_draw_vector[0], (Pixel)128);
	XSetForeground
	(display_id, gc_draw_vector[1], (Pixel)128);
	XSetForeground
	(display_id, gc_remove_vector, (Pixel)127);

/* set function for drawing and removing vectors */
	XSetFunction(display_id, gc_fill_rectangle, (int)GXcopy);
	XSetFunction(display_id, gc_draw_vector[0], (int)GXor);
	XSetFunction(display_id, gc_draw_vector[1], (int)GXor);
	XSetFunction(display_id, gc_remove_vector, (int)GXand);

/*set dash and solid attributes*/
	XSetLineAttributes(display_id,gc_draw_vector[0],
	line_width, LineSolid, cap_style, join_style);
	XSetLineAttributes(display_id,gc_draw_vector[1],
	line_width, LineOnOffDash, cap_style, join_style);
	XSetLineAttributes(display_id,gc_remove_vector,
	line_width, LineSolid, cap_style, join_style);

	XSync(display_id, False);
}
/*********************************************************************/
/* quit the program */
/*********************************************************************/
void	quit_ ()
{
	exit(0);
}
/*********************************************************************/
/* remove box */
/*********************************************************************/
void 	removebox_ (int *ix1, int *iy1, int *ix2, int *iy2)
{
/* start loop to look for box */
	for (i = 0; i < nboxes; i++)
	{
	if(box_x[i] == *ix1
	&& box_y[i] == *iy1
	&& box_width[i] == (*ix2 - *ix1)
	&& box_height[i] == (*iy2 - *iy1))
		{
/* remove box from image area */
		XDrawRectangle
		(display_id, image_area_window_id, gc_remove_vector,
			box_x[i] + pan_x, box_y[i] + pan_y, 
				box_width[i], box_height[i]);

/* draw box into pixmap */
		XDrawRectangle
		(display_id, screen_pixmap[map_no], gc_remove_vector,
		box_x[i], box_y[i], box_width[i], box_height[i]);

/* Draw box in zoom window if present */
		if(zoom_flag == 1)
			{
			zoom_diam_x = (int)box_width * zoom_factor_x;
			zoom_diam_y = (int)box_height * zoom_factor_y;
			convert_to_zoom(box_x[i], box_y[i],
			        &zoom_centre_x, &zoom_centre_y);
/* draw zoomed box in zoom area */
			XDrawRectangle
			(display_id, zoom_area_window_id, gc_remove_vector,
		 	 zoom_centre_x, zoom_centre_y, 
			 zoom_diam_x, zoom_diam_y);

/* draw box into zoom pixmap */
			XDrawRectangle
			(display_id, zoom_pixmap, gc_remove_vector,
			 zoom_centre_x, zoom_centre_y, 
			 zoom_diam_x, zoom_diam_y);
			}

		XSync(display_id, False);

/* shift boxes up in list */
		nboxes = nboxes - 1;
		if(nboxes > 0)
		{
			box_x[i] = box_x[nboxes];
			box_y[i] = box_y[nboxes];
			box_width[i] = box_width[nboxes];
			box_height[i] = box_height[nboxes];
		}
		}
	}
}
/*********************************************************************/
/* remove circle */
/*********************************************************************/
void 	removecircle_ 
	(int *ix, int *iy, float *rad)
{

	irad = (int)*rad;
/* start loop to look for circle */
	for (i = 0; i < ncircles; i++)
	{
	if(circle_x[i] == *ix - irad
	&& circle_y[i] == *iy - irad
	&& circle_diam[i] == (int)(2.0 * *rad))

/* circle matches, overdraw it */
		{
/* remove circle from image area */
		XDrawArc
		(display_id, image_area_window_id, gc_remove_vector, 
		circle_x[i] + pan_x, circle_y[i] + pan_y,
		circle_diam[i], circle_diam[i],
		circle_angle, circle_angle);

/* remove circle from pixmap */
		XDrawArc
		(display_id, screen_pixmap[map_no], gc_remove_vector, 
		circle_x[i], circle_y[i],
		circle_diam[i], circle_diam[i],
		circle_angle, circle_angle);

/* remove circle in zoom window if present */
		if(zoom_flag == 1)
			{
			zoom_diam_x = circle_diam[i] * zoom_factor_x;
			zoom_diam_y = circle_diam[i] * zoom_factor_y;

/* convert coordinates to lie in zoom area */
			convert_to_zoom(circle_x[i]+circle_diam[i]/2,
				circle_y[i]+circle_diam[i]/2,
				&zoom_centre_x, &zoom_centre_y);
			zoom_centre_x = zoom_centre_x - zoom_diam_x / 2;
			zoom_centre_y = zoom_centre_y - zoom_diam_y / 2;

/* remove zoomed circle from zoom window */
			XDrawArc
			(display_id, zoom_area_window_id, gc_remove_vector, 
			zoom_centre_x, zoom_centre_y,
			zoom_diam_x, zoom_diam_y,
			circle_angle, circle_angle);

/* remove zoomed circle from zoom pixmap */
			XDrawArc
			(display_id, zoom_pixmap, gc_remove_vector, 
			zoom_centre_x, zoom_centre_y,
			zoom_diam_x, zoom_diam_y,
			circle_angle, circle_angle);
			}

		XSync(display_id, False);
/* shift final circle to fill the space */
                ncircles = ncircles - 1;
                if(ncircles > 0)
                {
                        circle_x[i] = circle_x[ncircles];
                        circle_y[i] = circle_y[ncircles];
                        circle_diam[i] = circle_diam[ncircles];
                }
                return;
                }
	}
}
/*********************************************************************/
/* remove elipse */
/*********************************************************************/
void 	removeelipse_ 
        (int *ix, int *iy, float *radx, float *rady, float *rang)
{

        int iang,icount,iloc;
        float rx,ry;
        short sx,sy;
        XPoint points1[361];
        XPoint points2[361];


	iradx = (int)*radx;
	irady = (int)*rady;

	/* start loop to look for elipse */
	for (i = 0; i < nelipses; i++)
	{
		if(elipse_x[i] == *ix - iradx
		&& elipse_y[i] == *iy - irady
		&& elipse_diamx[i] == (int)(2.0 * *radx)
		&& elipse_diamy[i] == (int)(2.0 * *rady)
       	 	&& elipse_ang1[i] == *rang)

		/* elipse matches, overdraw it */
		{
			/* remove elipse from image area */
			/* Prepare a polygon to draw the elipse as lines */
	       	 	iang=(int)*rang;
	       	 	icount = 0;
	        	for(iloc=0;iloc<361;iloc=iloc+5)
	        	{ rx = *radx * cs[iloc];
	       		  ry = *rady * sn[iloc];
	       		  sx =   cs[iang] * rx + sn[iang] * ry;
	       		  sy = - sn[iang] * rx + cs[iang] * ry;
	       		  points1[icount].x = sx + *ix + pan_x;
	       		  points1[icount].y = sy + *iy + pan_y;
	       		  points2[icount].x = sx + *ix;
	       		  points2[icount].y = sy + *iy;
	       		  icount++;
	       		}
	
		        XDrawLines(display_id,image_area_window_id, gc_remove_vector,
		        points1,icount,CoordModeOrigin);
	
		        XDrawLines(display_id,screen_pixmap[map_no], gc_remove_vector,
		        points2,icount,CoordModeOrigin);

			XSync(display_id, False);

			/* shift final elipse to fill the space */
	                nelipses = nelipses - 1;
	       	        if(nelipses > 0)
	       	        {
       		          elipse_x[i] = elipse_x[nelipses];
       		          elipse_y[i] = elipse_y[nelipses];
       		          elipse_diamx[i] = elipse_diamx[nelipses];
       		          elipse_diamy[i] = elipse_diamy[nelipses];
       		          elipse_ang1[i] = elipse_ang1[nelipses];
       		        }
			return;
		}
	}
}
/*********************************************************************/
/* remove lines */
/*********************************************************************/
void 	removelines_ 
	(int *ix1, int *iy1, int *ix2, int *iy2, int *ilines)
{
	int		jlines;

/* if ilines = 0, just remove most recent line */
	if(*ilines == 0)
	{
		*ilines = 1;
		ix1[0] = line_x1[nlines-1];
		iy1[0] = line_y1[nlines-1];
		ix2[0] = line_x2[nlines-1];
		iy2[0] = line_y2[nlines-1];
	}

/* start loop to remove lines */
	for (i = 0;i < *ilines; i++)
		{
/* remove line from image area */
		segments[i].x1 = ix1[i] + pan_x;
		segments[i].y1 = iy1[i] + pan_y;
		segments[i].x2 = ix2[i] + pan_x;
		segments[i].y2 = iy2[i] + pan_y;
		}
	XDrawSegments
	(display_id, image_area_window_id, gc_remove_vector, 
	 segments, *ilines);

/* remove line segments from pixmap */
	for (i = 0;i < *ilines; i++)
		{
		segments[i].x1 = ix1[i];
		segments[i].y1 = iy1[i];
		segments[i].x2 = ix2[i];
		segments[i].y2 = iy2[i];
		}
	XDrawSegments
	(display_id, screen_pixmap[map_no], gc_remove_vector,
	 segments, *ilines);

/* remove from zoom area if present */
	if (zoom_flag == 1) 
		{
		for (i = 0;i < *ilines; i++)
			{
			convert_to_zoom(ix1[i], iy1[i], 
					&zoom_x1, &zoom_y1);
			convert_to_zoom(ix2[i], iy2[i], 
					&zoom_x2, &zoom_y2);
			segments[i].x1 = zoom_x1;
			segments[i].y1 = zoom_y1;
			segments[i].x2 = zoom_x2;
			segments[i].y2 = zoom_y2;
			}
		XDrawSegments
		(display_id, zoom_area_window_id, gc_remove_vector,
		 segments, *ilines);

/* remove line in zoom pixmap */
		XDrawSegments
		(display_id, zoom_pixmap, gc_remove_vector,
		 segments, *ilines);
		}

	XSync(display_id, False);

/* remove line(s) from list */
	for (i = 0; i < *ilines; i++)
		{
		jlines = nlines;
		for (n = 0; n < jlines; n++)
			if(line_x1[n] == ix1[i] 
			&& line_y1[n] == iy1[i]
			&& line_x2[n] == ix2[i]
			&& line_y2[n] == iy2[i])

/* move the existing lines up to fill the space */
				{
				line_x1[n] = line_x1[nlines];
				line_y1[n] = line_y1[nlines];
				line_x2[n] = line_x2[nlines];
				line_y2[n] = line_y2[nlines];
				nlines = nlines - 1;
				}
		}
	return;
}
/*********************************************************************/
/* remove point */
/*********************************************************************/
void	removepoint_ (int *ix, int *iy)
{

/* start loop to look for point */
	for (i = 0; i < npoints; i++)
	{
	if(point_x[i] == *ix && point_y[i] == *iy)
/* point coords match, overdraw the point */
		{
/* remove point from image area */
		XDrawPoint
		(display_id, image_area_window_id, gc_remove_vector, 
		point_x[i] + pan_x, point_y[i] + pan_y);

/* remove point from pixmap */
		XDrawPoint
		(display_id, screen_pixmap[map_no], gc_remove_vector, 
		point_x[i], point_y[i]);

/* remove point from zoomed area */
		if (zoom_flag == 1) 
		{
		convert_to_zoom(point_x[i],point_y[i],&zoom_x1, &zoom_y1);

/* remove from zoom window */
		XDrawPoint(display_id, zoom_area_window_id, gc_remove_vector, 
		zoom_x1, zoom_y1);

/* remove from zoom pixmap */
		XDrawPoint(display_id, zoom_pixmap, gc_remove_vector, 
		zoom_x1, zoom_y1);
		}

	XSync(display_id, False);

/* move the existing points up to fill the space */
		npoints = npoints - 1;
		if(npoints > 0)
		{
			point_x[i] = point_x[npoints];
			point_y[i] = point_y[npoints];
		}
		return;
		}
	}
}
/*********************************************************************/
/* remove text */
/*********************************************************************/
void removetext_ (int *ix, int *iy, char *text_pointer, int *length)
{

/* start loop to look for text */
	for (i = 0; i < ntext; i++)
	{
	if(text_x[i] == *ix && text_y[i] == *iy)
/* point coords match, overdraw the text */
		{
/* set font */
		XSetFont(display_id, gc_remove_vector, text_font[i]);
/* remove string from image area */
		XDrawString
		(display_id, image_area_window_id, gc_remove_vector, 
		text_x[i] + pan_x, text_y[i] + pan_y,
		&text_string[i][0], text_lengths[i]);

/* remove string from pixmap */
		XDrawString
		(display_id, screen_pixmap[map_no], gc_remove_vector, 
		text_x[i], text_y[i],
		&text_string[i][0], text_lengths[i]);

/* remove string from zoom window */
	 	if (zoom_flag == 1)
		{
		convert_to_zoom(text_x[i], text_y[i], &zoom_x1, &zoom_y1);

/* remove from zoom window */
		XDrawString(display_id, zoom_area_window_id, gc_remove_vector, 
		zoom_x1, zoom_y1,
		&text_string[i][0], text_lengths[i]);

/* remove from zoom pixmap */
		XDrawString(display_id, zoom_pixmap, gc_remove_vector, 
		zoom_x1, zoom_y1,
		&text_string[i][0], text_lengths[i]);
		}

	XSync(display_id, False);

/* move the existing text strings up to fill the space */
		ntext = ntext - 1;
		if(ntext > 0)
		{
			text_x[i] = text_x[ntext];
			text_y[i] = text_y[ntext];
			strncpy(&text_string[i][0],&text_string[ntext][0],
				string_length);
			text_lengths[i] = text_lengths[ntext];
			text_font[i] = text_font[ntext];
		}
		return;
		}
	}
}
/*********************************************************************/
/* Clear image and zoom areas of text, vectors etc. */
/*********************************************************************/
void 	removevectors_ ()
{
	short		line_x[16384];
	short		line_y[16384];
	int		mlines;
	int		nvectors;
	int		ix;
        int		iy;
	float		rad;
	float		radx;
	float		rady;
	float		rang;
	int		rline_x1[16384];
	int		rline_y1[16384];
	int		rline_x2[16384];
	int		rline_y2[16384];
	int		ixtemp;
	int		iytemp;

/* Draw over all boxes in list */
	nvectors = nboxes;
	for ( n = 0; n < nvectors; n++)
	{
	ix = box_x[n];
	iy = box_y[n];
	ixtemp = ix + box_width[n];
	iytemp = iy + box_height[n];
	removebox_ (&ix, &iy, &ixtemp, &iytemp);
	}

/* Draw over all circles in list */
	nvectors = ncircles;
	for ( n = 0; n < nvectors; n++)
	{
	rad = (float)circle_diam[n] * 0.5;
	irad = (int)rad;
	ix = circle_x[n] + irad;
	iy = circle_y[n] + irad;
	removecircle_(&ix,&iy,&rad);
	}

/* Draw over all elipses in list */
        nvectors = nelipses;
        for ( n = 0; n < nvectors; n++)
        {
          radx = (float)elipse_diamx[n] * 0.5;
          rady = (float)elipse_diamy[n] * 0.5;
          rang  = (float)elipse_ang1[n];
          iradx = (int)radx;
          irady = (int)rady;
          ix = elipse_x[n] + iradx;
          iy = elipse_y[n] + irady;
          removeelipse_(&ix,&iy,&radx,&rady,&rang);
        }
	nelipses = 0;

/* Draw over all lines in list */
	for (n = 0; n < nlines; n++)
		{
		rline_x1[n] = line_x1[n];
		rline_y1[n] = line_y1[n];
		rline_x2[n] = line_x2[n];
		rline_y2[n] = line_y2[n];
		}
	mlines = nlines;
	removelines_ (rline_x1, rline_y1, rline_x2, rline_y2, &mlines);

/* Draw over all points in list */
	nvectors = npoints;
	for ( n = 0; n < nvectors; n++)
	removepoint_ (&point_x[0], &point_y[0]);

/* Draw over all text in list */
	nvectors = ntext;
	for ( n = 0; n < nvectors; n++)
	{
	ixtemp = text_x[0];
	iytemp = text_y[0];
	removetext_ (&ixtemp, &iytemp, 
		      &text_string[0][0], &text_lengths[0]);
	}

/* copy pixmap to image area */
	if(image_flag == 1)
	  drawpixmap_ (&map_no);

/* clear zoom window */
	if(zoom_flag == 1)
	{
	XClearWindow(display_id, zoom_area_window_id);
	XCopyArea(display_id, zoom_pixmap, zoom_area_window_id,
		  gc_fill_rectangle, 0, 0, 
                  (unsigned int)zoom_width, (unsigned int)zoom_height, 0, 0);
	}

}
/*********************************************************************/
/* set line style dashed */
/*********************************************************************/
void 	setdash_ ()
{
	dash_style = 1;
}
/*********************************************************************/
/* set line style solid */
/*********************************************************************/
void 	setsolid_ ()
{
	dash_style = 0;
}
/*********************************************************************/
/* hide slider bar */
/*********************************************************************/
void	sliderhide_ ()
{
	if(slider_flag >= 1)
	{
	XtUnmanageChild(sliderBox);
	slider_flag = 0;

/* both menu and label present, reposition menu wrt label */
	if(label_flag == 1 && menu_flag == 1)
		{
		XtUnmanageChild(menuBox);
		XtVaSetValues(menuBox,
			XtNfromVert, label,
			XtNvertDistance,1,
	        	NULL);                   /* argument list*/
		XtManageChild(menuBox);
		XRaiseWindow(display_id,menuBox_window_id);
		}
/* menu only, reposition menu wrt top of window */
	else if(label_flag == 0 && menu_flag ==1)
		{
		XtUnmanageChild(menuBox);
		XtVaSetValues(menuBox,
			XtNfromVert, NULL,
			XtNvertDistance,4,
	        	NULL);
		XtManageChild(menuBox);
		XRaiseWindow(display_id,menuBox_window_id);
		}
	XSync(display_id, False);
	}
}
/*********************************************************************/
/* slider initialize and display */
/*********************************************************************/
void	sliderinit_ (int *nsliders, float *slider1_start, 
                                     float *slider2_start)
{
	if(slider_flag >= 1) XtUnmanageChild(sliderBox);
	slider_flag = *nsliders;
	ksliders = *nsliders;
	slider1_start_position = *slider1_start;
	slider2_start_position = *slider2_start;

/* position wrt label if present */
	if(label_flag == 1)
		XtVaSetValues(sliderBox,
			XtNfromVert, label,
			XtNvertDistance,1,
	        	NULL);

/* position wrt iobox if present */
	else if(iobox_flag == 1)
		XtVaSetValues(sliderBox,
			XtNfromVert, ioBox,
			XtNvertDistance,1,
	        	NULL);

/* if menu present reposition it */
	if(menu_flag == 1)
		{
		XtUnmanageChild(menuBox);
		XtVaSetValues(menuBox,
			XtNfromVert, sliderBox,
			XtNvertDistance,1,
	        	NULL);                   /* argument list*/
		XtManageChild(menuBox);
		XRaiseWindow(display_id,menuBox_window_id);
		}

/* position slider1 in the centre and add callback */
	XawScrollbarSetThumb(slider1, slider1_start_position, slider_size);
	XtAddCallback(slider1, XtNjumpProc, 
		(XtCallbackProc)action_slider_callback, (XtPointer)1);

/* reset geometry for single or double slider */
	if(slider_flag == 1)
	{
		XtUnmanageChild(slider2);
		XtUnmanageChild(colour_bar2);
		XtVaSetValues(slider1,
		XtNfromHoriz, slider_label,
		XtNhorizDistance, 4,
		XtNfromVert, NULL,
		XtNvertDistance, 4,
		NULL);

		XtVaSetValues(colour_bar1,
		XtNfromHoriz, slider1,
		XtNhorizDistance, 4,
		XtNfromVert, NULL,
		XtNvertDistance, 4,
		NULL);
	}
/* position slider 2 in the centre and add callback */
	else if(slider_flag == 2)
	{
		XawScrollbarSetThumb(slider2, slider2_start_position, 
                                                        slider_size);
		XtAddCallback(slider2, XtNjumpProc, 
			(XtCallbackProc)action_slider_callback, (XtPointer)2);
/* reset slider 1*/
		XtVaSetValues(slider1,
		XtNfromHoriz, NULL,
		XtNfromVert, slider_label,
		XtNvertDistance, 4,
		NULL);
/* reset colour bar 1*/
		XtVaSetValues(colour_bar1,
		XtNfromHoriz, slider1,
		XtNhorizDistance, 4,
		XtNfromVert, slider_label,
		XtNvertDistance, 4,
		NULL);
/* reset slider 2 */
		XtVaSetValues(slider2,
		XtNfromHoriz, colour_bar1,
		XtNhorizDistance,4,
		XtNfromVert, slider_label,
		XtNvertDistance, 4,
		NULL);
/* reset colour bar 2 */
		XtVaSetValues(colour_bar2,
		XtNfromHoriz, slider2,
		XtNhorizDistance, 4,
		XtNfromVert, slider_label,
		XtNvertDistance, 4,
		NULL);
		XtManageChild(slider2);
		XtManageChild(colour_bar2);
	}

/* push slider in front */
	XtManageChild(sliderBox);
	XRaiseWindow(display_id, sliderBox_window_id);

/* synchronise */
	XSync(display_id, False);
	while (expose_slider_flag == 0)
	 eventdispatch_ ();

	XSync(display_id, False);
	expose_slider_flag = 0;
}

/*********************************************************************/
/* zoom init - called from fortran */
/*********************************************************************/
void	zoominit_ ()
{
	   image_pointer_x = window_width / 2;
	   image_pointer_y = window_height / 2;
	   zoom_display ();
}
/*********************************************************************/
/* zoom display */
/*********************************************************************/
void	zoom_display ()
{
	int		imap;
	int 		imapx;
	int 		imapy;
	int 		zoom_start_x;
	int 		zoom_start_y;
	int		ix;
	int		iy;
	int		izoom;

	zoom_pointer_x = image_pointer_x + pan_x;
	zoom_pointer_y = image_pointer_y + pan_y;

/* create zoom area. zoom_start_x, zoom_start_y are map starting points .
   Note that zoom window is forced odd to centre the middle pixel as
   well as possible, that the zoom factor is forced even. The displayed 
   zoom window will be even, but the number of zoomed pixels is always odd */

	zoom_start_x = image_pointer_x - image_dest_x 
	                    - zoom_width / (2 * zoom_factor_x);
	zoom_start_y = image_pointer_y - image_dest_y 
	                    - zoom_height / (2 * zoom_factor_y);
	for (iy = 0; iy < zoom_height; iy++)
		{
		imapy = zoom_start_y + iy / zoom_factor_y;
		for (ix = 0; ix < zoom_width; ix++)
			{
			imapx = zoom_start_x + ix / zoom_factor_x;
			imap = map_width * imapy + imapx;
			izoom = iy * zoom_width + ix;

/* load zoom map if within limits */
			if(imapx >= 0 && imapx < map_width)
				zoom_area_pointer[izoom] = image_pointer[imap];
			else
				zoom_area_pointer[izoom] = 0;
			}
		}

/* hide zoom window */
	if(zoom_flag == 1) zoomhide_ ();

/* create zoom map */
	zoom_map = XCreateImage(display_id,visual_id,
		     image_depth, image_format, image_offset,
		     &zoom_area_pointer[0], 
		     (unsigned int)zoom_width, (unsigned int)zoom_height,
		     image_bitmap_pad, image_bytes_per_line);

/* create zoom pixmap */
	zoom_pixmap = XCreatePixmap(display_id,zoom_area_window_id,
		(unsigned int)zoom_width, (unsigned int)zoom_height, 
		image_depth);

	XSync(display_id, False);
	if (icallback_value == 999) return;

/* move zoom widget to correct place on window and set colours */
	zoom_window_x1 = zoom_pointer_x - zoom_width / 2 ;
	zoom_window_y1 = zoom_pointer_y - zoom_height / 2 ;
	zoom_window_x2 = zoom_pointer_x + zoom_width / 2 ;
	zoom_window_y2 = zoom_pointer_y + zoom_height / 2 ;

	XtVaSetValues( zoom_area,
			XtNfromHoriz, NULL,
			XtNfromVert, NULL,
			XtNwidth, (Dimension)zoom_width,
			XtNheight, (Dimension)zoom_height,
			XtNhorizDistance, zoom_window_x1,
			XtNvertDistance, zoom_window_y1,
			XtNborderColor, (Pixel)colour_red,
			XtNforeground, (Pixel)colour_black,
			XtNbackground, (Pixel)colour_white,
			NULL);

	XtManageChild(zoom_area);

/* put zoom window in front of image area */
	XRaiseWindow(display_id,zoom_area_window_id);

/* fill zoom pixmap with zeroes */
	XFillRectangle(display_id, zoom_pixmap, gc_fill_rectangle, 
		image_source_x, image_source_y,
		(unsigned int)zoom_width, (unsigned int)zoom_height);

/* write zoom map into zoom pixmap */
	XPutImage(display_id, zoom_pixmap,
		  gc_fill_rectangle,zoom_map,image_source_x, image_source_y,
		  image_source_x, image_source_y, 
		  (unsigned int)zoom_width, (unsigned int)zoom_height);

/* draw all boxes in list */
	for ( n = 0; n < nboxes; n++)
	{
		style = box_style[i];
		zoom_diam_x = (int)box_width[n] * zoom_factor_x;
		zoom_diam_y = (int)box_height[n] * zoom_factor_y;
                convert_to_zoom(box_x[n], box_y[n],
                                &zoom_centre_x, &zoom_centre_y);
/* draw box into zoom pixmap */
                XDrawRectangle
                (display_id, zoom_pixmap, gc_draw_vector[style],
                 zoom_centre_x, zoom_centre_y, zoom_diam_x, zoom_diam_y);

	}
/* Draw all circles in list */
	for ( n = 0; n < ncircles; n++)
	{
		style = circle_style[n];
		zoom_diam_x = circle_diam[n] * zoom_factor_x;
		zoom_diam_y = circle_diam[n] * zoom_factor_y;

/* convert coordinates to lie in zoom area */
		convert_to_zoom(circle_x[n]+circle_diam[n]/2,
			circle_y[n]+circle_diam[n]/2,
			&zoom_centre_x, &zoom_centre_y);
		zoom_centre_x = zoom_centre_x - zoom_diam_x / 2;
		zoom_centre_y = zoom_centre_y - zoom_diam_y / 2;

/* draw zoomed circle */
		XDrawArc
		(display_id, zoom_pixmap, gc_draw_vector[style],
		zoom_centre_x, zoom_centre_y, 
		zoom_diam_x, zoom_diam_y,
		circle_angle, circle_angle);
	}

/* Draw all vectors in list */
	n = 0;
	for ( i = 0; i < nlines; i++)
		{
		if( i > 0 ) 
/* if line style changes, write previous segment */
			{
			if(line_style[i] != line_style[i-1])
				{
/* draw line in zoom pixmap */
				XDrawSegments
				(display_id, zoom_pixmap, 
				gc_draw_vector[style],
				segments, n);
				n = 0;
				}
			}
/* convert coordinates for zoom window */
		style = line_style[i];
		convert_to_zoom
		(line_x1[i], line_y1[i], &zoom_x1, &zoom_y1);
		segments[n].x1 = zoom_x1;
		segments[n].y1 = zoom_y1;

		convert_to_zoom
		(line_x2[i], line_y2[i], &zoom_x2, &zoom_y2);
		segments[n].x2 = zoom_x2;
		segments[n].y2 = zoom_y2;
		n++;
	}
/* draw final segment in zoom pixmap */
	if(n > 0)
		{
		XDrawSegments
		(display_id, zoom_pixmap, gc_draw_vector[style],
		segments, n);
		}

/* Draw all points in list */
	for ( n = 0; n < npoints; n++)
	{
		convert_to_zoom(point_x[n], point_y[n], &zoom_x1, &zoom_y1);

		XDrawPoint(display_id, zoom_pixmap, gc_draw_vector[0], 
		zoom_x1, zoom_y1);
	}

/* Draw all text in list */
	for ( n = 0; n < ntext; n++)
	{
		convert_to_zoom(text_x[n], text_y[n], &zoom_x1, &zoom_y1);

		XDrawString(display_id, zoom_pixmap, gc_draw_vector[0], 
		zoom_x1, zoom_y1, &text_string[n][0], text_lengths[n]);
	}

	zoom_flag = 1;
}
