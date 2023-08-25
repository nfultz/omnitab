/*  x11_src.c  - c routines for Dataplot X11 driver */
/*  UPDATED  - June     1991.  Fixed bug in color handling. */
/*  UPDATED  - June     1991.  Use color names rather than specific
 *                             RGB values.  Had a problem with PC 
 *                             implementations not having enough free
 *                             color cells.  The global variable 
 *                             COLOR_TYPE is used to control which 
 *                             method is used.
 *  UPDATED  - January  1992.  Problem with pixmap on black-white
 *                             devices, modified "xerase" routine.
 *  UPDATED  - January  1992.  Variable FONT_NAME_DEFAULT to softcode
 *                             default X11 font.
 *  UPDATED  - April    1992.  Handle routine names as upper case (for
 *                             the Cray)
 *  UPDATED  - May      1992.  Convex had a problem with i_to_s 
 *                             routine.  Temporary fix is to use
 *                             "-pcc" flag to force K&R C.  Need to
 *                             update so will work correctly for either
 *                             ANSI standard C or K&R C.  I think only
 *                             problem is trying to modify character
 *                             constants.  May need to convert to
 *                             strings.
 */
/*  This driver has been tested on Suns (both monochrome and color),
 *  Silicon Graphics Iris, HP-9000, a Cray Y-MP, DEC workstation, 
 *  and a PC implementation.   */
/*
 *  The Dataplot command can be entered from any currently open window.
 *  Dataplot's normal alphanumeric I/O (e.g., entering commands, output
 *  from FIT's) will occur in this window.  Dataplot will create a separate
 *  graphics window where the graphical output will be generated.  Note
 *  that although Dataplot will send a recommended size and position for
 *  the graphics window, the user has control over this through the window
 *  manager.  Dataplot will check for the actual dimensions whenever a clear
 *  screen command is entered.  The bit gravity is set to NorthWest so that
 *  drawing coordinates will be valid until the next erase page command.
 *
 *  A dummy version of this library is maintained for those systems
 *  that do not support X11 or that do not allow C routines to be called
 *  from Fortran.  Since the dummy library is coded in Fortran, routine
 *  names will be limited to six characters.
 *
 *  Note that calling C from Fortran is not standard.  Therefore, these
 *  routines may require some tweaking for some operating systems.  This
 *  version was tested on the Sun (using Unix).  The following is a list
 *  of portability issues.
 *
 *  1) The Sun Unix system appends an underscore ("_") to the Fortran
 *     name.  For example, if the Fortran routine calls XEND, the C name
 *     will be XEND_.  If your operating system does not do this, simply
 *     remove the underscores from the C routines.
 *
 *
 *     NOTE: May, 1990.  The global variable APPEND_UNDERSCORE is now
 *           used to handle this problem.  If your system appends the
 *           underscore, set this variable to 1.  Otherwise set it to
 *           0.  Conditional compilation statements will insert the correct
 *           function name.
 *
 *     NOTE: April, 1992.  Most Unix Fortran compilers automatically 
 *           convert routine names to lower case.  However, the Cray does
 *           not.  Add the global variable SUBROUTINE_CASE to handle this.
 *           For lower case, set this variable to 1.  For upper case, set
 *           this variable to 0.
 *
 *  2) The primary portability problem will be in how arguments are passed
 *     back and forth between Fortran and C.  Note that Fortran passes
 *     arguments by reference (i.e., it sends the address) while C passes
 *     by value (i.e., a local copy is made of the variable).  On the
 *     Sun (and most Unix systems), real and integer arguments can be sent
 *     via the function arguments.  Be aware that the C subroutine must
 *     declare the function arguments to be pointers since Fortran is
 *     sending an address rather than a value.  The function argument must
 *     be declared as a pointer even if its value is not being returned to
 *     the calling Fortran program.
 *
 *     However, note that arrays are treated as pointers in C, so array
 *     names are not declared as pointers in C.
 *
 *     Character strings should be sent as an array of Ascii Decimal
 *     Equivalents (i.e., "A" should be sent as the integer 65, "0"
 *     should be sent as the integer 48) to correspond to how C defines
 *     character strings.  Note that the string should end with the null
 *     character (integer 0).
 *
 *     If your system passes data differently (e.g., through some type of
 *     common), both the calling Fortran program and the C code here will
 *     have to be modified.
 *
 *  3) The Unix makefile may need to be modified for some Unix systems.
 *     This should be straightforward (may need to specify different
 *     directories for X11 or system libraries).
 *
 *  This initial version is strictly a "device driver".  Development to
 *  utilize the windowing capabilities of X is under consideration, but is
 *  not yet implemented.
 *
 *  The primary references for writing this driver were:
 *
 *     "Introduction to the X Window System" by Oliver Jones (1989).
 *     "Xlib Programming Manual for Version 11" by Adrian Nye (1990).
 *
 *  The following routines are included:
 *
 *  xcheck     - check for expose and configure events
 *  xrdloc     - read mouse position when button pressed
 *  xinit      - initialize X11
 *  xend       - close X11
 *  xclear     - flush the buffer
 *  xerase     - clear the screen
 *  xupdat     - copy pixmap to screen
 *  xfore      - set the foreground color
 *  xback      - set the background color
 *  xlattr     - set line attributes (width, style, cap, join)
 *  xdraw      - draw a polyline
 *  xpoint     - draw a point
 *  xcirc      - draw a circle
 *  xregfl     - solid fill of a region
 *  xtexth     - draw a horizontal character string
 *  xtextv     - draw a vertical character string
 *  xtattr     - set text attributes
 *  i_to_s     - utility routine to convert array of ADE's to string (Array version)
 *  set_screen - utility routine to set screen height and width
 *
 */

/*  Site dependent definitions (see comments above) */

#define APPEND_UNDERSCORE 1
#define SUBROUTINE_CASE 1
#define COLOR_TYPE 1

/*  include files */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdio.h>
#include <strings.h>

/* global definitions */

#if COLOR_TYPE
#define MAX_COLORS    66
#else
#define MAX_COLORS    16
#endif
#define BORDER_WIDTH      3
#define DEFAULT_X_SIZE  600
#define DEFAULT_Y_SIZE  465
#define DEFAULT_X        50
#define DEFAULT_Y        50
#define MIN_X_SIZE      200
#define MIN_Y_SIZE      200

/* X11 declarations */
Display       *display;           /* display connection */
Window        window;             /* window identifier */
Pixmap        pixmap;             /* pixmap identifier */
GC            gc;                 /* graphics context */
XGCValues     gcvalues;           /* set graphics context attributes */
XColor        colors[MAX_COLORS]; /* colors to draw with */
XColor        exact_def;          /* returns exact RGB values */
XFontStruct   *font_struct;       /* returned font structure pointer */
Colormap      color_map;          /* default color map */
Visual        *vis;               /* pointer to visual structure */
XEvent        event;              /* holds X server events */
XSetWindowAttributes  window_attributes;    /* attributes structure */
XWindowAttributes     returned_attributes;  /* returned attributes */
unsigned long value_mask;         /* value mask for window attributes */
static XSizeHints                 /* size hints for window manager */
    xsh = {
       (PPosition | PSize | PMinSize),   /* flags */
       DEFAULT_X_SIZE,                   /* height */
       DEFAULT_Y_SIZE,                   /* width */
       MIN_Y_SIZE,                       /* minimum height */
       MIN_X_SIZE,                       /* minimum width */
       DEFAULT_X,                        /* x coordinate */
       DEFAULT_Y,                        /* y coordinate */
    };
static XWMHints       /* more hints for window manager */
    xwmh = {
       (InputHint | StateHint),         /* flags */
       False,                           /* input */
       NormalState,                     /* initial state */
       0,                               /* icon pixmap */
       0,                               /* icon window */
       0, 0,                            /* icon location */
       0,                               /* icon mask */
       0                                /* window group */
    };
static XClassHint    /* class hints for window manager */
    xch = {
        "dataplot",                      /* name */
        "Graphics",                      /* class */
    };
static char  *argv[] = {                 /* dummy command line arguments */
                          (char *)NULL   /* required by a few calls */
                        };
static int   argc = 0;

/* common parameters */
unsigned int  width, height;      /* last known window size */
unsigned long black, white;       /* values for black and white */
int           screen;             /* default screen */
int           num_cells;          /* number of color cells available */
int           num_planes;         /* number of planes available */
int           depth;              /* depth (number of planes or bits/pixel */
int           color_flag;         /* 0 - monochrome, 1 - color */
int           max_colors;         /* maximum colors actually allocated */
int           configure_flag;     /* 0 - configuration up to date,
                                     1 - configuration has been changed */
int           expose_flag;        /* 0 - expose status has not changed,
                                     1 - expose status has changed */
unsigned int root_width, root_height, root_depth, root_bw;
Window       root_id;
int          root_x, root_y;
int          pixmap_flag;         /* 0 - do not draw to pixmap
                                     1 - draw to pixmap */
int          color_list[MAX_COLORS]; /* Color availability */
char         *color_names[] = {   /* define color names (for Release */
 /*  0 */        "black",         /* 3, p. 184 of Adrian Nye book,   */
 /*  1 */        "white",         /* these should be recognized by   */
 /*  2 */        "green",         /* all Release 3 implementations.  */
 /*  3 */        "yellow",        /* If color not physically         */
 /*  4 */        "red",           /* available, should be mapped to  */
 /*  5 */        "blue",          /* closest possible alternative.   */
 /*  6 */        "magenta",
 /*  7 */        "cyan",
 /*  8 */        "orange",
 /*  9 */        "yellow green",
 /* 10 */        "dark green",
 /* 11 */        "light blue",
 /* 12 */        "blue violet",
 /* 13 */        "violet red",
 /* 14 */        "dark slate gray",
 /* 15 */        "light gray",
 /* 16 */        "aquamarine",
 /* 17 */        "brown",
 /* 18 */        "cadet blue",
 /* 19 */        "coral",
 /* 20 */        "cornflower blue",
 /* 21 */        "dark olive green",
 /* 22 */        "dark orchid",
 /* 23 */        "dark slate blue",
 /* 24 */        "dark turquoise",
 /* 25 */        "firebrick",
 /* 26 */        "forest green",
 /* 27 */        "gold",
 /* 28 */        "goldenrod",
 /* 29 */        "gray",
 /* 30 */        "indian red",
 /* 31 */        "khaki",
 /* 32 */        "dim gray",
 /* 33 */        "light steel blue",
 /* 34 */        "lime green",
 /* 35 */        "maroon",
 /* 36 */        "medium aquamarine",
 /* 37 */        "medium blue",
 /* 38 */        "medium forest green",
 /* 39 */        "medium goldenrod",
 /* 40 */        "medium orchid",
 /* 41 */        "medium sea green",
 /* 42 */        "medium slate blue",
 /* 43 */        "medium spring green",
 /* 44 */        "medium turquoise",
 /* 45 */        "medium violet red",
 /* 46 */        "midnight blue",
 /* 47 */        "navy",
 /* 48 */        "orange red",
 /* 49 */        "orchid",
 /* 50 */        "pale green",
 /* 51 */        "pink",
 /* 52 */        "plum",
 /* 53 */        "purple",
 /* 54 */        "salmon",
 /* 55 */        "sea green",
 /* 56 */        "sienna",
 /* 57 */        "sky blue",
 /* 58 */        "slate blue",
 /* 59 */        "spring green",
 /* 60 */        "steel blue",
 /* 61 */        "tan",
 /* 62 */        "thistle",
 /* 63 */        "turquoise",
 /* 64 */        "violet",
 /* 65 */        "wheat"
              };

/* flags for current attribute settings */
static int    OPEN_FLAG = 0;          /* 0 - X11 closed, 1 - X11 open */
int           WIDTH_CURRENT;          /* current line width */
int           LINE_STYLE_CURRENT;     /* current line style */
int           CAP_STYLE_CURRENT;      /* current cap style */
int           JOIN_STYLE_CURRENT;     /* current join style */
char          *FONT_NAME_CURRENT;     /* name of current font */
char          *FONT_NULL = "ZZZZ";    /* null font */
char          *FONT_NAME_DEFAULT = "8X13"; /* name of default font */
int           FONT_HEIGHT_CURRENT;    /* pixel ascent of current font */
int           FONT_DESCENT_CURRENT;   /* descent of current font */
int           FONT_GAP_CURRENT;       /* vertical gap of current font */
int           BACKGROUND_CURRENT;     /* current background color */
int           COLOR_CURRENT;          /* current color */
int           PIXMAP_CURRENT;         /* 0 - closed, 1 - open */
int           ORIENTATION_CURRENT;    /* current orientation */

#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void  xclear_(), xend_(), xcheck_();
void  xinit_(), xlattr_(), xdraw_();
void  xpoint_(), xcirc_(), xregfl_(), xrdloc_();
void  xfore_(),  xerase_(), xupdat_();
void  xtexth_(), xtextv_(),xtattr_();
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void  XCLEAR_(), XEND_(), XCHECK_();
void  XINIT_(), XLATTR_(), XDRAW_();
void  XPOINT_(), XCIRC_(), XREGFL_(), XRDLOC_();
void  XFORE_(),  XERASE_(), XUPDAT_();
void  XTEXTH_(), XTEXTV_(), XTATTR_();
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void  xclear(), xend(), xcheck();
void  xinit(), xlattr(), xdraw();
void  xpoint(), xcirc(), xregfl(), xrdloc();
void  xfore(),  xerase(), xupdat();
void  xtexth(), xtextv(),xtattr();
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void  XCLEAR(), XEND(), XCHECK();
void  XINIT(), XLATTR(), XDRAW();
void  XPOINT(), XCIRC(), XREGFL(), XRDLOC();
void  XFORE(),  XERASE(), XUPDAT();
void  XTEXTH(), XTEXTV(), XTATTR();
#endif
void  i_to_s(), set_screen(), xback();

/* XCHECK  - routine to check for X expose and configuration events.
 *           Specifically, resizing of the graphics window by the window
 *           manager (usually initiated by the user) generates a
 *           configuration event.  Placing windows on top of the graphics
 *           window and then re-raising it will generate an expose event.
 *           Dataplot makes no attempt to check if the part of the
 *           graphics window is not visible.  It simply draws and lets the
 *           window manager do any required clipping.
 *
 *           Note that this routine will not wait for any events.  It will
 *           simply check if there are any currently in the event queue.
 *
 *           Also, no action is taken for expose and configure events.
 *           This routine simply sets a flag indicating that these events
 *           were found.  It is the calling programs option as to what
 *           should be done about them.
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xcheck_(expose_flag_2, error_flag)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XCHECK_(expose_flag_2, error_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xcheck(expose_flag_2, error_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XCHECK(expose_flag_2, error_flag)
#endif
int  *error_flag;
int  *expose_flag_2;
{
     XEvent   event;            /* holds Xserver events */

     *error_flag = 0;
     *expose_flag_2 = 0;
     while (XEventsQueued(display, QueuedAfterReading) != 0) {/* check queue */
          XNextEvent(display, &event);        /* get next event from queue */
          switch (event.type) {
          case DestroyNotify:   /* window has been destroyed */
              XFreeGC(display, gc);
              XDestroyWindow(display,window);
              XCloseDisplay(display);
              *error_flag = 1;
              break;
          case Expose:          /* portion of window has become visible */
              if(event.xexpose.count == 0) {
                expose_flag = 1;
              }
              *expose_flag_2 = 1;
              break;
          case ConfigureNotify: /* window has been reconfigured */
              configure_flag = 1;
              break;
          default:
              break;
          }                     /* end switch */
     }                          /* end while */
}

/* XRDLOC  - routine to read a position from the graphics window (used
 *           by the Dataplot cross-hair command).  The mouse position at
 *           the next mouse click will be determined.
 *
 *           This routine will wait until a mouse button is pressed.
 *
 *           Note that expose and configure events will still be collected.
 *           Also, the pointer motion events are included for future
 *           development.  Currently, the code is commented out
 *           (although it should valid).  It will be activated if Dataplot
 *           develops a need for it later.  Dataplot's cross-hair simply
 *           wants a single coordinate position when any of the mouse buttons
 *           is pressed.
 *
 * ixret   - x coordinate of mouse position when button pressed
 * iyret   - y coordinate of mouse position when button pressed
 * error   - error flag (for window destroy event)
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xrdloc_(ixret, iyret, error)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XRDLOC_(ixret, iyret, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xrdloc(ixret, iyret, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XRDLOC(ixret, iyret, error)
#endif
int  *ixret, *iyret, *error;
{
     XEvent   event;            /* holds Xserver events */
     int      x, y;             /* the last X pointer position */
     int      new_x, new_y;     /* a new X pointer position */
     unsigned int dummy;        /* placeholder for unwanted return value */
     int      done;

     XSelectInput(display, window,    /* Types of graphic input to accept */
       StructureNotifyMask |          /* Window notification events */
       ExposureMask |                 /* Expose events */
       ButtonPressMask |              /* Button press */
       ButtonMotionMask |             /* Pointer moves with button pressed */
       PointerMotionMask);            /* Pointer moves in window */

       *ixret = -1;
       *iyret = -1;
       *error = 0;
       done = 0;
       while (done == 0) {               /* loop until button press event */

          XNextEvent(display, &event);   /* get next event from queue */
          switch (event.type) {
          case DestroyNotify:            /* window has been destroyed */
              XFreeGC(display, gc);
              XDestroyWindow(display,window);
              XCloseDisplay(display);
              *error = 1;
              done = 1;
              break;
          case Expose:                   /* expose event */
              if(event.xexpose.count == 0) {
                expose_flag = 1;
              }
              break;
          case ConfigureNotify:          /* window has been reconfigured */
              configure_flag = 1;
              break;
          case MotionNotify:             /* pointer motion */
          /*  XQueryPointer(display, window,
                            &dummy, &dummy,
                            &dummy, &dummy,
                            &new_x, &new_y,
                            &mask);
              x = new_x;
              y = new_y;    */
              break;
          case ButtonPress:     /* Button event */
              *ixret = event.xbutton.x;
              *iyret = event.xbutton.y;
              done = 1;
              break;
          default:
              break;
          }       /* end switch */
       }          /* end while */

       XSelectInput(display, window,    /* Reset default event selection */
         StructureNotifyMask |          /* Window notification events */
         ExposureMask);                 /* Expose events */

}

/* XINIT  - routine to initialize X11.
 *
 * xp          - suggested width of graphics window in pixels (0 for default)
 * yp          - suggested height of graphics window in pixels (0 for default)
 * or          - 0 for landscape, 1 for portrait, 2 for use pixel sizes
 *               3 for square
 * ixret       - acutal width returned by window manager (as opposed to
 *               suggested width sent to window manager)
 * iyret       - actual height returned by window manager (as opposed to
 *               suggested height sent to window manager)
 * display_name- display name (NULL means use the default)
 * error_flag  - following error codes:
 *               0 - normal
 *               1 - unable to open display connection
 *               2 - X11 already open
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xinit_(xp, yp, or, ixret, iyret, display_name, error_flag)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XINIT_(xp, yp, or, ixret, iyret, display_name, error_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xinit(xp, yp, or, ixret, iyret, display_name, error_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XINIT(xp, yp, or, ixret, iyret, display_name, error_flag)
#endif
int    *xp, *yp, *or;
int    *error_flag, *ixret, *iyret;
int    display_name[];
{
     int          xpixels, ypixels, orien;
     int          temp, i;
     int          class;
     int          bits_per_rgb;
     float        atemp;
     Status       result;
     int          len, itest;
     char         *display_string;

     *error_flag = 0;
     xpixels = *xp;
     ypixels = *yp;
     orien = *or;
     if(OPEN_FLAG != 0) {                         /* X11 already open */
       *error_flag = 2;
       return;
     }

     display_string = "                                                                  " ;
     i_to_s(display_name,display_string,80,&len);
     itest = strncmp(display_string, "DEFAULT", 7);
     if (itest == 0) {          /* default display name */
       if((display = XOpenDisplay(NULL)) == NULL) { /*open display connection*/
         *error_flag = 1;
         OPEN_FLAG = 0;
         return;
        }
     }
     else {           /* user specified display name */
       if((display = XOpenDisplay(display_string)) == NULL) {
         *error_flag = 1;
         OPEN_FLAG = 0;
         return;
        }
     }

     OPEN_FLAG = 1;
     screen = DefaultScreen(display);        /* Set default screen */
     white = WhitePixel(display,screen);     /* Set white */
     black = BlackPixel(display,screen);     /* Set black */

     XGetGeometry(                           /* Get root window geometry */
                  display, DefaultRootWindow(display),
                  &root_id,                  /* root id */
                  &root_x, &root_y,          /* root position */
                  &root_width, &root_height, /* root width and height */
                  &root_bw, &root_depth);    /* root border width and depth */

     set_screen(xpixels, ypixels, orien);
     ORIENTATION_CURRENT = orien;

     window = XCreateSimpleWindow(           /* Open graphics window */
                display,                     /* Pointer to Display structure */
                DefaultRootWindow(display),  /* Parent for new window */
                xsh.x,                       /* X position of window */
                xsh.y,                       /* Y position of window */
                xsh.width,                   /* Width of window */
                xsh.height,                  /* Height of window */
                BORDER_WIDTH,                /* Pixel width of border */
                black,                       /* Border color */
                white);                      /* Background color */

     /* Set the bit gravity to NorthWest.  This means that if the
      * window is resized or repositioned by the window manager, the
      * window will be redrawn from top left corner.  This means we
      * can use current scale units until next page erase (i.e., do not
      * change units until start a new graph).  This is best solution
      * since Dataplot is vector rather than raster oriented, so not
      * possible to redraw what was previously drawn.  Transformed window
      * may not use full window or it may clip current graph, but should
      * otherwise be OK.  Next erase page will start drawing in new units.
      */
     window_attributes.bit_gravity = NorthWestGravity; /* Set bit gravity */
     value_mask = CWBitGravity;
     XChangeWindowAttributes(display, window, value_mask,
         &window_attributes);

     XSetStandardProperties(display, window, "Dataplot", "Dataplot",
       None, argv, argc, &xsh);
     XSetWMHints(display, window, &xwmh);
     XSetClassHint(display, window, &xch);

     XSelectInput(display, window,    /* Types of graphic input to accept */
       StructureNotifyMask |          /* Window notification events */
       ExposureMask );                /* Expose events */

    XMapWindow(display, window);      /* Map the window (make visible) */
    XFlush(display);                  /* Flush the buffer */

    /* Get actual height and width set by the window manager */
    XGetWindowAttributes(display, window, &returned_attributes);
    width = returned_attributes.width;
    height = returned_attributes.height;
    *ixret = width;
    *iyret = height;

    do {
        XNextEvent(display, &event);
    } while (event.type != MapNotify || event.xmap.window != window);

    gc = XCreateGC(display, window, 0, &gcvalues);/* Create graphics context */
    XSetState(display,                /* the current display */
      gc,                             /* the current graphics context */
      black,                          /* set default foreground color */
      white,                          /* set default background color */
      GXcopy,                         /* set default to "overwrite" */
      AllPlanes);
    XSetLineAttributes(display, gc,   /* current display , graphics context */
      0,                              /* default line width */
      LineSolid,                      /* default soild line */
      CapButt,                        /* default end-cap style */
      JoinMiter);                     /* default line join style */
    WIDTH_CURRENT = 0;
    LINE_STYLE_CURRENT = 0;
    CAP_STYLE_CURRENT = 0;
    JOIN_STYLE_CURRENT = 0;

    FONT_NAME_CURRENT = FONT_NULL;
    PIXMAP_CURRENT = 0;
    pixmap_flag = 0;

    num_cells = XDisplayCells(display,screen);    /* Color inquiry calls */
    num_planes = XDisplayPlanes(display, screen);
    color_map = XDefaultColormap(display, screen);
    vis = XDefaultVisual(display, screen);
    class = vis->class;
    bits_per_rgb = vis->bits_per_rgb;
    depth = XDefaultDepth(display, screen);


    if (depth == 1) {                 /* one-plane monochrome */
      color_flag = 0;
    }
    else if (class == PseudoColor) {  /* multi-plane color */
      color_flag = 2;
    }
    else if (class == GrayScale)   {  /* multi-plane monochrome */
       color_flag = 1;
    }
    else if (class == StaticGray)  {  /* multi-plane monochrome, unchangeable
                                         color map */
       color_flag = 1;
    }
    else if (class == DirectColor) {  /* direct color */
       color_flag = 2;
    }
    else if (class == TrueColor)   {  /* direct color, unchangeable color map*/
       color_flag = 2;
    }
    else if (class == StaticColor) {  /* multi-plane color, unchangeable
                                         color map */
       color_flag = 2;
    }
    else {
       color_flag = 0;
    }

/* June, 1991.  Switch from specifying colors by specific RGB values
 * to using the named colors (as defined by the R3 Color Database).
 * This avoids the problem of running out of color cell entries
 * (occured on a PC implementation).  In addition, all implementations
 * at R3 or higher should support all the colors in the database 
 * (or at least map the color to the closest possible alternative
 * available on the particular workstation).  It has the drawback
 * that RGB values cannot be specified exactly, but this is not
 * not really a problem since most of the colors we defined for
 * Dataplot are defined in the database.  Release 4 and some       
 * implementations of X11 support a broader database.  We will limit
 * it to more conservative one since this should be supported on
 * all implementations (R3 or higher).  The global variable COLOR_TYPE
 * is used to control whether the old method or the new method is used.
 * A possible future enhancement might be to allow a user-specified
 * color map based on RGB values.  This would work better with the
 * old implementation.
 */
    max_colors = 0;
    if (color_flag > 1) {  /* define colors */
       max_colors = MAX_COLORS;
#if COLOR_TYPE
       for (i=0; i < MAX_COLORS; i++) {
           color_list[i] = 1;
           if (!XAllocNamedColor (display, color_map, color_names[i], 
               &exact_def, &colors[i])) {
             color_list[i] = 0;
           } 
        } 
#else
      colors[0].red = 0;   /* define black */
      colors[0].green = 0;
      colors[0].blue = 0;

      colors[1].red = 65535; /* define white */
      colors[1].green = 65535;
      colors[1].blue = 65535;

      colors[2].red = 0; /* define green */
      colors[2].green = 40000;
      colors[2].blue = 0;

      colors[3].red = 65535; /* define yellow */
      colors[3].green = 65535;
      colors[3].blue = 0;

      colors[4].red = 65535;   /* define red */
      colors[4].green = 0;
      colors[4].blue = 0;

      colors[5].red = 0;   /* define blue */
      colors[5].green = 0;
      colors[5].blue = 65535;

      colors[6].red = 65535;   /* define magenta */
      colors[6].green = 0;
      colors[6].blue = 65535;

      colors[7].red = 0; /* define cyan */
      colors[7].green = 65535;
      colors[7].blue = 65535;

      colors[8].red = 65535; /* define Yellow Orange  (or Orange) */
      colors[8].green = 32767;
      colors[8].blue = 0;

      colors[9].red = 32767; /* define Yellow Green */
      colors[9].green = 65535;
      colors[9].blue = 0;

      colors[10].red = 0; /* define Blue Green */
      colors[10].green = 65535;
      colors[10].blue = 32767;

      colors[11].red = 0; /* define Green Blue */
      colors[11].green = 32767;
      colors[11].blue = 65535;

      colors[12].red = 32767; /* define Blue Violet (or purple) */
      colors[12].green = 0;
      colors[12].blue = 65535;

      colors[13].red = 65535; /* define Red violet */
      colors[13].green = 0;
      colors[13].blue = 32767;

      colors[14].red = 21845; /* define Dark Grey */
      colors[14].green = 21845;
      colors[14].blue = 21845;

      colors[15].red = 43690; /* Define Light Grey */
      colors[15].green = 43690;
      colors[15].blue = 43690;

      for (i = 0; i < 15; i++) {   /* allocate the 16 colors */
         color_list[i] = 1;
         if (! XAllocColor(display, color_map, &colors[i])) {
           color_list[i] = 0;      /* color allocation failed */
         }
      }
#endif

    }
    BACKGROUND_CURRENT = -1;
    COLOR_CURRENT = -1;
    temp = 0;
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
    xfore_(&temp);
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
    XFORE_(&temp);
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
    xfore(&temp);
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
    XFORE(&temp);
#endif
    BACKGROUND_CURRENT = 1;

}

/* XERASE  - routine to clear the screen.  Check to see if the window
 *           configuration has been changed by the window manager (or
 *           by the user).  If so, reset the screen size.  Resizes
 *           done by the window manager will take precedence over resizes
 *           requested by the calling program (assume user resized window
 *           via the window manager).
 *
 *           Clear the pixmap as well as the graphics window.
 *
 *           Note that this routine assumes that XFLUSH has already
 *           been called to flush the buffer and that XCHECK has been
 *           called to check for expose and configure events.
 *
 *  xpixels   - width (in pixels) for graphics window
 *  ypixels   - height (in pixels) for graphics window
 *  orien     - window orientation (i.e., landscape, portrait, or none)
 *  ixret     - width returned to calling program
 *  iyret     - height returned to calling program
 *  back_col  - background color
 *  pix_flag  - 0 = no pixmap generated, 1 = pixmap generated
 *
 */

#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xerase_(xpixels, ypixels, orien, ixret, iyret, back_col, pix_flag)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XERASE_(xpixels, ypixels, orien, ixret, iyret, back_col, pix_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xerase(xpixels, ypixels, orien, ixret, iyret, back_col, pix_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XERASE(xpixels, ypixels, orien, ixret, iyret, back_col, pix_flag)
#endif
int  *xpixels, *ypixels, *orien, *back_col, *pix_flag;
int  *ixret, *iyret;
{

   int   temp_color, temp_color_2;

   if (configure_flag == 1) {    /* graphics window re-configured by user */
      /* Get actual height and width set by the window manager */
      XGetWindowAttributes(display, window, &returned_attributes);
      width = returned_attributes.width;
      height = returned_attributes.height;
      *ixret = width;
      *iyret = height;
   }
   else if (    /* calling program changing width or height of window */
            ((*xpixels > 0) && (*ypixels > 0) &&
            (*xpixels != width || *ypixels != height)) ||
            (*orien != ORIENTATION_CURRENT)) {
      set_screen(*xpixels, *ypixels, *orien);
      ORIENTATION_CURRENT = *orien;
      XUnmapWindow(display,window);
      XResizeWindow(display, window, xsh.width, xsh.height);

      XMapWindow(display, window);
      do {     /* make sure correct window is in place */
          XNextEvent(display, &event);
      } while (event.type != MapNotify || event.xmap.window != window);
      expose_flag = 0;
      configure_flag = 0;

      /* Get actual height and width set by the window manager */
      XGetWindowAttributes(display, window, &returned_attributes);
      width = returned_attributes.width;
      height = returned_attributes.height;
      *ixret = width;
      *iyret = height;

   }
   else {
     *ixret = width;
     *iyret = height;
   }
   configure_flag = 0;

   switch (*pix_flag) {
      case 0:                         /* user does not want pixmap */
          if (PIXMAP_CURRENT != 0) {  /* get rid of current pixmap */
            PIXMAP_CURRENT = 0;
            XFreePixmap(display, pixmap);
          }
          break;
      case 1:                          /* user wants a pixmap */
          if (PIXMAP_CURRENT != 0) {   /* pixmap already exists */
            XFreePixmap(display, pixmap);
          }
          else {                      /* pixmap does not yet exist */
            PIXMAP_CURRENT = 1;
          }
          pixmap = XCreatePixmap(display, window, width, height, depth);
          break;
      default:
          break;
   }

   xback(*back_col);                         /* set background color */
   XClearWindow(display, window);            /* clear the window */
   temp_color = BACKGROUND_CURRENT;
   temp_color_2 = COLOR_CURRENT;
   BACKGROUND_CURRENT = -1;
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
   xfore_(&temp_color);
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
   XFORE_(&temp_color);
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
   xfore(&temp_color);
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
   XFORE(&temp_color);
#endif
   XFillRectangle(display, window, gc, 0, 0, width, height);
/* January 1992.  Follwoing 3 lines added.  */
   if (PIXMAP_CURRENT == 1) {                /* set pixmap to background */
     XFillRectangle(display, pixmap, gc, 0, 0, width, height);
   }
/* End change.  */
   BACKGROUND_CURRENT = temp_color;
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
   xfore_(&temp_color_2);
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
   XFORE_(&temp_color_2);
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
   xfore(&temp_color_2);
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
   XFORE(&temp_color_2);
#endif
/* January 1992.  Following 5 lines commented out */
/* if (PIXMAP_CURRENT == 1) {   */             /* set pixmap to background */
/*   XSetForeground(display, gc, colors[BACKGROUND_CURRENT].pixel); */
/*   XFillRectangle(display, pixmap, gc, 0, 0, width, height);      */
/*   XSetForeground(display, gc, colors[COLOR_CURRENT].pixel);      */
/* }  */
/* End change.  */

}

/* XUPDAT - routine to copy pixmap to screen
 *
 * If an expose event has been detected, update the graphics
 * window with the pixmap (if calling program has requested it).
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xupdat_()
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XUPDAT_()
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xupdat()
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XUPDAT()
#endif
{

   if (expose_flag == 1 &&         /* update screen  if expose event */
       PIXMAP_CURRENT == 1) {      /* copy pixmap to screen to update */
        XCopyArea(display, pixmap, window, gc, 0, 0, width, height, 0, 0);
        XFlush(display);           /* do copy immediately */
   }

   expose_flag = 0;

}

/* XCLEAR  - routine to flush the buffer
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xclear_()
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XCLEAR_()
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xclear()
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XCLEAR()
#endif
{
   XFlush(display);
}

/* XEND   - routine to end X11.  Close the display.
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xend_()
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XEND_()
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xend()
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XEND()
#endif

{
     if (OPEN_FLAG != 0) {
        XFreeGC(display,gc);
        XDestroyWindow(display,window);
        XCloseDisplay(display);
     }
     OPEN_FLAG = 0;
}

/* XFORE  - routine to set the foreground color.  Specify the color as
 *          an index.
 *       INDEX   COLOR_TYPE = 0        COLOR_TYPE = 1
 *       =====   ==============        ==============
 *           0 - black                 black
 *           1 - white                 white
 *           2 - green                 green
 *           3 - yellow                yellow
 *           4 - red                   red
 *           5 - blue                  blue
 *           6 - magenta               magenta
 *           7 - cyan                  cyan
 *           8 - yellow orange         orange
 *           9 - yellow green          yellow green
 *          10 - blue green            dark green
 *          11 - green blue            light blue
 *          12 - blue violet           blue violet
 *          13 - red violet            violet red
 *          14 - dark grey             dark slate gray
 *          15 - light grey            light gray
 *          16 -                       aquamarine
 *          17 -                       brown
 *          18 -                       cadet blue
 *          19 -                       coral
 *          20 -                       cornflower blue
 *          21 -                       dark olive green
 *          22 -                       dark orchid
 *          23 -                       dark slate blue
 *          24 -                       dark turquoise
 *          25 -                       firebrick       
 *          26 -                       forest green 
 *          27 -                       gold
 *          28 -                       goldenrod
 *          29 -                       gray
 *          30 -                       indian red 
 *          31 -                       khaki
 *          32 -                       dim gray   
 *          33 -                       light blue steel 
 *          34 -                       lime green
 *          35 -                       maroon
 *          36 -                       medium aquamarine 
 *          37 -                       medium blue
 *          38 -                       medium forest green
 *          39 -                       medium goldenrod
 *          40 -                       medium orchid
 *          41 -                       medium sea green
 *          42 -                       medium slate blue
 *          43 -                       medium spring green
 *          44 -                       medium turquoise
 *          45 -                       medium violet red
 *          46 -                       midnight blue
 *          47 -                       navy
 *          48 -                       orange red
 *          49 -                       orchid
 *          50 -                       pale green 
 *          51 -                       pink 
 *          52 -                       plum
 *          53 -                       purple
 *          54 -                       salmon
 *          55 -                       sea green 
 *          56 -                       sienna
 *          57 -                       sky blue 
 *          58 -                       slate blue
 *          59 -                       spring green
 *          60 -                       steel blue 
 *          61 -                       tan  
 *          62 -                       thistle    
 *          63 -                       turquoise
 *          64 -                       violet
 *          65 -                       wheat 
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xfore_(index)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XFORE_(index)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xfore(index)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XFORE(index)
#endif
int   *index;
{
      unsigned long  temp;
      int            temp2;

      if (*index == COLOR_CURRENT) return;
      COLOR_CURRENT = *index;

      if (*index == BACKGROUND_CURRENT) {    /* test if equal background */
        if (*index == 0) {                   /* set to white */
          COLOR_CURRENT = 1;
          XSetForeground(display, gc, white);
        }
        else {                               /* set to black */
          COLOR_CURRENT = 0;
          XSetForeground(display, gc, black);
        }
        return;
      }

      if (color_flag < 2) {   /* monochrome devices */
        switch (*index) {
           case 0:       /* black foreground color */
               temp = black;
               COLOR_CURRENT = 0;
               break;
           case 1:       /* white foreground color */
               temp = white;
               COLOR_CURRENT = 1;
               break;
           default:      /* default, set to black */
               temp = black;
               COLOR_CURRENT = 0;
               break;
         }
         XSetForeground(display, gc, temp);
       }
       else {          /* color devices */
         temp2 = *index;
         if (temp2 < 0) temp2 = 1;
         if (temp2 > max_colors - 1) {    /* index out of range */
           if (BACKGROUND_CURRENT != 0) {  /* set to black */
             temp2 = 0;
           }
           else {                          /* set to white */
             temp2 = 1;
           }
         }
         else if (color_list[temp2] == 0) { /* color unavailable */
           if (BACKGROUND_CURRENT != 0) {  /* set to black */
             temp2 = 0;
           }
           else {                          /* set to white */
             temp2 = 1;
           }
         }
         COLOR_CURRENT = temp2;
         XSetForeground(display, gc, colors[temp2].pixel);
       }

}


/* xback  - routine to set the background color.  Specify the color as
 *          an index.  See comments for xfore routine above for the 
 *          index to color mapping.
 *
 * Note: the background color only applies in the following 4 cases:
 *          XDrawImageString
 *          XCopyPlane
 *          drawing lines with line style LineDoubleDash
 *          filling with fill style of FillOpaqueStippled
 *       Although Dataplot does not currently use any of these features,
 *       the background value will be set (in case any of these are
 *       implemented at a future date).  However, the background will
 *       be set by doing a solid fill rectangle of the entire screen
 *       (setting the foreground color to the background color).
 *
 */
void xback(index)
int  index;
{

      unsigned long temp;
      int           temp2;

      if (index == BACKGROUND_CURRENT) return;

      if (color_flag < 2) {   /* monochrome devices */
        temp2 = index;
        if (temp2 < 0 || temp2 > 1) temp2 = 2;
        switch (temp2) {
           case 0:       /* black background color */
               temp = black;
               BACKGROUND_CURRENT = 0;
               break;
           case 1:       /* white background color */
               temp = white;
               BACKGROUND_CURRENT = 1;
               break;
           case 2:
               temp = white;
               BACKGROUND_CURRENT = 1;
               break;
           default:      /* default, set to white */
               temp = white;
               BACKGROUND_CURRENT = 1;
               break;
         }
         XSetBackground(display, gc, temp);
       }
       else {          /* color devices */
         temp2 = index;
         if (temp2 < 0) temp2 = 1;
         if (temp2 > max_colors - 1) {    /* index out of range */ 
           if (BACKGROUND_CURRENT != 1) {  /* set to white */
             temp2 = 1;
             BACKGROUND_CURRENT = 1;
           }
           else {                          /* set to black */
             temp2 = 0;
             BACKGROUND_CURRENT = 0;
           }
         }
         else if (color_list[temp2] == 0) { /* color unavailable */
           if (BACKGROUND_CURRENT != 1) {  /* set to white */
             temp2 = 1;
             BACKGROUND_CURRENT = 1;
           }
           else {                          /* set to black */
             temp2 = 0;
             BACKGROUND_CURRENT = 0;
           }
         }
         BACKGROUND_CURRENT = temp2;
         XSetBackground(display, gc, colors[temp2].pixel);
       }
}

/* XLATTR  - set line attributes.  Note that the attribute will only be
 *           set if it is being changed (i.e., test against current
 *           value of the attribute).
 *
 * index - parameter that sets the value
 * icode - identify which attribute to set
 *         1 - set the line width in pixels
 *         2 - set the line style (i.e., solid or dash).  Currently only
 *             three dash patterns are supported.  However, additional ones
 *             may be added.
 *             0 - solid line
 *             1 - dash line
 *             2 - dotted line
 *             3 - dot-dash line (dash2 pattern)
 *             4 - dash3
 *             5 - dash4
 *         3 - set the line cap
 *             0 - cap butt (end-caps squared off at endoints perpindicular
 *                 to the slope of the line)
 *             1-  cap not last (similar to cap butt, but for 1 pixel wide
 *                 line, do not draw last point of line)
 *             2 - cap round (end-caps are circles with diameter equal to
 *                 line width)
 *             3 - cap projecting (end caps are squared off similar to cap
 *                 butt, however they project half the line width beyond
 *                 the end points)
 *         4 - set the line join
 *             0 - miter join (outer edges of wide line extended so that
 *                 they meet at same angle as would narow lines)
 *             1 - round join (corners are rounded off using a circle of
 *                 diameter equal to line width centered at join point)
 *             2 - bevel join (intersecting endpoints of lines both drawn
 *                 as if they were end points with cap butt style.  The
 *                 small triangle created is filled).
 *
 */
#define MAX_WIDTH  15
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xlattr_(index, icode)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XLATTR_(index, icode)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xlattr(index, icode)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XLATTR(index, icode)
#endif
int    *index, *icode;
{
unsigned long  valuemask;
int            dash_offset;
int            width_temp;
int            temp;
static char    dot [] = {1,2};
static char    dash [] = {6,3};
static char    dash3 [] = {3,3};
static char    dash4 [] = {2,4};
static char    dash_dot [] = {9,3,3,3};

   switch (*icode) {
      case 1:         /* set the line width */
          if (*index == WIDTH_CURRENT) break;
          valuemask = GCLineWidth;
          if (*index < 2) {
             gcvalues.line_width = 0;
          }
          else {
             width_temp = *index;
             if (width_temp > MAX_WIDTH) width_temp = MAX_WIDTH;
             gcvalues.line_width = width_temp;
          }
          XChangeGC(display, gc, valuemask, &gcvalues);
          WIDTH_CURRENT = *index;
          break;
      case 2:         /* set the line style */
          if (*index == LINE_STYLE_CURRENT) break;
          valuemask = GCLineStyle;
          dash_offset = 0;
          temp = *index;
          if (temp > 3) temp = 3;
          switch (temp) {    /* index determines the style */
             case 0:         /* solid line */
                 gcvalues.line_style = LineSolid;
                 break;
             case 1:         /* dashed line */
                 gcvalues.line_style = LineOnOffDash;
                 XSetDashes(display, gc, dash_offset, dash, 2);
                 break;
             case 2:         /* dotted line */
                 gcvalues.line_style = LineOnOffDash;
                 XSetDashes(display, gc, dash_offset, dot, 2);
                 break;
             case 3:         /* dash-dot line */
                 gcvalues.line_style = LineOnOffDash;
                 XSetDashes(display, gc, dash_offset, dash_dot, 4);
                 break;
             case 4:         /* dash3 */
                 gcvalues.line_style = LineOnOffDash;
                 XSetDashes(display, gc, dash_offset, dash3, 2);
                 break;
             case 5:         /* dash4 line */
                 gcvalues.line_style = LineOnOffDash;
                 XSetDashes(display, gc, dash_offset, dash4, 2);
                 break;
              default:
                 gcvalues.line_style = LineSolid;
                 break;
          }
          XChangeGC(display, gc, valuemask, &gcvalues);
          LINE_STYLE_CURRENT = *index;
          break;
      case 3:         /* set the line cap style */
          if (*index == CAP_STYLE_CURRENT) break;
          valuemask = GCCapStyle;
          switch (*index) {  /* index determines the style */
             case 0:         /* cap butt */
                 gcvalues.cap_style = CapButt;
                 break;
             case 1:         /* cap round */
                 gcvalues.cap_style = CapRound;
                 break;
             case 2:         /* cap not last */
                 gcvalues.cap_style = CapNotLast;
                 break;
             case 3:         /* cap projecting */
                 gcvalues.cap_style = CapProjecting;
                 break;
             default:
                 gcvalues.cap_style = CapButt;
                 break;
          }
          XChangeGC(display, gc, valuemask, &gcvalues);
          CAP_STYLE_CURRENT = *index;
          break;
      case 4:         /* set the join style */
          if (*index == JOIN_STYLE_CURRENT) break;
          valuemask = GCJoinStyle;
          switch (*index) {  /* index determines the style */
             case 0:         /* miter join */
                 gcvalues.join_style = JoinMiter;
                 break;
             case 1:         /* round join */
                 gcvalues.join_style = JoinRound;
                 break;
             case 2:         /* bevel join */
                 gcvalues.join_style = JoinBevel;
                 break;
             default:
                 gcvalues.join_style = JoinMiter;
                 break;
          }
          XChangeGC(display, gc, valuemask, &gcvalues);
          JOIN_STYLE_CURRENT = *index;
          break;
      default:
          break;
   }
}

/* XDRAW  - draw a polyline.  The attributes of the line have been
 *          previously set (by the XLATTR routine).
 *
 * xpts   - contains the x coordinates
 * ypts   - contains the y coordinates
 * npts   - the number of points to plot
 *
 */
#define MAX_LINE_POINTS  500
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xdraw_(xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XDRAW_(xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xdraw(xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XDRAW(xpts, ypts, npts)
#endif
int   xpts[], ypts[];
int   *npts;
{
   XPoint  points[MAX_LINE_POINTS];

   if (*npts == 2) {      /* draw 2 points */
      int  x1, y1, x2, y2;
      x1 = xpts[0];
      x2 = xpts[1];
      y1 = ypts[0];
      y2 = ypts[1];
      XDrawLine(display, window, gc, x1, y1, x2, y2);
      if (PIXMAP_CURRENT == 1) {
        XDrawLine(display, pixmap, gc, x1, y1, x2, y2);
      }
      return;
   }
   else if (*npts > 2) {     /* more than 2 points */
     int  i, ilower, iupper, nloops, temp, itimes;
     iupper = 0;
     nloops = 0;
     while (nloops != 1) {  /* do MAX_LINE_POINTS per loop */
        ilower = iupper;
        iupper = ilower + MAX_LINE_POINTS - 1;
        if (iupper > *npts) {
          iupper = *npts;
          nloops = 1;
        }
        temp = 0;
        itimes = iupper - ilower;
        for (i = 0; i < itimes; i++) {
           points[i].x = xpts[i + ilower];
           points[i].y = ypts[i + ilower];
           temp = temp + 1;
        }
        XDrawLines(display, window, gc, points, temp, CoordModeOrigin);
        if (PIXMAP_CURRENT == 1) {
          XDrawLines(display, pixmap, gc, points, temp, CoordModeOrigin);
        }
     }  /* end while loop */
   }    /* end of npts loop */

}

/* XPOINT - draw a point.
 *          previously set (by the XLATTR routine).
 *
 * ix     - contains the x coordinate
 * iy     - contains the y coordinate
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xpoint_(ix, iy)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XPOINT_(ix, iy)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xpoint(ix, iy)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XPOINT(ix, iy)
#endif
int   *ix, *iy;
{
   XDrawPoint(display, window, gc, *ix, *iy);
   if (PIXMAP_CURRENT == 1) {
     XDrawPoint(display, pixmap, gc, *ix, *iy);
   }

}

/* XCIRC  - draw a circle.  Note that the circle may be either filled or 
 *          unfilled.  For a filled circle, do twice, (once for the 
 *          outline, once to fill the interior).
 *
 * ix     - contains the x coordinate for the center of the circle
 * iy     - contains the y coordinate for the center of the circle 
 * irad   - radius
 * ifill  - 0 for unfilled circle, 1 for filled circle
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xcirc_(ix, iy, irad, ifill)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XCIRC_(ix, iy, irad, ifill)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xcirc(ix, iy, irad, ifill)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XCIRC(ix, iy, irad, ifill)
#endif
int   *ix, *iy, *irad, *ifill;
{

   int   xpos, ypos, iwidth, iheight, iang1, iang2, ir;

   iang1 = 0;
   iang2 = 23040;
   ir = *irad;
   xpos = *ix - *irad;
   ypos = *iy - *irad;
   iwidth = 2 * ir;
   iheight = 2 * ir;
   XDrawArc(display, window, gc, xpos, ypos, iwidth, iheight, iang1, iang2);
   if (PIXMAP_CURRENT == 1) {
     XDrawArc(display, pixmap, gc, xpos, ypos, iwidth, iheight, iang1, iang2);
   }
   if (*ifill != 0) {
     XFillArc(display, window, gc, xpos, ypos, iwidth, iheight, iang1, iang2);
     if (PIXMAP_CURRENT == 1) {
       XDrawArc(display, pixmap, gc, xpos, ypos, iwidth, iheight, iang1, iang2);
     }
   }

}

/* XREGFL - fill a region.  Rectangular regions will be filled differently
 *          non-rectangular regions.  Dataplot only handles convex polygons,
 *          so set this (for faster performance).  This routine only does
 *          solid fills.  Hatch patterns must be drawn
 *          by the calling program (i.e., send the individual lines to
 *          the XDRAW routine).
 *
 * xpts   - contains the x coordinates
 * ypts   - contains the y coordinates
 * npts   - the number of points in the polygon (if 2, assume a rectangle,
 *          otherwise, a convex polygon)
 *
 */
#define MAX_REG_POINTS  100
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xregfl_(xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XREGFL_(xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xregfl(xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XREGFL(xpts, ypts, npts)
#endif
int   xpts[], ypts[];
int   *npts;
{
   XPoint  points[MAX_REG_POINTS];

   if (*npts == 2) {      /* rectangle */
      int  x1, y1, x2, y2, rect_height, rect_width;
      if (xpts[0] <= xpts[1]) {
        x1 = xpts[0];
        x2 = xpts[1];
        rect_width = x2 - x1 + 1;
      }
      else {
        x1 = xpts[1];
        x2 = xpts[0];
        rect_width = x2 - x1 + 1;
      }
      if (ypts[0] <= ypts[1]) {
        y1 = ypts[0];
        y2 = ypts[1];
        rect_height = y2 - y1 + 1;
      }
      else {
        y1 = ypts[1];
        y2 = ypts[0];
        rect_height = y2 - y1 + 1;
      }
      XFillRectangle(display, window, gc, x1, y1, rect_width, rect_height);
      if (PIXMAP_CURRENT == 1) {
        XFillRectangle(display, pixmap, gc, x1, y1, rect_width, rect_height);
      }
   }
   else if (*npts > 2) {     /* convex polygon */
      int  i, temp;
      temp = *npts;
      if( *npts > MAX_REG_POINTS) temp = MAX_REG_POINTS;
      for (i = 0; i < temp; i++) {
         points[i].x = xpts[i];
         points[i].y = ypts[i];
      }
      XFillPolygon(display,window,gc,points,temp,Convex,CoordModeOrigin);
      if (PIXMAP_CURRENT == 1) {
        XFillPolygon(display,pixmap,gc,points,temp,Convex,CoordModeOrigin);
      }
   }

}
/* XTATTR - set the font for drawing text strings.  Note that the fonts
 *          available on a particular workstation can vary, so a test
 *          is made to see if the font exists.  If not, the 8x13 font
 *          will be used (this should be available on all X
 *          workstations.
 *
 *          It is the calling programs responsibility to provide a
 *          valid font name.  This routine does no checking for valid
 *          names since it is possible for local sites to have their own
 *          predefined font names.  The calling program should be sure
 *          to end the font name string with a null (i.e., ascii 0).
 *
 *          In order to avoid excessive reloading of the same font, the
 *          current font name is stored.  A change is made only if the
 *          requested font differs from the current font.
 *
 * font        - name of font
 * error       - 0 - valid font,
 *               1 - invalid font, leave current font in place
 *               2 - invalid font, set default
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xtattr_(font, error)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XTATTR_(font, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xtattr(font, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XTATTR(font, error)
#endif
int    font[];
int    *error;
{

   int          itest;          /* temporary variables */
   int          len;            /* number of characters in font name */
   char         *font_name;     /* converted font name */
   int          i;
   int          direction_hint, font_ascent, font_descent, font_gap;
   XCharStruct  overall;

   font_name = "                                                             ";
   i_to_s(font, font_name, 80, &len);

   /* Check current font against requested font */
   itest = strcmp(font_name, FONT_NAME_CURRENT);
   if (itest != 0) {
     if (FONT_NAME_CURRENT != FONT_NULL) {   /* free default font */
       XFreeFont(display, font_struct);
     }
     font_struct = XLoadQueryFont (display, font_name);
     if (font_struct == 0) {
       if (FONT_NAME_CURRENT == FONT_NULL) {  /* load default font */
          /* font_name = "8X13";  */
          font_name = FONT_NAME_DEFAULT; 
         *error = 2;
       }
       else {                              /* leave current font */
         strcpy(font_name, FONT_NAME_CURRENT);
         *error = 1;
       }
       font_struct = XLoadQueryFont(display, font_name);
     }
     strcpy(FONT_NAME_CURRENT, font_name);
     XSetFont(display, gc, font_struct->fid);  /* Set the font */
     XTextExtents(font_struct, " ", 0, &direction_hint, &font_ascent,
          &font_descent, &overall);
     font_gap = (font_ascent + font_descent) * .20;
     FONT_HEIGHT_CURRENT = font_ascent + font_descent;
     FONT_GAP_CURRENT = font_gap;
   }
}

/* XTEXTH - draw a horizontal text string.
 *
 *          Use XDrawString rather than XDrawText so that background pixels
 *          are not changed.  This is so that other lines that may intersect
 *          the character box will not be blanked out.
 *
 * string - text string to draw
 * ixpos  - x position
 * iypos  - y position
 * ijusth - justification (horizontal)
 *          0 - left justified
 *          1 - center justified
 *          2 - right justified
 * ijustv - justiciation (vertical)
 *          0 - center justified
 *          1 - bottom justified
 *          2 - top justified
 * error  - error flag
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xtexth_(string, ixpos, iypos, ijusth, ijustv, error)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XTEXTH_(string, ixpos, iypos, ijusth, ijustv, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xtexth(string, ixpos, iypos, ijusth, ijustv, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XTEXTH(string, ixpos, iypos, ijusth, ijustv, error)
#endif
int    string[];
int    *ixpos, *iypos, *ijusth, *ijustv, *error;
{

   int          itest, itempx, itempy;   /* temporary variables */
   int          len;                     /* number of characters in string */
   int          string_width;            /* width of string in pixels */
   char         string2[130];            /* converted string */
   int          i;

   i_to_s(string, string2, 130, &len);
   string_width = XTextWidth(font_struct, string2, len);

   switch (*ijusth) {
      case 0:                       /* Left justified string */
          itempx = *ixpos;
          break;
      case 1:                       /* Center justified string */
          itempx = *ixpos - (string_width/2);
          break;
      case 2:                       /* Right justified string */
          itempx = *ixpos - string_width;
          break;
      default:
          itempx = *ixpos;
          break;
   }
   switch (*ijustv) {
      case 0:                       /* Center justified string */
          itempy = *iypos + (FONT_HEIGHT_CURRENT/2.0);
          break;
      case 1:                       /* Bottom justified string */
          itempy = *iypos;
          break;
      case 2:                       /* Top justified string */
          itempy = *iypos + FONT_HEIGHT_CURRENT;
          break;
      default:
          itempy = *iypos + (FONT_HEIGHT_CURRENT/2.0);
          break;
    }


    XDrawString(display, window, gc, itempx, itempy, string2, len);
    if (PIXMAP_CURRENT == 1) {
      XDrawString(display, pixmap, gc, itempx, itempy, string2, len);
    }

}

/* XTEXTV - draw a horizontal text string.
 *
 *          Use XDrawString rather than XDrawText so that background pixels
 *          are not changed.  This is so that other lines that may intersect
 *          the character box will not be blanked out.
 *
 * string - text string to draw
 * ixpos  - x position
 * iypos  - y position
 * ijusth - justification (horizontal)
 *          0 - left justified
 *          1 - center justified
 *          2 - right justified
 * ijustv - justiciation (vertical)
 *          0 - center justified
 *          1 - bottom justified
 *          2 - top justified
 * error  - error flag
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xtextv_(string, ixpos, iypos, ijusth, ijustv, error)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XTEXTV_(string, ixpos, iypos, ijusth, ijustv, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xtextv(string, ixpos, iypos, ijusth, ijustv, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XTEXTV(string, ixpos, iypos, ijusth, ijustv, error)
#endif
int    string[];
int    *ixpos, *iypos, *ijusth, *ijustv, *error;
{

   int          itest, itempx, itempy;   /* temporary variables */
   int          len;                     /* number of characters in string */
   int          y_pix_len;               /* height of entire string */
   int          string_width;            /* width of string in pixels */
   char         string2[130];            /* converted string */
   int          i, ijunk;
   int          string3[2];              /* one character at a time */

   i_to_s(string, string2, 130, &len);
   y_pix_len = len * (FONT_HEIGHT_CURRENT + FONT_GAP_CURRENT);

   switch (*ijustv) {
      case 0:                       /* Center justified string */
          itempy = -(y_pix_len/2) + FONT_HEIGHT_CURRENT;
          break;
      case 1:                       /* Bottom justified string */
          itempy = -y_pix_len + FONT_HEIGHT_CURRENT;
          break;
      case 2:                       /* Top justified string */
          itempy = FONT_HEIGHT_CURRENT;
          break;
      default:
          itempy = -(y_pix_len/2) + FONT_HEIGHT_CURRENT;
          break;
    }
    itempy = *iypos + itempy;

   string3[1] = 0;
   for (i = 0; i < len; i++) {  /* plot each character one at a time */
      string3[0] = string[i];
      i_to_s(string3,string2, 2, &ijunk);
      string_width = XTextWidth(font_struct, string2, 1);

     switch (*ijusth) {
        case 0:                       /* Left justified string */
            itempx = 0;
            break;
        case 1:                       /* Center justified string */
            itempx = (string_width/2);
            break;
        case 2:                       /* Right justified string */
            itempx = string_width;
            break;
        default:
            itempx = *ixpos;
            break;
     }

     itempx = *ixpos - itempx;
     XDrawString(display, window, gc, itempx, itempy, string2, 1);
     if (PIXMAP_CURRENT) {
       XDrawString(display, pixmap, gc, itempx, itempy, string2, 1);
     }
     itempy = itempy + (FONT_HEIGHT_CURRENT + FONT_GAP_CURRENT);

   }

}

/* i_to_s  - utitlity routine to convert an integer array containing
 *           Ascii Decimal Equivalents to a character string array.  The
 *           Fortran routines pass character type data as an array of
 *           ADE's, which this routine then converts to C's character
 *           type.  Note that the input array is assumed to be correct
 *           (i.e., a value between 0 and 127) and no error checking is
 *           done on it.
 *
 * string1 - input array containing ADE's.
 * string2 - output array in C character format.
 * maxlen  - maximum length for string2
 * ilen    - length of character string
 *
 */
void i_to_s(string1, string2, maxlen, ilen)
int   string1[], maxlen, *ilen;
char  string2[];

{
     int  i;
     i = 0;
     while (string1[i] != 0 && i < (maxlen - 1) ) {
         string2[i] = string1[i];
         i++;
     }
     *ilen = i;
     string2[i]='\0';
}

/* Set the screen size, use following steps:
 *
 * orien = 2:
 *
 * if xpixels and ypixels are zero, use the default sizes given
 * by this routine.  Otherwise, use xpixels and ypixels.  However,
 * compare them to the number of pixels available on the screen
 * (i.e., the root window).  If not within a certain tolerance, use
 * a half the screen width and 2/3 the screen height.
 *
 * orien = 0:
 *
 * This specifies a landscape orientation.  If the calling routine
 * specifies a positive number of x pixels, this will be used as the
 * x size.  The y size will then be (8.5/11.) of the x size.  If no
 * x size is given (i.e., xpixels = 0), then the default x size will
 * be used.  In either case, the aspect ratio will be constrained.
 *
 * orien = 1:
 *
 * This specifies a portrait orientation.  This works similarly to
 * the landscape orientation, except now the x size will be (8.5/11.)
 * of the y size.  If ypixels = 0, base the y size on the default x
 * size.
 *
 * orien = 3:
 *
 * This specifies a square orientation.  If ypixels = 0, then base on
 * default y size, otherwise use the ypixels value.
 */

void set_screen(xpixels, ypixels, orien)
int   xpixels;  /* suggested width in pixels */
int   ypixels;  /* suggested height in pixels */
int   orien;    /* orientation (landscape, portrait) */

{
   int   temp;
   float atemp;

   switch (orien) {
   case 2:              /* User specified height and width (in pixels) */
       if (xpixels > 0 && ypixels > 0) {
         temp = xpixels;
         atemp = temp;
         if (atemp < 0.3*root_width || atemp > 0.8*root_width) {
            atemp = 0.5*root_width;
            temp = atemp + 0.5;
         }
         xsh.width = temp;
         temp = ypixels;
         atemp = temp;
         if (atemp < 0.3*root_height || atemp > 0.8*root_height) {
            atemp = 0.67*root_height;
            temp = atemp + 0.5;
         }
         xsh.height = temp;
       }
       else {            /* Use default size */
         temp = DEFAULT_X_SIZE;
         atemp = temp;
         if (atemp < 0.3*root_width || atemp > 0.8*root_width) {
            atemp = 0.5*root_width;
            temp = atemp + 0.5;
         }
         xsh.width = temp;
         temp = DEFAULT_Y_SIZE;
         atemp = temp;
         if (atemp < 0.3*root_height || atemp > 0.8*root_height) {
            atemp = 0.67*root_height;
            temp = atemp + 0.5;
         }
         xsh.height = temp;
       }
       xsh.min_aspect.x = 2;
       xsh.min_aspect.y = 3;
       xsh.max_aspect.x = 4;
       xsh.max_aspect.y = 3;
       break;
   case 0:                     /* Landscape orientation */
       if (xpixels > 0 ) {     /* User specified x size */
         temp = xpixels;
         atemp = temp;
         if (atemp < 0.3*root_width || atemp > 0.8*root_width) {
            atemp = 0.5*root_width;
            temp = atemp + 0.5;
         }
         xsh.width = temp;
         atemp = xpixels * (8.5/11.0);
         if (atemp < 0.3*root_height || atemp > 0.8*root_height) {
            atemp = 0.67*root_height;
         }
         temp = atemp + 0.5;
         xsh.height = temp;
       }
       else {                 /* Use default x size */
         temp = DEFAULT_X_SIZE;
         atemp = temp;
         if (atemp < 0.3*root_width || atemp > 0.8*root_width) {
            atemp = 0.5*root_width;
            temp = atemp + 0.5;
         }
         xsh.width = temp;
         atemp = temp * (8.5/11.0);
         if (atemp < 0.3*root_height || atemp > 0.8*root_height) {
            atemp = 0.67*root_height;
         }
         temp = atemp + 0.5;
         xsh.height = temp;
       }
       xsh.min_aspect.x = 11;
       xsh.min_aspect.y = 8;
       xsh.max_aspect.x = 11;
       xsh.max_aspect.y = 9;
       break;
   case 1:                      /* Portrait orientation */
       if (ypixels > 0) {       /* User specified y size */
         temp = ypixels;
         atemp = temp;
         if (atemp < 0.3*root_height || atemp > 0.8*root_height) {
            atemp = 0.67*root_height;
         }
         temp = atemp + 0.5;
         xsh.height = temp;
         atemp = temp * (8.5/11.);
         if (atemp < 0.3*root_width || atemp > 0.8*root_width) {
            atemp = 0.5*root_width;
         }
         temp = atemp + 0.5;
         xsh.width = temp;
       }
       else {                   /* Use default size (base on X default) */
         temp = DEFAULT_X_SIZE;
         atemp = temp;
         if (atemp < 0.3*root_height || atemp > 0.8*root_height) {
            atemp = 0.67*root_height;
         }
         temp = atemp + 0.5;
         xsh.height = temp;
         atemp = temp * (8.5/11.0);
         if (atemp < 0.3*root_width || atemp > 0.8*root_width) {
            atemp = 0.5*root_width;
         }
         temp = atemp + 0.5;
         xsh.width = temp;
       }
       xsh.min_aspect.x = 8;
       xsh.min_aspect.y = 11;
       xsh.max_aspect.x = 9;
       xsh.max_aspect.y = 11;
       break;
   case 3:                     /* Square orientation */
       if (ypixels > 0 ) {     /* User specified y size */
         temp = ypixels;
         atemp = temp;
         if (atemp < 0.3*root_height || atemp > 0.8*root_height) {
            atemp = 0.5*root_height;
            temp = atemp + 0.5;
         }
         xsh.height = temp;
         xsh.width = temp;
       }
       else {                 /* Use default x size */
         temp = DEFAULT_X_SIZE;
         atemp = temp;
         if (atemp < 0.3*root_height || atemp > 0.8*root_height) {
            atemp = 0.5*root_height;
            temp = atemp + 0.5;
         }
         xsh.height = temp;
         xsh.width = temp;
       }
       xsh.min_aspect.x = 1;
       xsh.min_aspect.y = 1;
       xsh.max_aspect.x = 1;
       xsh.max_aspect.y = 1;
       break;
   default:           /* Orientation flag not specified */
       if (xpixels > 0 && ypixels > 0) {
         temp = xpixels;
         atemp = temp;
         if (atemp < 0.3*root_width || atemp > 0.8*root_width) {
            atemp = 0.5*root_width;
            temp = atemp + 0.5;
         }
         xsh.width = temp;
         temp = ypixels;
         atemp = temp;
         if (atemp < 0.3*root_height || atemp > 0.8*root_height) {
            atemp = 0.67*root_height;
            temp = atemp + 0.5;
         }
         xsh.height = temp;
       }
       else {            /* Use default size */
         temp = DEFAULT_X_SIZE;
         atemp = temp;
         if (atemp < 0.3*root_width || atemp > 0.8*root_width) {
            atemp = 0.5*root_width;
            temp = atemp + 0.5;
         }
         xsh.width = temp;
         temp = DEFAULT_Y_SIZE;
         atemp = temp;
         if (atemp < 0.3*root_height || atemp > 0.8*root_height) {
            atemp = 0.67*root_height;
            temp = atemp + 0.5;
         }
         xsh.height = temp;
       }
       xsh.min_aspect.x = 2;
       xsh.min_aspect.y = 3;
       xsh.max_aspect.x = 4;
       xsh.max_aspect.y = 3;
       break;
    }

}
