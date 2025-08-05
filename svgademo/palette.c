/* [Comments translated from German to English] */

/***********************************************************************

palette.c - provides some functions for manipulating the color palette
            of VGA cards.
            Developed for the SVGA driver by Ullrich von Bassewitz,
            but should also work on any IBM 8514 card in conjunction
            with the corresponding BGI driver.

            Friedlieb Jung-Merkelbach
            Burbachstraße 41
            W-5270 Gummersbach 31

            The code did cost me some work, but on the other hand,
            it certainly isn't going to save the world.
            Therefore, I have no objection to its use, even within
            commercial programs.
            It would be nice, however, if improvements/corrections or
            interesting new palettes with application examples were sent
            to my address above on a diskette.  :-)

***********************************************************************/

# include <dos.h>
# include <graphics.h>

# include "palette.h"

typedef unsigned char Byte;

typedef struct   /* 1 entry in the VGA palette */
{
    Byte red;
    Byte green;
    Byte blue;
} RGB_Entry;


/*
 *  The following table should give usable results for any VGA card.
 *  Corrections may be necessary, however; the corresponding values
 *  would have to be tried out or (for those who prefer) "empirically
 *  determined".
 */
static RGB_Entry EGAPalette[16] =
{  /*  Red  Green  Blue */
    { 0x00, 0x00, 0x00 } ,     /*  0  Black       */
    { 0x00, 0x00, 0xAA } ,     /*  1  Blue        */
    { 0x00, 0xAA, 0x00 } ,     /*  2  Green       */
    { 0x00, 0xAA, 0xAA } ,     /*  3  Cyan        */
    { 0xAA, 0x00, 0x00 } ,     /*  4  Red         */
    { 0xAA, 0x00, 0xAA } ,     /*  5  Magenta     */
    { 0xAA, 0x55, 0x00 } ,     /*  6  Brown       */
    { 0xAA, 0xAA, 0xAA } ,     /*  7  Light gray  */
    { 0x57, 0x57, 0x57 } ,     /*  8  Dark gray   */
    { 0x55, 0x55, 0xFF } ,     /*  9  Light blue  */
    { 0x00, 0xFF, 0x00 } ,     /* 10  Light green */
    { 0x00, 0xFF, 0xFF } ,     /* 11  Light cyan  */
    { 0xFF, 0x55, 0x55 } ,     /* 12  Light red   */
    { 0xFF, 0x55, 0xFF } ,     /* 13  Light magenta */
    { 0xFF, 0xFF, 0x00 } ,     /* 14  Yellow      */
    { 0xFF, 0xFF, 0xFF }       /* 15  White       */
} ;

/************************************************************************

         int fade(enum fade_mode modus, int steps, int milli)

Fades the first 16 colors in or out. An extension to more colors is not
meaningful in the current form, since only the first 16 colors (namely
via the table above) are known.

return 0 on success, -1 on invalid mode
Valid modes (see palette.h):
    FADE_UP         fades in: from black to full brightness
    FADE_DOWN       fades out: from 'totally normal' to 'pitch dark'
    FADE_BLACKOUT   switches off all lights at once
    FADE_RESTORE    switches on again: restores the original colors

steps specifies the number of steps to use for fading.
milli specifies a time in milliseconds to wait after each step.

These two parameters are only evaluated for FADE_UP and FADE_DOWN.

************************************************************************/

int fade(enum fade_mode modus, int steps, int milli)
{
    int i,
        loop;

    switch (modus)
    {
        case FADE_UP:
            for (loop = 0; loop < steps; ++loop)
            {
                for (i = 0; i < 16; i++)
                    setrgbpalette(i,
                        EGAPalette[i].red   * (loop / ((double) steps + 0.5)),
                        EGAPalette[i].green * (loop / ((double) steps + 0.5)),
                        EGAPalette[i].blue  * (loop / ((double) steps + 0.5)));
                delay(milli);
            }
            fade(FADE_RESTORE, 0 ,0);
            break;

        case FADE_DOWN:
            for (loop = steps; loop >= 0; --loop)
            {
                for (i = 0; i < 16; i++)
                    setrgbpalette(i,
                        EGAPalette[i].red   * (loop / ((double) steps + 0.5)),
                        EGAPalette[i].green * (loop / ((double) steps + 0.5)),
                        EGAPalette[i].blue  * (loop / ((double) steps + 0.5)));
                delay(milli);
            }
            fade(FADE_BLACKOUT, 0 ,0);
            break;

        case FADE_BLACKOUT:
            for (i = 0; i < 16; i++)
               setrgbpalette(i, 0, 0, 0);     /* All black */
            break;

        case FADE_RESTORE:
            for (i = 0; i < 16; i++)
                setrgbpalette(i,      /* Restore original EGA colors */
                    EGAPalette[i].red,
                    EGAPalette[i].green,
                    EGAPalette[i].blue);
            break;

        default:
            return -1;
            /* break; */
    }
    return 0;
}


void plane(void)  /* Inspired by c't 12/89, p. 168 */
{
    int r,
        g,
        b,
        i;

    for (i = 1; i >= 0; --i)
    {
        for (g = 0; g < 16; ++g)
            for (b = 0; b < 16; ++b)
                setrgbpalette(g + 16 * b, (i * 63) << 4, g << 4, b << 4);
        delay(2000);
        for (r = 0; r < 16; ++r)
            for (g = 0; g < 16; ++g)
                setrgbpalette(r + 16 * g, r << 4, g << 4, (i * 63) << 4);
        delay(2000);
        for (r = 0; r < 16; ++r)
            for (b = 0; b < 16; ++b)
                setrgbpalette(r + 16 * b, r << 4, (i * 63) << 4, b << 4);
        delay(2000);
    }
    return;

}


void setuniformpalette(void)  /* see c't 12/89, p. 168 */
{
    int r,
        g,
        b,
        i = 0;

    for (r = 0; r < 64; r += 9)
        for (g = 0; g < 64; g += 9)
            for (b = 0; b < 64; b += 21)
                setrgbpalette(i++, r << 2, g << 2, b << 2);
    return;
}


/************************************************************************

                   void set32palette(void)

Sets a palette with the following components:
  16 standard colors
  16 complementary colors
   7 brightness scales with 32 levels each for the colors
     red, green, blue, yellow, cyan, magenta, and gray

************************************************************************/

void set32palette(void)
{
    int i;

    for (i = 0; i < 16; i++)
    {
       setrgbpalette(i,                 /* Original EGA colors */
           EGAPalette[i].red,
           EGAPalette[i].green,
           EGAPalette[i].blue);
       setrgbpalette(i + 16,            /* Complementary colors */
           0xFF - EGAPalette[i].red,
           0xFF - EGAPalette[i].green,
           0xFF - EGAPalette[i].blue);
    }
    for (i = 0; i < 32; i++)
    {                     /*     Red    Green   Blue  */
       setrgbpalette(i +  32, i << 3,      0,      0);    /* Red     */
       setrgbpalette(i +  64,      0, i << 3,      0);    /* Green   */
       setrgbpalette(i +  96,      0,      0, i << 3);    /* Blue    */
       setrgbpalette(i + 128, i << 3, i << 3,      0);    /* Yellow  */
       setrgbpalette(i + 160,      0, i << 3, i << 3);    /* Cyan    */
       setrgbpalette(i + 192, i << 3,      0, i << 3);    /* Magenta */
       setrgbpalette(i + 224, i << 3, i << 3, i << 3);    /* Gray    */
    }
    return;
}


/************************************************************************

                   void set32Hpalette(void)

Sets a palette with the following components:
  16 standard colors
  16 complementary colors
   7 brightness scales with 32 levels each for the colors
     red, green, blue, yellow, cyan, magenta, and gray
     just like set32palette(), but starting in the second
     half of the possible values

************************************************************************/

void set32Hpalette(void)
{
    int i;

    for (i = 0; i < 16; i++)
    {
       setrgbpalette(i,                 /* Original EGA colors */
           EGAPalette[i].red,
           EGAPalette[i].green,
           EGAPalette[i].blue);
       setrgbpalette(i + 16,            /* Complementary colors */
           0xFF - EGAPalette[i].red,
           0xFF - EGAPalette[i].green,
           0xFF - EGAPalette[i].blue);
    }
    for (i = 32; i < 64; i++)
    {                     /*     Red    Green   Blue  */
       setrgbpalette(i,       i << 2,      0,      0);    /* Red     */
       setrgbpalette(i +  32,      0, i << 2,      0);    /* Green   */
       setrgbpalette(i +  64,      0,      0, i << 2);    /* Blue    */
       setrgbpalette(i +  96, i << 2, i << 2,      0);    /* Yellow  */
       setrgbpalette(i + 128,      0, i << 2, i << 2);    /* Cyan    */
       setrgbpalette(i + 160, i << 2,      0, i << 2);    /* Magenta */
       setrgbpalette(i + 192, i << 2, i << 2, i << 2);    /* Gray    */
    }
    return;
}


/************************************************************************

                   void set64palette(void)

Sets a palette with 4 brightness scales with 64 values each, with the
three primary colors red, green, and blue as well as gray as a "pure
mixed color"

************************************************************************/

void set64palette(void)
{
    int i;

    for (i = 0; i < 64; i++)
    {                   /*       Red    Green   Blue  */
       setrgbpalette(      i, i << 2,      0,      0);    /* Red scale   */
       setrgbpalette(i +  64,      0, i << 2,      0);    /* Green scale */
       setrgbpalette(i + 128,      0,      0, i << 2);    /* Blue scale  */
       setrgbpalette(i + 192, i << 2, i << 2, i << 2);    /* Gray scale  */
    }
    return;
}


void setflowpalette(void) /* an example for smooth color transitions */
{
    int i;

    for (i = 0; i < 64; i++)
    {                        /*         Red           Green          Blue  */
       setrgbpalette(      i,        i << 2, (63 - i) << 2,             0);
       setrgbpalette( i + 64, (63 - i) << 2,             0,        i << 2);
       setrgbpalette(i + 128,             0,        i << 2, (63 - i) << 2);
    }
    for (i = 0; i < 32; i++)
    {
       setrgbpalette(i + 192,        i << 3, (31 - i) << 3,       i << 3);
       setrgbpalette(i + 224, (31 - i) << 3, (31 - i) << 3,       i << 3);
    }
    return;
}

