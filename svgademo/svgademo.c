/* [Comments and strings translated from German to English] */

/**********************************************************************

svgademo.c - Demo of some possibilities of the SVGA.BGI driver

In deep gratitude for the great driver and to strengthen the
C-fraction developed by

    Friedlieb Jung-Merkelbach
    Burbachstraße 41
    W-5270 Gummersbach 31

And now the same saying as in palette.c, copied in via cut&paste:
The code did cost me some work, but on the other hand is certainly
not suitable to save the world. Therefore, I have nothing against its use,
even within commercial programs.
However, it would be nice, in case of improvements/corrections or interesting
new palettes with application example, to send a corresponding diskette to
my above address.  :-)

**********************************************************************/

# include <alloc.h>
# include <bios.h>
# include <graphics.h>
# include <dos.h>
# include <stdio.h>
# include <conio.h>
# include <stdlib.h>
# include <time.h>
# include <signal.h>

# include "palette.h"


int GraphDriver,
    GraphMode = 1,  /* Autodetect */
    MaxX,
    MaxY;


void quit(void) /* Program abort via Ctrl-Break or ESC */
{
    closegraph();
    printf("Bye...");
    exit(1);
}


void wait(int sek) /* waits for key press or up to sek seconds */
{
    time_t ticks = clock();

    while (kbhit())  /* Clear keyboard buffer, evaluate ESC */
        if (getch() == 27) /* Escape */
            quit();

    while (!kbhit())
        if ((clock() - ticks) / CLK_TCK > sek)
            break;
}


/* displays (columns * rows) colors on the screen */
void displayColors(int spalten, int zeilen)
{
    int i,
        j,
        breite = (int) (MaxX / (double) (spalten) + 0.5),
        x = 0;

    if (spalten * zeilen != 256)
        return; /* Gotcha! */

    for (i = 0; i < spalten; i++)
    {
        for (j = 0; j < zeilen; ++j)
        {
            setfillstyle(SOLID_FILL, i + (j * spalten));
            bar(x, MaxY * j / (double) zeilen,
                x + breite, MaxY * (j + 1) / (double) zeilen);
        }
        x += breite;
    }
    return;
}





void far *msg_buf = NULL;      /* Image buffer for msg_on() and msg_off() */
int      msg_x,                /*  - its upper left corner, x */
         msg_y;                /*  - its upper left corner, y */

void msg_on(char *msg)  /* outputs a message in the center of the screen */
{
    int w,
        h;
    int end_x,
        end_y;

    if (MaxX <= 320)  /* due to negative x-coordinates and */
        return;       /* possibly incorrect clipping */

    settextjustify(CENTER_TEXT, CENTER_TEXT);
    settextstyle(DEFAULT_FONT, HORIZ_DIR, 1);
    setfillstyle(SOLID_FILL, 30);
    setcolor(0);
    w = textwidth(msg);
    h = textheight(msg);

    if ((msg_buf = farmalloc( /* won 1st price in readable-programming contest */
        imagesize(msg_x = (MaxX - w) / 2 - 5, msg_y = (MaxY - h) / 2 - 2,
                  end_x = (MaxX + w) / 2 + 5, end_y = (MaxY + h) / 2 + 2))) == NULL)
            return;
    getimage(msg_x, msg_y, end_x, end_y, msg_buf);
    bar(msg_x, msg_y, end_x, end_y);
    outtextxy(MaxX / 2, MaxY / 2, msg);
    return;
}


void msg_off(void)   /* removes the message created with msg_on() */
{
    if (msg_buf == NULL)
        return;
    putimage(msg_x, msg_y, msg_buf, COPY_PUT);
    farfree(msg_buf);
    msg_buf = NULL;
    return;
}



void ShadowText(int x, int y, int tfarbe, int sfarbe, char *text)
{ /* outputs shadowed text */
    int altefarbe = getcolor();

    setcolor(sfarbe);
    outtextxy(x + 1, y + 1, text);        /* subtle */
    /* outtextxy(x + 2, y + 2, text); */  /* less subtle */
    setcolor(tfarbe);
    outtextxy(x, y, text);
    setcolor(altefarbe);
    return;
}



void errex(char *text) /* Ends the program with an error message */
{
    printf("%s.\nProgram requires VGA card.\n", text);
    exit(1);
}


typedef void (*VoidFunPtr) (void);

/* calls showfunc, displays text and waits */
void show(VoidFunPtr showfunc, char *text)
{
    if (showfunc != NULL)
        showfunc();
    msg_on(text);
    wait(5);
    msg_off();
    wait(5);
    return;
}


void init(void) /* initializes graphics */
{
    extern void _Cdecl SVGA_driver(void);

    if ((GraphDriver = installuserdriver("SVGA", NULL)) < 0)
        errex("Driver cannot be installed");
    if (registerbgidriver(SVGA_driver) < 0)
        errex("Driver cannot be registered");
    if (registerbgifont(sansserif_font) < 0)
        errex("Font cannot be registered");
    initgraph(&GraphDriver, &GraphMode, "");
    if (graphresult() != grOk )
        errex("Graphics cannot be initialized");

    /*
     *  Query for mode 0. msg_on() would otherwise write beyond the screen edge,
     *  which can have the worst consequences. Try it!
     */
    if ((MaxX = getmaxx()) <= 320)
    {
        int gm = getgraphmode();

        restorecrtmode();
        printf("Your VGA card is unfortunately not sufficiently supported by SVGA.\n");
        printf("You can watch the demo, but will have to do without explanatory texts\n");
        printf("Invest in your hardware. (Press any key...) ");
        do
            if (getch() == 27) /* Escape */
                quit();
        while (kbhit());
        setgraphmode(gm);
    }
    MaxY = getmaxy();
    return;
}



void intro(void) /* Program start: welcoming the guests, agenda, ... */
{
    fade(FADE_BLACKOUT, 0, 0);
    settextjustify(CENTER_TEXT, CENTER_TEXT);
    settextstyle(SANS_SERIF_FONT, HORIZ_DIR, 6);
    ShadowText(MaxX / 2, MaxY / 4, LIGHTBLUE, LIGHTGRAY, "SVGA BGI-Driver");
    settextstyle(SANS_SERIF_FONT, HORIZ_DIR, 4);
    ShadowText(MaxX / 2, MaxY / 2, LIGHTBLUE, LIGHTGRAY, "Demo program");
    ShadowText(MaxX / 2, MaxY / 4 * 3, LIGHTBLUE, LIGHTGRAY, "Now you know what you've got...");
    ShadowText(MaxX / 2, MaxY / 4 * 3 + 1.2 * textheight("X"),
        LIGHTBLUE, LIGHTGRAY, "(Please lean back)");
    fade(FADE_UP, 120, 1);
    wait(5);
    fade(FADE_DOWN, 120, 1);

    cleardevice();
    ShadowText(MaxX / 2, MaxY / 4, YELLOW, LIGHTBLUE, "The real question is what to");
    ShadowText(MaxX / 2, MaxY / 4 + 1.2 * textheight("X"),
        YELLOW, LIGHTBLUE, "actually do with 256 colors.");
    ShadowText(MaxX / 2, MaxY / 2, YELLOW, LIGHTBLUE,
        "Here are some suggestions.");
    fade(FADE_UP, 120, 1);
    wait(5);

    fade(FADE_DOWN, 50, 4);
    cleardevice();
    fade(FADE_RESTORE, 0, 0);

    return;
}


void balls(void)
{
    int x,
        y,
        size,               /* size factor */
        color,              /* used color */
        startcolor;         /* start number of the color scale */

    set32Hpalette();
    setlinestyle(SOLID_LINE, 0, THICK_WIDTH);
    randomize();

    while (kbhit())
        getch();

    while (!kbhit())
    {
        x = random(MaxX);
        y = random(MaxY);
        startcolor = random(7) * 32 + 32;
        size = random(3) == 2 ? 2 : 1;    /* more small balls */

        if (bioskey(2) & 0x01)   /* left shift key: other palette */
            set32palette();
        else if (bioskey(2) & 0x02) /* right shift key: switch back */
            set32Hpalette();
        for (color = 0; color < 32; ++color)
        {
            setcolor(startcolor + 32 - color);
            circle(x, y, color * size);
        }
    }
    return;
}



void main(void)
{
    signal(SIGINT, quit);
    init();

    if (MaxX > 320)  /* Text is not readable at 320 * 200 */
        intro();

    displayColors(16, 16);
    msg_on(" Default palette after initgraph() ");
    wait(5);
    msg_off();
    wait(5);

    displayColors(64, 4);
    msg_on(" The same, just displayed a bit differently ");
    wait(5);
    msg_off();
    wait(5);

    show(set64palette, " 4 scales of 64 values each, primary colors + gray ");
    show(set32palette, " Standard colors + 7 scales of 32 values each ");

    displayColors(16, 16);
    show(plane, " Color planes ");
    show(setuniformpalette, " \"Uniform quantization\" ");

    displayColors(64, 4);
    show(setflowpalette, " An example with smooth transitions ");

    cleardevice();
    set32Hpalette();
    balls();

    do
        getch();
    while (kbhit());

    closegraph();
}

