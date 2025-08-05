[Translated from German to English]

                  SVGA.BGI (C) 1990-95 Ullrich von Bassewitz
                           (C) 2020-23 Javier Guti‚rrez Chamorro

                                  README.TXT



General Information
~~~~~~~~~~~~~~~~~~
The changes in version 3.40 of the driver mainly concern S3 cards. As a result,
however, there have also been some general changes, which are listed here:

  * In version 3.40 of the driver, the control of S3 cards was completely
    revised and extended. Therefore, the segment switching routines as well as
    the routines for using multiple screen pages had to be changed for _all_
    cards. If problems occur with cards that worked flawlessly in older
    versions of the driver, please report them, stating the card name/chipset.
  * Almost all mode numbers have changed, as a new mode "1280*1024 Autodetect"
    has been added.



S3 Cards
~~~~~~~~

  * For S3 cards, the accelerator functions of the hardware are now used. If
    problems occur, this can be disabled with option "3" (see SVGA.DOC).

  * The mode numbers for switching to graphics modes are different for S3
    cards. I have based the SVGA modes on my SPEA Mirage, which uses the
    following modes:

        S3 640x480x256          69h
        S3 800x600x256          6Bh
        S3 1024x768x256         6Dh
        S3 1280x1024x256        72h

    These mode numbers are obviously not valid for all S3 cards; in particular,
    there apparently exist S3 cards with VESA support already in the video BIOS.
    If switching to graphics mode does not work correctly, the options 'V' and
    'M' can be used in conjunction with a VESA driver to ensure correct mode
    switching. 'V' disables VESA detection, i.e., the card is not treated as a
    VESA card, but 'M' forces the use of VESA mode numbers when switching.
    Through this (admittedly somewhat cumbersome) procedure, the VESA driver is
    used only for mode switching.
    If these problems occur with your card, you are welcome to send me a list
    of the mode numbers supported by your card. Perhaps a better solution can
    be found for a later version of the driver.

  * In the DOS boxes of OS/2, the setting VIDEO_8514A_XGA_IOTRAP should be set
    to Off. In the default setting (On), all accesses to S3 card registers are
    intercepted and checked. Since the S3 accelerator functions are controlled
    via such registers, this results in a sometimes considerable loss of speed.



Performance
~~~~~~~~~~~
Unlike most other drivers, SVGA.BGI allows for a very wide adaptation of the
driver to the environment (80386 version, S3 hardware, fast bankswitching
routines for VESA cards). The many configuration options of the driver have
apparently caused confusion, and I have often been asked what the "fastest" or
"best" settings are.

The answer to this is (of course): "It depends".

To name just two examples:

  * The 80386 driver is only measurably faster than the "normal" version on
    16-bit ISA cards in one function (PutImage when Mode != CopyPut). This
    changes, however, if it is a VLB or PCI card.

  * The driver uses the line function of the card for S3 cards to draw the
    lines when filling circles and polygons. However, these functions work with
    I/O commands, which can be more or less fast depending on the processor and
    mode. In addition, the overhead per line is higher when using the graphics
    hardware than when drawing the lines by software. Which is faster therefore
    depends not only on the length of the lines, but also on the mode in which
    the processor is running and, last but not least, of course, on the speed
    of the CPU itself.

So, no general statements can be made about the "best" or "fastest" setting.
Anyone who really depends on the highest performance should try out the best
setting on the target machine itself.

