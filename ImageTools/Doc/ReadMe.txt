MEGA 65 Image Tools Read Me
===========================


1.  Introduction
----------------

These tools are meant for use in the preparation of images for display on the
forth-coming MEGA65 computer.

There is a tool for conversion of images (ImageToTiles) to the VIC-IV Tiled 
Image format as well as a tool for viewing these images (ImageView).

The tools were written with Object Pascal and will compile on either Delphi 
(any XE version should work, XE8 tested) on Windows or FreePascal with Lazarus 
on any platform (Linux tested).

There are some issues with FreePascal/Lazarus compilation which will be 
mentioned in the compilation section.


2.  Usage
---------

The image tools are simple to use, GUI tools.  ImageToTiles has an additional
command-line invocation mode.


2.1. ImageToTiles
-----------------

When run from the command-line, the following structure is used:

        ImageToTiles [-b#<nnnnnn>] <inputfile> <outputfile>

Where:
	Option -b supplies the background colour.  <nnnnnn> is a hexadecimal
              number representing the colour (RRGGBB).  This is commonly 
              referred to as "internet format".  It is optional with black as
              the default.

      <inputfile> is the image file to process.  Currently PNG or BMP files are
              supported.

      <outputfile> is the V4TI image file to output.  The file should use the 
              extension ".v4t".

When run in GUI mode (without any parameters), select the background colour and
open a file to process by clicking Open.  Click Process to process the image.
Click Save to save the image.  Click Clear to clear any open image.

Presently, only images that are multiples of 8 in width and height should be 
opened.  The tool does not correct for situations where this is not the case.
This will be addressed in the near future.


2.2.  ImageView
---------------

This tool has only a GUI mode.  Click File | Open to open a V4TI file and view
it.


3.  Compilation
---------------

The tools can be compiled with either Delphi or FPC/Lazarus as already 
mentioned.  Simply open the project files and compile.

The tools use advanced compiler features (generics) and because of this, only 
the most recent versions of FPC (version 3.0.0 or above) can be used to compile
the tools.  At the time of writing, you will need to get the compiler and IDE 
from svn and manually build them or download the components individually, 
upgrading as required.  It is beyond the scope of this document to provide 
detailed information on the installation of these suites.

Further to the FPC version requirement, it was noted that at least on Linux, the
64 bit version of the compiler fails to compile the tools, giving an internal 
error.  Since version 3 of FPC is in a pre-release state, it is expected that 
this will be addressed sometime before release.


4.  Further Information
-----------------------

For further information about these tools or the MEGA65 project, please contact
me, Daniel England, at "mewpokemon {at} hotmail {dot} com" using "MEGA65" in the 
subject line.





Daniel.
