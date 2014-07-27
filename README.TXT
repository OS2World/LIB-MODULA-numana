
                  MODULA-2 NUMERICAL ANALYSIS SOFTWARE

Current version: Numana01.
Last updated: 16 August 1986.

This package is freeware.  Do with it what you wish, but don't expect
it to be a fully professional package.  Its main purpose is to give
you some ideas about how to deal with variable-sized data structures.

Like all free software, this comes "as is" with no warranties as to
its fitness for a particular purpose.  If you use it, you must agree
to accept full responsibility for any consequences of its use.
If you don't agree with this condition, don't use the software.

You have permission to extend and modify the software, and indeed
this is encouraged.  (If enough volunteers do this, we can build up
to a more complete numerical analysis library.)

If you are going to use this material for something like a homework
assignment, then I (and presumably also your instructor) expect a
citation to the original source material.  Just like a paper in a
research journal, this software collection is a "publication" with
an identified author.


CONTENTS

After unzipping this package you should find several directories.
The top-level directory contains some project files - you can
throw these away if you don't use the XDS compiler.
Sources are contained in the following subdirectories:

      DEF	The definition modules
      SRC       This is where the interesting stuff is.
      SRC\TESTS Some test programs for exercising the modules.

The main library modules are:

    Cx           Complex number arithmetic
    Fourier      Fast Fourier transform.
    Integration  Routines to solve ordinary differential equations.
    Vec          Vector operations.
    Mat          Matrix operations, including eigenvalues, inverse, etc.
    Poly         Manipulation of polynomials, including root-finding.
    MiscM2       Miscellaneous procedures, collected in one module to
                 ease the porting of the other modules to various
                 environments.

(In addition, there are a couple of support modules, which are pre-release
versions of the PMOS/2 freeware that I plan to release at some later date.)

As written, the software assumes that you have the XDS compiler.
If you have a different compiler then you'll have to modify the sources (not a big job) to suit the compiler/library combination you do have.  The changes should be confined mainly to the MiscM2 module.

(I don't have any affiliation with XDS - it simply happens to be
the OS/2 compiler I use - except to the extent that I'm a
beta-tester for their compiler.)

You'll notice that all modules use the data type LONGREAL extensively.
This is because of a peculiar feature of the floating point hardware
in a PC: although LONGREAL gives better precision than REAL, the
extra time overhead is negligible.  (There is of course a space
overhead.)  If you don't like this decision, you can safely change
LONGREAL to REAL in all modules.

You'll also notice that the modules use some features of
ISO standard Modula-2 (notably 2-dimensional open arrays)
which aren't supported by some older compilers.  If you
have a PIM-style compiler (e.g. FST, TopSpeed), then you
need an older version of this package.  You can track down
the older (DOS) version by following the WWW reference below.


AUTHOR INFORMATION

            Peter Moylan
            Department of Electrical and Computer Engineering
            The University of Newcastle, NSW 2308
            Australia.

            Phone: +61 49 21 6023      (time zone GMT+10)
            Fax:   +61 49 21 6993
            e-mail:   peter@ee.newcastle.edu.au
            WWW:   http://www.ee.newcastle.edu.au/users/staff/peter/Moylan.html

The preferred method of contacting me is via e-mail; this will probably
bring a faster response than with paper mail.
