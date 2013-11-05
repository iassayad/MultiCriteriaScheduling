OVERVIEW :
----------
SynDEx is a system level CAD software, supporting the "Algorithm Architecture
Adequation" (AAA) methodology, for rapid prototyping and optimizing the
implementation of real-time embedded applications on multicomponent
architectures. It has been designed and developed at INRIA in the Rocquencourt
Research Unit France, by the OSTRE team.

SynDEx currently runs under the following systems :
* Linux
* Windows NT/95/98/2000
* Solaris


DISTRIBUTED FILES :
-------------------
README         this file
COPYRIGHT      distribution license
HELP           help file
bin/           syndex binaries
libs/          libraries
examples/      some example SynDEx files 
syndex-?.?.?   shell script, to run syndex

INSTALLATION :
--------------

SynDEx needs some informations to run; those informations are passed
through the options of the command line. The distribution contains a
pre-configured script : you will have to modify it.  The variable
<INSTALL_DIR> must contain the SynDEx installation directory. Default
is "." which will most of the time work.

WARNING : For some versions of Windows (95/98) variables have a fixed
maximum size.  You will get a message which is approximately
"Unsufficient environment space" ("Espace environnement insuffisant"
in French version").  Therefore you may need to use shorter names than
the default ones ("DIR" instead of "INSTALL_DIR", s??? instead of
syndex-?.?.?).  You must also use the Dos format for your directories
(eg progra~1 instead of Program_Files).

EXECUTION :
-----------

To run SynDEx, just execute the script syndex-?.?.? of the syndex directory.
To see the various options for this script, use option -h or --help.

COPYRIGHT :
-----------
See the file COPYRIGHT.

DOCUMENTATION :
---------------
See the file HELP.

INFORMATION :
-------------
More detailed information can be found on the SynDEx Web :
http://www-rocq.inria.fr/syndex/
