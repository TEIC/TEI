
                       Unicode encoding for GNU Emacs
                                      
                               March 15, 2000
                                      
   GNU Emacs has had support for a large multi-lingual character set
   since Version 20, which incorporated the "Mule" (Multi-lingual Emacs)
   patches made at ETL in Japan. For historic reasons, Mule has defined
   its own character set and encoding (when Mule was designed, Unicode
   simply didn't exist). Sooner or later, Emacs will migrate to using
   Unicode internally, but in the meantime, we need to define an encoding
   that will convert between Unicode externally and Emacs' Mule encoding
   internally.
   
   I used to have a home-grown implementation for this based on an
   external C program. If you are interested, it is still available at
   [1]ftp://ftp.cs.ust.hk/pub/ipe/emacs-utf-0.62.tar.gz.
   
   Now, however, I'm using Miyashita Hisashi's "Mule-UCS" package, which
   integrates better with GNU Emacs (in particular, it's much easier to
   read/write mail in UTF-8).
   
   This webpage describes an extension to Mule-UCS to cover the complete
   Basic Multilingual Plane (the BMP). In particular, this package
     * defines a new encoding utf-8, which can be used like any other
       Emacs coding system. It is indicated by the letter "u" in the mode
       line.
     * covers the complete BMP.
     * uses two Unicode fonts to render the whole BMP range, one for
       half-width characters, one for full-width characters.
     * adheres to Markus Kuhn's definition of "wcwidth()" to select
       between the two fonts, so that Emacs should work fine on a UTF-8
       aware terminal emulator (with two exceptions, see below).
       
   Note that you do not need to recompile or modify Emacs itself, the
   Unicode support can be installed separately (you don't need to be the
   administrator of the system running Emacs).
   
                                  [INLINE]
                                      
   First, you'll need GNU Emacs version 20.4 or higher. (Version 20.6 is
   recommended, as it fixes a bug in the encoding system.)
   
   Second, you'll need a Unicode font (more precisely, a font in the
   iso10646-1 encoding). I recommend you download Markus Kuhn's
   [2]ucs-fonts.tar.gz from his [3]Unicode font page. Install the fonts
   and check that you have the following fonts available:
csz094[~].. xlsfonts | grep 10646
-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso10646-1
-misc-fixed-medium-r-normal--14-130-75-75-c-70-iso10646-1
-misc-fixed-medium-r-normal--15-140-75-75-c-90-iso10646-1
-misc-fixed-medium-r-normal--18-120-100-100-c-90-iso10646-1
-misc-fixed-medium-r-normal--20-200-75-75-c-100-iso10646-1

   If you want to use CJK characters, you'll also need a full-width
   Unicode font. Install the fonts from [4]ucs-fonts-asian.tar.gz, and
   check that you have the following font available:
csz094[~].. xlsfonts | grep 10646 | grep -- -ja-
-misc-fixed-medium-r-normal-ja-18-120-100-100-c-180-iso10646-1

   Now download the package
   [5]ftp://ftp.cs.ust.hk/pub/ipe/oc-unicode-0.72.2.tar.gz and unpack it.
   Note that it contains a copy of Miyashita Hisashi's "Mule-UCS" package
   (the original is at [6]ftp://ftp.m17n.org/pub/mule/Mule-UCS/).
   
   Compile the package by using the following in the top-level directory:
emacs -batch -l oc-comp.el

   This will byte-compile the "Mule-UCS-0.72" package. Copy all files
   with .elc extension from the "Mule-UCS-0.72/lisp" subdirectory onto
   your Emacs lisp path. In addition, copy oc-unicode.el, oc-charsets.el,
   and oc-tools.el onto your Emacs lisp path.
   
   Add the following to your .emacs file:
(require 'oc-unicode)
(if (eq window-system 'x)
      (oc-create-fontset
       "-misc-fixed-medium-r-normal--18-*-*-*-*-*-fontset-standard"
       "-misc-fixed-medium-r-normal-ja-18-*-iso10646-*")
      (oc-create-fontset
       "-misc-fixed-medium-r-normal--13-*-*-*-*-*-fontset-standard"
       "-misc-fixed-medium-r-normal-ja-13-*-iso10646-*"))

   Start Emacs and check that "Describe Coding System" in the Mule menu
   recognizes utf-8 as an encoding. If this goes wrong, you probably do
   not have multilingual support (Mule) enabled. Make sure you are not
   starting Emacs with the command line option --unibyte (perhaps through
   a shell-script?) and that the environment variable EMACS_UNIBYTE is
   not set. Check your .emacs file for things that could disable Mule
   support, such as "(standard-display-european 1)" (it is obsolete
   anyway). The Emacs info system has more information about Mule under
   the menu entry "International".
   
   Load the file UTF-8-demo.utf (a sample file by Markus Kuhn). The
   extension .utf should trigger the UTF-8 encoding, and you should see
   the letter u appear at the beginning of your mode line. The text
   itself will be full of empty square boxes though--Emacs has not yet
   loaded the Unicode font. To do so, use "Set Font/Fontset" in the Mule
   menu, and select the fontset standard: 18-dot medium. You should be
   able to see all characters displayed properly now.
   
   If you don't like the font or its size, you will need to create a
   different fontset. Just change the oc-create-fontset function in
   .emacs. You can create multiple fontsets to choose from.
   
   Once you are happy with your selected fontset, you can set up your
   .emacs file so that Emacs will switch to it automatically:
(if (eq window-system 'x)
      (set-frame-font (oc-create-fontset
       "-misc-fixed-medium-r-normal--18-*-*-*-*-*-fontset-standard"
       "-misc-fixed-medium-r-normal-ja-18-*-iso10646-*"))
      (oc-create-fontset
       "-misc-fixed-medium-r-normal--13-*-*-*-*-*-fontset-standard"
       "-misc-fixed-medium-r-normal-ja-13-*-iso10646-*"))

   You can insert a character by its Unicode number using "M-x
   insert-ucs-character".
   
   You can query information from the Unicode character database about
   the character under the cursor using "M-x unicode-what". If you have
   the database (a text file) installed on your system, put the filename
   in your .emacs file like this:
(setq unicode-data-path ".../UnicodeData-Latest.txt")

   (If you don't set this variable, Emacs will try to retrieve the file
   from the Unicode FTP server.)
   
   You will probably want to bind these commands to keys, for instance to
   the key combinations "C-c =" and "C-c i", like this:
(global-set-key "\C-c=" 'unicode-what)
(global-set-key "\C-ci" 'insert-ucs-character)

    CJK
    
   If you are interested in CJK languages, try loading Kanji.utf and
   Hangul.utf. They show the whole Ideographic block, from U+4e00 to
   U+9fa6, and the complete Hangul block, from U+ac00 to U+d7ff.
   
    Input methods
    
   An important fact to understand about this implementation is that the
   Mule "character set" is simply the disjoint union of several national
   character sets. Many characters in the Unicode sense correspond to
   characters in several of these character sets. For instance, the
   "CAPITAL LETTER A WITH ACUTE" in the latin-1 and latin-2 charsets are
   different characters as far as Emacs is concerned.
   
   When you use the "utf-8" encoding to load a file, one internal charset
   will be chosen for each Unicode character. You can verify the charset
   of a character under the cursor by typing "C-u C-x =".
   
   The difficulty is that the input methods that come with Emacs (in the
   leim directory) do distinguish the internal character sets, and will
   never create characters in the newly added Unicode sets ("Unicode A"
   to "Unicode E"). The characters will look "right" on the screen (with
   a different font, perhaps), but searching will fail if the search
   string is from a different internal character set.
   
   You can always force the characters in your file to the "standard"
   Unicode encoding by saving and reloading the file (C-x C-s C-x C-v).
   For serious work, however, you will want to switch the input methods
   for scripts you are using to generate the standard Unicode
   representation directly. This is easy to achieve, you only need to
   convert the file defining the input method to UTF-8 encoding.
   
   Here is how you would do this for, say, the input methods in
   latin-post.el:
    1. Create a directory quail in your personal Emacs lisp load path,
       say ~/lisp/quail. This assumes that ~/lisp is on your Emacs lisp
       load path. (Type "C-h v load-path" in Emacs to check.)
    2. Open the file /usr/share/emacs/20.6/leim/quail/latin-post.el (or
       wherever it is on your system).
    3. Change the encoding to Unicode (C-x C-m f utf-8)
    4. Insert the following line as the very first line of the file:
;; -*- coding: utf-8 -*-
    5. Save it as ~/lisp/quail/latin-post.el
    6. Now you are ready to use it: (C-x C-m C-\ latin-2-post) Make sure
       the input methods are loaded from the newly created file.
       
   Of course the existing input methods only cover a small part of the
   new Unicode repertoire. See the Quail documentation to find out how to
   create input methods for GNU Emacs.
   
    wcwidth adherence
    
   The package uses Markus Kuhn's [7]definition of "wcwidth()" to select
   between the half-width and full-width Unicode fonts, so that Emacs
   should work fine on a UTF-8 aware terminal emulator following the same
   definition.
   
   There are two exceptions: I had to arbitrarily split the user-defined
   character range into full-width and half-width, and to optimally use
   the available slots, I made U+e000..U+efff full-width.
   
   On the other hand, it doesn't make sense to make some, but not all
   conjoining Jamo full-width. In my implementation, they are all
   half-width (of course, on a conjoining renderer the result of
   conjoining would be full-width).
   
    Disclaimer.
    
   This package exists because I needed to edit UTF-8 encoded files
   containing multiple character sets, and found putting together this
   converter easier than getting used to another editor.
   
   I do not expect to do much more work on this, since GNU Emacs is
   moving to use Unicode as its internal character set, and will support
   all of this natively.
     _________________________________________________________________
   
   
    [8]Otfried Cheong

References

   1. ftp://ftp.cs.ust.hk/pub/ipe/emacs-utf-0.62.tar.gz
   2. http://www.cl.cam.ac.uk/~mgk25/download/ucs-fonts.tar.gz
   3. http://www.cl.cam.ac.uk/~mgk25/ucs-fonts.html
   4. http://www.cl.cam.ac.uk/~mgk25/download/ucs-fonts-asian.tar.gz
   5. ftp://ftp.cs.ust.hk/pub/ipe/oc-unicode-0.72.2.tar.gz
   6. ftp://ftp.m17n.org/pub/mule/Mule-UCS/
   7. http://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c
   8. http://www.cs.ust.hk/~otfried
