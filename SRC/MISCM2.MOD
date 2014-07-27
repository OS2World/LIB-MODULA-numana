IMPLEMENTATION MODULE MiscM2;

        (********************************************************)
        (*                                                      *)
        (*          Miscellaneous utility procedures            *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        31 July 1996                    *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (*   Shortcomings:                                      *)
        (*      1. The PressAnyKey procedure is failing to      *)
        (*         return until end-of-line is found.  As a     *)
        (*         workaround, I've changed it to require the   *)
        (*         <Enter> key to be pressed.                   *)
        (*      2. (fixed)                                      *)
        (*                                                      *)
        (*      The purpose of this module is to provide        *)
        (*      the non-portable part of a numerical            *)
        (*      analysis package - i.e. to separate out the     *)
        (*      library dependencies, so that most of the       *)
        (*      work in porting the software to another         *)
        (*      compiler or library lies in rewriting this      *)
        (*      (simple) module.                                *)
        (*                                                      *)
        (*      Many of the procedures here relate to output    *)
        (*      to a screen window.  For use in an environment  *)
        (*      which does not support screen windows, you      *)
        (*      simply have to replace the definition of        *)
        (*      type "Window" by a dummy definition, and let    *)
        (*      the implementation ignore the "Window"          *)
        (*      parameters.                                     *)
        (*                                                      *)
        (*      One catch with the present approach is that     *)
        (*      it requires the concept of the "current         *)
        (*      window".  Do not attempt to use this module     *)
        (*      in multitasking applications, because if        *)
        (*      more than one task is doing screen output       *)
        (*      then there is an ambiguity in what constitutes  *)
        (*      the current window.                             *)
        (*                                                      *)
        (*      This version is for use with the XDS compiler.  *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT
    (* type *)  ADDRESS,
    (* proc *)  MOVE, ADDADR;

IMPORT LongMath, STextIO, SWholeIO, SRealIO, SLongIO, Conversions;

(************************************************************************)

VAR
    (* The currently selected screen window. *)

    cw: Window;

(************************************************************************)
(*                      MATHEMATICAL FUNCTIONS                          *)
(************************************************************************)

PROCEDURE Exp (x: LONGREAL): LONGREAL;

    (* Exponential. *)

    BEGIN
        RETURN LongMath.exp(x);
    END Exp;

(************************************************************************)

PROCEDURE Log (x: LONGREAL): LONGREAL;

    (* Natural logarithm. *)

    BEGIN
        RETURN LongMath.ln(x);
    END Log;

(************************************************************************)

PROCEDURE Power (x, y: LONGREAL): LONGREAL;

    (* Computes x to the power of y. *)

    BEGIN
        RETURN LongMath.power(x,y);
    END Power;

(************************************************************************)

PROCEDURE Sin (x: LONGREAL): LONGREAL;

    (* Sine of x (radians). *)

    BEGIN
        RETURN LongMath.sin(x);
    END Sin;

(************************************************************************)

PROCEDURE Cos (x: LONGREAL): LONGREAL;

    (* Cosine of x (radians). *)

    BEGIN
        RETURN LongMath.cos(x);
    END Cos;

(************************************************************************)

PROCEDURE Sqrt (x: LONGREAL): LONGREAL;

    (* Square root. *)

    BEGIN
        RETURN LongMath.sqrt(x);
    END Sqrt;

(************************************************************************)

PROCEDURE ATan2 (x, y: LONGREAL): LONGREAL;

    (* Inverse tangent of y/x.  Result is in range -PI to PI. *)

    VAR result: LONGREAL;

    BEGIN
        IF x = 0.0 THEN
           IF y = 0.0 THEN RETURN 0.0
           ELSIF y < 0.0 THEN RETURN -0.5*PI
           ELSE RETURN 0.5*PI
           END (*IF*);
        ELSE
            result := LongMath.arctan (y/x);
            IF x < 0.0 THEN
                IF y >= 0.0 THEN result := PI - result
                ELSE result := result - PI
                END (*IF*);
            END (*IF*);
            RETURN result;
        END (*IF*);
    END ATan2;

(************************************************************************)
(*                      MISCELLANEOUS UTILITIES                         *)
(************************************************************************)

PROCEDURE BlockCopy (source, destination: ADDRESS;  bytecount: CARDINAL);

    (* Copies an array of bytes from the source address to the          *)
    (* destination address.                                             *)

    BEGIN
        MOVE (source, destination, bytecount);
    END BlockCopy;

(************************************************************************)

PROCEDURE AddOffset (A: ADDRESS;  increment: CARDINAL): ADDRESS;

    (* Returns a pointer to the memory location whose physical address  *)
    (* is Physical(A)+increment.  It is assumed that the caller will    *)
    (* never try to run off the end of a segment.                       *)

    BEGIN
        RETURN ADDADR (A, increment);
    END AddOffset;

(************************************************************************)
(*                  NUMERIC-TO-STRING CONVERSION                        *)
(************************************************************************)

PROCEDURE LongRealToString (number: LONGREAL;
                                        VAR (*OUT*) buffer: ARRAY OF CHAR;
                                        fieldsize: CARDINAL);

    (* Converts the number to a decimal character string in array       *)
    (* "buffer", right-justified in a field of fieldsize characters.    *)
    (* The format depends on the size of the number relative to the     *)
    (* size of the buffer.                                              *)

    BEGIN
        Conversions.LongRealToString (number, buffer, fieldsize);
    END LongRealToString;

(************************************************************************)
(*                          SCREEN OUTPUT                               *)
(************************************************************************)

PROCEDURE SelectWindow (w: Window);

    (* Specifies that all screen output, up until the next call to      *)
    (* SelectWindow, will be to window w.                               *)

    BEGIN
        cw := w;
    END SelectWindow;

(************************************************************************)

PROCEDURE WriteString (s: ARRAY OF CHAR);

    (* Writes s to the current window. *)

    BEGIN
        STextIO.WriteString (s);
    END WriteString;

(************************************************************************)

PROCEDURE WriteLn;

    (* Writes an end-of-line to the current window. *)

    BEGIN
        STextIO.WriteLn;
    END WriteLn;

(************************************************************************)

PROCEDURE PressAnyKey;

    (* "Press any key to continue". *)
    (* Bug: this is requiring "Enter" before the character can  *)
    (* be read.  I'm not yet sure how to solve this.            *)
    (* As a temporary work-around, we require the user to       *)
    (* press the <Enter> key rather than the <Any> key.         *)

    (*VAR dummy: CHAR;*)

    BEGIN
        STextIO.WriteLn;
        STextIO.WriteString ("Press Enter to continue");
        STextIO.SkipLine;
        (*STextIO.ReadChar (dummy);*)

        (* Alternative approaches that didn't work: *)

        (*conio.getch();*)              (* Linker can't find getch *)
        (*InOut.Read (dummy);*)         (* Same problem as with STextIO.ReadChar *)
        (*OS2.KbdCharIn(dummy);*)       (* Compiler can't find KbdCharIn, even though *)
                                        (* :INCL_KBD+ was specified.  Error in OS2.DEF? *)

    END PressAnyKey;

(************************************************************************)

PROCEDURE Error (message: ARRAY OF CHAR);

    (* Puts a message to the screen. *)

    (* VAR w, save: Window; *)

    BEGIN
        (*
        save := cw;
        Windows.OpenWindow (w, Windows.black, Windows.green, 11, 14, 10, 69,
                                Windows.simpleframe, Windows.nodivider);
        SelectWindow (w);
        *)
        WriteString ("Error: ");  WriteString (message);
        PressAnyKey;
        (*
        Windows.CloseWindow (w);
        SelectWindow (save);
        *)
    END Error;

(************************************************************************)

PROCEDURE WriteCard (N: CARDINAL);

    (* Writes a cardinal value. *)

    BEGIN
        SWholeIO.WriteCard (N, 8);
    END WriteCard;

(************************************************************************)

PROCEDURE WriteRJCard (number, fieldsize: CARDINAL);

    (* Like WriteCard, but the result is right justified in a field     *)
    (* of fieldsize characters.                                         *)

    BEGIN
        SWholeIO.WriteCard (number, fieldsize);
    END WriteRJCard;

(************************************************************************)

PROCEDURE WriteReal (x: REAL;  places: CARDINAL);

    (* Writes x in a field "places" characters wide. *)

    BEGIN
        SRealIO.WriteReal (x, places);
    END WriteReal;

(************************************************************************)

PROCEDURE WriteLongReal (x: LONGREAL;  places: CARDINAL);

    (* Writes x in a field "places" characters wide. *)

    VAR buffer: ARRAY [0..127] OF CHAR;

    BEGIN
        (* I've scrapped my use of SLongIO here because it was giving wrong answers. *)
        (*SLongIO.WriteReal (x, places);*)

        LongRealToString (x, buffer, places);
        WriteString (buffer);

    END WriteLongReal;

(************************************************************************)
(*                         KEYBOARD INPUT                               *)
(************************************************************************)

PROCEDURE ReadCard (VAR (*OUT*) N: CARDINAL);

    (* Reads a cardinal from the keyboard, echoing it to screen. *)

    BEGIN
        SWholeIO.ReadCard (N);
    END ReadCard;

(************************************************************************)

PROCEDURE ReadLongReal(): LONGREAL;

    (* Reads and converts a numeric string from the keyboard.   *)

    VAR result: LONGREAL;

    BEGIN
        SLongIO.ReadReal(result);
        RETURN result;
    END ReadLongReal;

(************************************************************************)

BEGIN
    cw := 0;
END MiscM2.

