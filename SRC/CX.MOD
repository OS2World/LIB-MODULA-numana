IMPLEMENTATION MODULE Cx;

        (********************************************************)
        (*                                                      *)
        (*              Complex number arithmetic               *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        31 July 1996                    *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

FROM MiscM2 IMPORT
    (* const*)  PI,
    (* proc *)  WriteString, WriteLongReal,
                Sin, Cos, Sqrt, Exp, Log, ATan2;

FROM LongComplexMath IMPORT
    (* proc *)  abs, arg, conj, sqrt, exp, ln, sin, cos, scalarMult;

(************************************************************************)

PROCEDURE Cmplx (x, y: LONGREAL): Complex;

    (* Returns x + iy. *)

    BEGIN
        RETURN CMPLX (x, y);
    END Cmplx;

(************************************************************************)

PROCEDURE Re (Z: Complex): LONGREAL;

    (* Returns the real part of Z. *)

    BEGIN
        RETURN RE(Z);
    END Re;

(************************************************************************)

PROCEDURE Im (Z: Complex): LONGREAL;

    (* Returns the imaginary part of Z. *)

    BEGIN
        RETURN IM(Z);
    END Im;

(************************************************************************)

PROCEDURE Magnitude (Z: Complex): LONGREAL;

    (* Returns the magnitude of Z. *)

    BEGIN
        RETURN abs(Z);
    END Magnitude;

(************************************************************************)

PROCEDURE Phase (Z: Complex): LONGREAL;

    (* Returns the phase (angle) of Z.  The result is in the range      *)
    (* -PI to +PI, but never exactly equal to -PI.                      *)

    BEGIN
        RETURN arg(Z);
    END Phase;

(************************************************************************)

PROCEDURE Conjg (Z: Complex): Complex;

    (* Returns the complex conjugate of Z. *)

    BEGIN
        RETURN conj(Z);
    END Conjg;

(************************************************************************)

PROCEDURE Add (A, B: Complex): Complex;

    (* Computes A + B. *)

    BEGIN
        RETURN A+B;
    END Add;

(************************************************************************)

PROCEDURE Sub (A, B: Complex): Complex;

    (* Computes A - B. *)

    BEGIN
        RETURN A-B;
    END Sub;

(************************************************************************)

PROCEDURE Mul (A, B: Complex): Complex;

    (* Computes A*B. *)

    BEGIN
        RETURN A*B;
    END Mul;

(************************************************************************)

PROCEDURE RMul (A: LONGREAL;  B: Complex): Complex;

    (* Multiplication by a real number. *)

    BEGIN
        RETURN scalarMult(A,B);
    END RMul;

(************************************************************************)

PROCEDURE Div (A, B: Complex): Complex;

    (* Computes A/B. *)

    BEGIN
        RETURN A/B;
    END Div;

(************************************************************************)

PROCEDURE Sqt (Z: Complex): Complex;

    (* Returns the square root of Z. *)

    BEGIN
        RETURN sqrt(Z);
    END Sqt;

(************************************************************************)

PROCEDURE Cexp (Z: Complex): Complex;

    (* Complex exponential. *)

    BEGIN
        RETURN exp(Z);
    END Cexp;

(************************************************************************)

PROCEDURE Cln (Z: Complex): Complex;

    (* Complex logarithm (untested. *)

    BEGIN
        RETURN ln(Z);
    END Cln;

(************************************************************************)

PROCEDURE Csin (Z: Complex): Complex;

    (* Complex sine (untested). *)

    BEGIN
        RETURN sin(Z);
    END Csin;

(************************************************************************)

PROCEDURE Ccos (Z: Complex): Complex;

    (* Complex cosine (untested). *)

    BEGIN
        RETURN cos(Z);
    END Ccos;

(************************************************************************)

PROCEDURE Write (Z: Complex;  places: CARDINAL);

    (* Writes Z in Cartesian form, with "places" characters allowed     *)
    (* for each of the real and imaginary parts.                        *)

    VAR j: CARDINAL;  impart: LONGREAL;

    BEGIN
        WriteLongReal (RE(Z), places);
        IF IM(Z) = 0.0 THEN
            FOR j := 0 TO places+3 DO
                WriteString (" ");
            END (*FOR*);
        ELSE
            impart := IM(Z);
            IF impart < 0.0 THEN
                WriteString (" - ");
                impart := -impart;
            ELSE
                WriteString (" + ");
            END (*IF*);
            WriteString ("j");
            WriteLongReal (impart, places);
        END (*IF*);
    END Write;

(************************************************************************)

END Cx.

