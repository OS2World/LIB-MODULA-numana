MODULE PolyTest;

        (********************************************************)
        (*                                                      *)
        (*              Test of Polynomials module              *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        17 March 2015                   *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

IMPORT Cx;

FROM Poly IMPORT
    (* type *)  Polynomial,
    (* proc *)  Init, Assign, Destroy, Write, Add, Sub, Mul, Div,
                Degree, GCD, Mueller, Newton, FindRoots;

FROM MiscM2 IMPORT
    (* type *)  Window,
    (* proc *)  SelectWindow, WriteString, WriteLn, PressAnyKey;

FROM LongComplexMath IMPORT
    (* proc *)  abs;

(*
FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  OpenWindow, CloseWindow;
*)

(************************************************************************)

TYPE
    Array1 = ARRAY [0..0] OF LONGREAL;
    Array2 = ARRAY [0..1] OF LONGREAL;
    Array3 = ARRAY [0..2] OF LONGREAL;
    Array4 = ARRAY [0..3] OF LONGREAL;
    Array5 = ARRAY [0..4] OF LONGREAL;
    Array6 = ARRAY [0..5] OF LONGREAL;
    Array11 = ARRAY [0..10] OF LONGREAL;

(************************************************************************)

PROCEDURE BasicTest;

    (* Checks some simple polynomial operations. *)

    CONST linesize = 78;

    VAR P0, P1, P2, P3, P4: Polynomial;
        w1, w2, w3: Window;

    BEGIN
        (*
        OpenWindow (w1, yellow, blue, 0, 5, 0, 39, simpleframe, nodivider);
        OpenWindow (w2, yellow, blue, 0, 5, 40, 79, simpleframe, nodivider);
        OpenWindow (w3, yellow, blue, 5, 24, 0, 1+linesize,
                                        simpleframe, nodivider);
        *)
        (* For our present version, Window variables are dummies. *)
        w1 := 1;  w2 := 2;  w3 := 3;
        SelectWindow (w3);
        WriteString ("TEST OF SIMPLE POLYNOMIAL OPERATIONS");
        WriteLn;  WriteLn;

        (* Test with a very simple polynomial. *)

        Init (P0);
        Assign (P0, Array2{2.0, 1.0});
        WriteString ("Polynomial P0 is");  WriteLn;
        Write (P0, 8, linesize);

        Init(P1);  Init(P2);  Init(P3);  Init(P4);

        (* Create polynomials P1 and P2. *)

        Assign (P1, Array3{1.0, 2.0, 3.0});
        SelectWindow (w1);
        WriteString ("Polynomial P1 is");  WriteLn;
        Write (P1, 8, linesize);
        Assign (P2, Array6{5.0, -2.0, 0.0, 0.0, -3.0, 1.0});
        SelectWindow (w2);
        WriteString ("Polynomial P2 is");  WriteLn;
        Write (P2, 8, linesize);

        (* Do some arithmetic with them. *)

        SelectWindow (w3);
        Add (P1, P2, P3);
        WriteString ("P1 + P2 is");  WriteLn;
        Write (P3, 8, linesize);  WriteLn;
        Sub (P1, P2, P3);
        WriteString ("P1 - P2 is");  WriteLn;
        Write (P3, 8, linesize);  WriteLn;
        Mul (P1, P2, P3);
        WriteString ("P1 * P2 is");  WriteLn;
        Write (P3, 8, linesize);  WriteLn;
        PressAnyKey;
        WriteString ("--------------------------");  WriteLn;

        (* Division tests. *)

        Div (P1, P2, P3, P4);
        WriteString ("P1 / P2: the quotient is  ");  WriteLn;
        Write (P3, 8, linesize);  WriteLn;
        WriteString ("and the remainder is");  WriteLn;
        Write (P4, 8, linesize);  WriteLn;
        WriteString ("Check: quotient*P2 + remainder is ");  WriteLn;
        Mul (P3, P2, P3);  Add (P3, P4, P3);
        Write (P3, 8, linesize);  WriteLn;
        PressAnyKey;
        WriteString ("--------------------------");  WriteLn;

        Div (P2, P1, P3, P4);
        WriteString ("P2 / P1: the quotient is  ");  WriteLn;
        Write (P3, 8, linesize);  WriteLn;
        WriteString ("and the remainder is");  WriteLn;
        Write (P4, 8, linesize);  WriteLn;
        WriteString ("Check: quotient*P1 + remainder is ");  WriteLn;
        Mul (P3, P1, P3);  Add (P3, P4, P3);
        Write (P3, 8, linesize);  WriteLn;

        PressAnyKey;
        WriteString ("--------------------------");  WriteLn;

        (* End of tests. *)

        Destroy (P1);  Destroy (P2);  Destroy (P3);  Destroy (P4);
        (*
        CloseWindow (w1);
        CloseWindow (w2);
        CloseWindow (w3);
        *)

    END BasicTest;

(************************************************************************)

PROCEDURE GCDTest;

    (* GCD test. *)

    CONST linesize = 78;

    VAR P1, P2, P3: Polynomial;

    BEGIN
        Init (P1);
        Init (P2);
        Init (P3);

        Assign (P1, Array3{6.0, 5.0, 1.0});
        Assign (P2, Array3{12.0, 7.0, 1.0});

        WriteString ("The greatest common divisor of");  WriteLn;
        Write (P1, 8, linesize);
        WriteString ("and");  WriteLn;
        Write (P2, 8, linesize);
        P3 := GCD (P1, P2);
        WriteString ("is");  WriteLn;
        Write (P3, 8, linesize);
        PressAnyKey;

    END GCDTest;

(************************************************************************)

PROCEDURE ReconstructFromRoots (degree: CARDINAL;  roots: ARRAY OF LONGCOMPLEX);

    (* Given the roots, rebuilds and prints the polynomial.  *)

    CONST
        small = 1.0E-12;

    (********************************************************************)

    PROCEDURE Different (z1, z2: LONGCOMPLEX): BOOLEAN;

        (* Returns TRUE if z1 <> z2 within reasonable accuracy. *)

        BEGIN
            RETURN abs(z1-z2) > small;
        END Different;

    (********************************************************************)

    CONST
        dummy = CMPLX (999999999.9, 999999999.9);
        linesize = 78;

    VAR P, Q: Polynomial;
        re, im: LONGREAL;
        coeffs1: ARRAY [0..1] OF LONGREAL;
        coeffs2: ARRAY [0..2] OF LONGREAL;
        j: CARDINAL;  k: INTEGER;

    BEGIN
        Init (P);  Init (Q);
        Assign (P, Array1{1.0});
        coeffs1[1] := 1.0;  coeffs2[2] := 1.0;
        j := degree;
        WHILE j > 0 DO
            DEC (j);
            IF roots[j] <> dummy THEN
                re := RE(roots[j]);
                im := IM(roots[j]);
                IF ABS(im) < small THEN

                    (* Real root. *)

                    coeffs1[0] := -re;
                    Assign (Q, coeffs1);
                ELSE
                    (* Complex root. Its partner should be somewhere    *)
                    (* in the roots array.                              *)

                    k := j - 1;
                    WHILE (k >= 0) AND Different(roots[k], CMPLX(re, -im)) DO
                        DEC (k);
                    END (*WHILE*);
                    IF k < 0 THEN
                        WriteString ("ERROR: mismatched complex roots.");
                        WriteLn;
                        RETURN;
                    END (*IF*);
                    roots[k] := dummy;
                    coeffs2[1] := -2.0*re;
                    coeffs2[0] := re*re + im*im;
                    Assign (Q, coeffs2);

                END (*IF*);
                Mul (P, Q, P);
            END (*IF*);
        END (*WHILE*);

        WriteString ("The reconstructed monic polynomial is");
        WriteLn;
        Write (P, 8, linesize);

    END ReconstructFromRoots;

(************************************************************************)

PROCEDURE RootTest;

    (* Roots of polynomials. *)

    CONST linesize = 78;

    VAR P: Polynomial;
        w: Window;

    (********************************************************************)

    PROCEDURE DoRootTest;

        VAR allroots: ARRAY [0..20] OF LONGCOMPLEX;
            j: CARDINAL;

        BEGIN
            WriteString ("Polynomial P is");  WriteLn;
            Write (P, 8, linesize);
            FindRoots (P, allroots);
            WriteString ("We found the following roots:");
            WriteLn;
            FOR j := 0 TO VAL(CARDINAL,Degree(P))-1 DO
                Cx.Write (allroots[j], 12);
                WriteLn;
            END (*FOR*);
            ReconstructFromRoots (Degree(P), allroots);
            PressAnyKey;
            WriteString ("--------------------------");  WriteLn;
        END DoRootTest;

    (********************************************************************)

    BEGIN
        (*
        OpenWindow (w, yellow, blue, 0, 24, 0, 1+linesize,
                                        simpleframe, nodivider);
        *)
        w := 0;
        SelectWindow (w);
        WriteString ("ROOTS OF POLYNOMIALS");
        WriteLn;  WriteLn;

        Init(P);

        (* Run examples for a few polynomials P. *)

        Assign (P, Array3{2.0, -3.0, 1.0});
        DoRootTest;
        Assign (P, Array3{1.0, 2.0, 3.0});
        DoRootTest;
        Assign (P, Array4{6.0, 11.0, 6.0, 1.0});
        DoRootTest;
        Assign (P, Array6{0.0, 4.0, 0.0, 0.0, 0.0, 1.0});
        DoRootTest;
        Assign (P, Array11{1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.0});
        DoRootTest;

        (* The next two are challenging, with equal roots and a    *)
        (* zero derivative where the roots are.                    *)

        Assign (P, Array4{-8.0, 12.0, -6.0, 1.0});
        DoRootTest;
        Assign (P, Array5{1.0, -4.0, 6.0, -4.0, 1.0});
        DoRootTest;

        Destroy (P);
        WriteString ("End of tests");

        (*CloseWindow (w);*)

    END RootTest;

(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

BEGIN
    BasicTest;
    GCDTest;
    RootTest;
END PolyTest.

