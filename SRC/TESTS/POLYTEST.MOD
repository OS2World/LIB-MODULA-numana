MODULE PolyTest;

        (********************************************************)
        (*                                                      *)
        (*              Test of Polynomials module              *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        31 July 1996                    *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

IMPORT Cx;

FROM Poly IMPORT
    (* type *)  Polynomial,
    (* proc *)  Init, Assign, Destroy, Write, Add, Sub, Mul, Div,
                Degree, Mueller, Newton, FindRoots;

FROM MiscM2 IMPORT
    (* type *)  Window,
    (* proc *)  SelectWindow, WriteString, WriteLn, PressAnyKey;

(*
FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  OpenWindow, CloseWindow;
*)

(************************************************************************)

TYPE
    Array3 = ARRAY [0..2] OF LONGREAL;
    Array4 = ARRAY [0..3] OF LONGREAL;
    Array5 = ARRAY [0..4] OF LONGREAL;
    Array6 = ARRAY [0..5] OF LONGREAL;
    Array11 = ARRAY [0..10] OF LONGREAL;

(************************************************************************)

PROCEDURE BasicTest;

    (* Checks some simple polynomial operations. *)

    CONST linesize = 78;

    VAR P1, P2, P3, P4: Polynomial;
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
        Destroy (P1);  Destroy (P2);  Destroy (P3);  Destroy (P4);
        (*
        CloseWindow (w1);
        CloseWindow (w2);
        CloseWindow (w3);
        *)

    END BasicTest;

(************************************************************************)

PROCEDURE RootTest;

    (* Roots of polynomials. *)

    CONST linesize = 78;

    VAR P: Polynomial;
        w: Window;

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
        (*
            Mueller (P, root);
            WriteString ("Found the root ");
            Cx.Write (root, 12);
            WriteLn;
            Newton (P, root);
            WriteString ("By Newton's method, improved this to");
            WriteLn;
            Cx.Write (root, 12);
            WriteLn;
        *)
            PressAnyKey;
            WriteString ("--------------------------");  WriteLn;
        END DoRootTest;

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

        (* This next test still giving wrong answers. *)

        Assign (P, Array11{1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.0});
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
    RootTest;
END PolyTest.

