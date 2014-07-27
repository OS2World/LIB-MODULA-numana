MODULE EigenTest;

        (********************************************************)
        (*                                                      *)
        (*              Test of Matrices module                 *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        16 August 1996                  *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

IMPORT Cx;

FROM Mat IMPORT
    (* proc *)  Zero, Unit, Write, Add, Sub, Mul,
                Random, Eigenvalues;

(*
FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  OpenWindow, CloseWindow;
*)

FROM MiscM2 IMPORT
    (* proc *)  SelectWindow, WriteString, WriteLn, PressAnyKey;

(************************************************************************)

CONST NMAX = 10;

(************************************************************************)

PROCEDURE EigenvalueTest (A: ARRAY OF ARRAY OF LONGREAL;  N: CARDINAL);

    (* Calculates and writes the eigenvalues of the leading NxN submatrix of A. *)

    VAR W: ARRAY [1..NMAX] OF LONGCOMPLEX;
        j: [1..NMAX];

    BEGIN
        WriteString ("Matrix A is");  WriteLn;
        Write (A, N, N, 10);

        WriteString ("The eigenvalues of A are");  WriteLn;
        Eigenvalues (A, W, N);
        FOR j := 1 TO N DO
            WriteString ("    ");  Cx.Write (W[j], 10);  WriteLn;
        END (*FOR*);

        PressAnyKey;

    END EigenvalueTest;

(************************************************************************)

PROCEDURE Test (A: ARRAY OF ARRAY OF LONGREAL);

    (* Eigenvalue test using all of A. *)

    BEGIN
        EigenvalueTest (A, HIGH(A[0])+1);
    END Test;

(************************************************************************)

PROCEDURE RunAllTests;

    (* A collection of eigenvalue calculations. *)

    TYPE M2 = ARRAY [1..2],[1..2] OF LONGREAL;
         M3 = ARRAY [1..3],[1..3] OF LONGREAL;

    VAR A: ARRAY [1..NMAX],[1..NMAX] OF LONGREAL;
        (*j: CARDINAL;*)

    BEGIN
        WriteString ("EIGENVALUES OF A SQUARE MATRIX");
        WriteLn;

        (* Try a random A matrix. *)

        Random (A, 5, 5);
        EigenvalueTest (A, 5);

        (* Try unit matrices of various sizes. *)
        (* Skipped - these all passed without trouble. *)

        (*
        Unit (A, NMAX);
        FOR j := 1 TO NMAX DO
            EigenvalueTest (A, j);
        END (*FOR*);
        *)

        (* A few more judiciously selected matrices. *)

        Test (M2{{0.0, 1.0}, {1.0, 0.0}});
        Test (M2{{0.0, 1.0}, {-1.0, 0.0}});
        Test (M2{{0.0, 1.0}, {-2.0, -3.0}});
        Test (M2{{0.0, -2.0}, {1.0, -3.0}});

        Test (M3{{0.0, 1.0, 0.0}, {0.0, 0.0, 1.0}, {-6.0, -11.0, -6.0}});

        (* Summary: every test so far gives the right answer. *)

    END RunAllTests;

(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

BEGIN
    RunAllTests;
END EigenTest.

