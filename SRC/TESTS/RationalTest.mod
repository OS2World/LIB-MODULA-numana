MODULE RationalTest;

        (********************************************************)
        (*                                                      *)
        (*               Test of Rational module                *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        17 March 2015                   *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

FROM Rational IMPORT
    (* type *)  Rational,
    (* proc *)  Init, Assign, Destroy, Write, Add, Sub, Mul, Div;

FROM MiscM2 IMPORT
    (* type *)  Window,
    (* proc *)  SelectWindow, WriteString, WriteLn, PressAnyKey;

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

    VAR R0, R1, R2, R3: Rational;
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
        WriteString ("TEST OF SIMPLE RATIONAL FUNCTION OPERATIONS");
        WriteLn;  WriteLn;

        (* Test with a very simple transfer function. *)

        Init (R0);
        Assign (R0, Array1{1.0}, Array2{2.0, 1.0});
        WriteString ("Rational function R0 is");  WriteLn;
        Write (R0, 8, linesize);

        Init(R1);  Init(R2);  Init(R3);

        (* Create polynomials R1 and R2. *)

        Assign (R1, Array2{1.0, 1.0}, Array3{6.0, 5.0, 1.0});
        SelectWindow (w1);
        WriteString ("R1 is");  WriteLn;
        Write (R1, 8, linesize);
        Assign (R2, Array2{2.0, 1.0}, Array3{12.0, 7.0, 1.0});
        SelectWindow (w2);
        WriteString ("R2 is");  WriteLn;
        Write (R2, 8, linesize);

        (* Do some arithmetic with them. *)

        SelectWindow (w3);
        Add (R1, R2, R3);
        WriteString ("R1 + R2 is");  WriteLn;
        Write (R3, 8, linesize);  WriteLn;
        Sub (R1, R2, R3);
        WriteString ("R1 - R2 is");  WriteLn;
        Write (R3, 8, linesize);  WriteLn;
        Mul (R1, R2, R3);
        WriteString ("R1 * R2 is");  WriteLn;
        Write (R3, 8, linesize);  WriteLn;
        PressAnyKey;
        WriteString ("--------------------------");  WriteLn;

        (* Division tests. *)

        Div (R1, R2, R3);
        WriteString ("R1 / R2: the quotient is  ");  WriteLn;
        Write (R3, 8, linesize);  WriteLn;
        PressAnyKey;

        WriteString ("--------------------------");  WriteLn;
        Destroy (R1);  Destroy (R2);  Destroy (R3);

        (*
        CloseWindow (w1);
        CloseWindow (w2);
        CloseWindow (w3);
        *)

    END BasicTest;

(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

BEGIN
    BasicTest;
END RationalTest.

