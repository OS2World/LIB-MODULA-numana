MODULE DEtest;

        (********************************************************)
        (*                                                      *)
        (*              Test of module Integration              *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        16 August 1996                  *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

(*
FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  OpenWindow, CloseWindow;
*)

FROM SYSTEM IMPORT
    (* type *)  ADDRESS;

FROM MiscM2 IMPORT
    (* type *)  Window,
    (* proc *)  SelectWindow, PressAnyKey, WriteString, WriteLn, WriteLongReal, Exp, Cos;

FROM Integration IMPORT
    (* type *)  SolutionMethod,
    (* proc *)  Solve;

FROM Vec IMPORT
    (* type *)  VectorPtr,
    (* proc *)  NewVector, DisposeVector;

(************************************************************************)

PROCEDURE Deriv1 (t: LONGREAL;  y: ARRAY OF LONGREAL;  VAR (*OUT*) RHS: ARRAY OF LONGREAL);

    (* Calculates right side for equation dy/dt = -y.   *)

    BEGIN
        RHS[0] := -y[0];
    END Deriv1;

(************************************************************************)

PROCEDURE Plot1 (dummy: ADDRESS;  t: LONGREAL;  y: ARRAY OF LONGREAL);

    (* Solution plotter for equation 1. *)

    BEGIN
        WriteLongReal (t, 8);  WriteString ("   ");
        WriteLongReal (y[0], 8);  WriteString ("   ");
        WriteLongReal (3.0*Exp(-t), 8);
        WriteLn;
    END Plot1;

(************************************************************************)

PROCEDURE Test1;

    CONST minstep = 0.05;  maxstep = 0.5;  tmax = 3.0;

    VAR y: VectorPtr;
        (*w: Window;*)

    BEGIN
        (*
        OpenWindow (w, black, brown, 0, 24, 0, 79, simpleframe, nodivider);
        SelectWindow (w);
        *)
        WriteString ("Solving a first order differential equation.");
        WriteLn;  WriteLn;
        y := NewVector (1);
        y^[0] := 3.0;
        WriteString ("      t    computed y   true y");
        WriteLn;
        Solve (1, Deriv1, BS, Plot1, NIL,
                        0.0, tmax, minstep, maxstep, y^, 1.0E-5);
        DisposeVector (y, 1);
        (*CloseWindow (w);*)
    END Test1;

(************************************************************************)

PROCEDURE Deriv2 (t: LONGREAL;  x: ARRAY OF LONGREAL;  VAR (*OUT*) RHS: ARRAY OF LONGREAL);

    (* Calculates right side for equation dx1/dt = x2, dx2/dt = -x1.   *)

    BEGIN
        RHS[0] := x[1];
        RHS[1] := -x[0];
    END Deriv2;

(************************************************************************)

PROCEDURE Plot2 (dummy: ADDRESS;  t: LONGREAL;  x: ARRAY OF LONGREAL);

    (* Solution plotter for equation 1. *)

    BEGIN
        WriteLongReal (t, 8);  WriteString ("   ");
        WriteLongReal (x[0], 8);  WriteString ("   ");
        WriteLongReal (Cos(t), 8);
        WriteLn;
    END Plot2;

(************************************************************************)

PROCEDURE Test2;

    CONST minstep = 0.05;  maxstep = 0.5;  tmax = 8.0;

    VAR x: VectorPtr;
        (*w: Window;*)

    BEGIN
        (*
        OpenWindow (w, black, brown, 0, 24, 0, 79, simpleframe, nodivider);
        SelectWindow (w);
        *)
        WriteString ("Solving a second order differential equation.");
        WriteLn;  WriteLn;
        x := NewVector (2);
        x^[0] := 1.0;
        x^[1] := 0.0;
        WriteString ("      t    computed y   true y");
        WriteLn;
        Solve (2, Deriv2, BS, Plot2, NIL,
                        0.0, tmax, minstep, maxstep, x^, 1.0E-5);
        DisposeVector (x, 2);
        (*CloseWindow (w);*)
    END Test2;

(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

BEGIN
    Test1;
    PressAnyKey;
    Test2;
END DEtest.

