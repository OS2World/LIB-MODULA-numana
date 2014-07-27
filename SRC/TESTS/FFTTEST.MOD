MODULE FFTtest;

        (********************************************************)
        (*                                                      *)
        (*              Test of module Fourier                  *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        31 July 1996                    *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (*      Observation: although the FFT is clearly a      *)
        (*      lot faster than using the direct definition     *)
        (*      of a DFT (i.e. procedure SlowFT), its           *)
        (*      accuracy improvement is smaller than I would    *)
        (*      have expected.  This is probably because I'm    *)
        (*      using iterative methods to calculate the        *)
        (*      complex exponentials.                           *)
        (*                                                      *)
        (********************************************************)

FROM Fourier IMPORT
    (* proc *)  FFT3, FFT8, SlowFT, RealFFT;

FROM MiscM2 IMPORT
    (* const*)  PI,
    (* proc *)  SelectWindow, PressAnyKey, WriteString, WriteLn,
                WriteCard, WriteLongReal, Sin, Cos;

FROM LongComplexMath IMPORT
    (* proc *)  abs;

IMPORT Cx;

(*
FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  OpenWindow, CloseWindow;
*)

(************************************************************************)

TYPE
    CxArray = ARRAY [0..1023] OF LONGCOMPLEX;

VAR
    (* To avoid stack overflow problems, we use static storage for our  *)
    (* big arrays.  (Heap allocation would also have been a viable      *)
    (* option.)                                                         *)

    TestData: CxArray;
    Results: CxArray;
    SpareCopy: CxArray;
    RealData: ARRAY [0..1023] OF LONGREAL;

(************************************************************************)
(*                      PLAUSIBILITY TESTS                              *)
(************************************************************************)

PROCEDURE TransformAndWrite (N: CARDINAL);

    (* Compares results of slow and fast methods. *)

    VAR j: CARDINAL;

    BEGIN
        WriteString ("     Slow result             FFT result");
        WriteString ("      Magnitude of difference");
        WriteLn;
        SlowFT (TRUE, N, TestData, Results);
        FFT8 (TRUE, N, TestData);
        FOR j := 0 TO N-1 DO
            Cx.Write (Results[j], 8);
            WriteString ("     ");
            Cx.Write (TestData[j], 8);
            WriteString ("     ");
            WriteLongReal (abs(Results[j] - TestData[j]), 8);
            WriteLn;
        END (*FOR*);
        PressAnyKey;
    END TransformAndWrite;

(************************************************************************)

PROCEDURE PlausibilityTest;

    (* FFT of some sample data arrays. *)

    CONST testsize = 16;

    VAR j: [0..testsize-1];  theta: LONGREAL;
        (*w: Window;*)

    BEGIN
        (*
        OpenWindow (w, black, brown, 0, 24, 0, 79, simpleframe, nodivider);
        SelectWindow (w);
        *)
        WriteString ("COMPARISON OF FFT WITH DIRECT TRANSFORM");
        WriteLn;

        (* Test on a constant function. *)

        WriteString ("Constant function, 1 data points.");
        WriteLn;
        FOR j := 0 TO testsize-1 DO
            TestData[j] := CMPLX (1.0, 0.0);
        END (*FOR*);
        TransformAndWrite (1);
        WriteString ("Constant function, 2 data points.");
        WriteLn;
        FOR j := 0 TO testsize-1 DO
            TestData[j] := CMPLX (1.0, 0.0);
        END (*FOR*);
        TransformAndWrite (2);

        WriteString ("Constant function, 4 data points.");
        WriteLn;
        FOR j := 0 TO testsize-1 DO
            TestData[j] := CMPLX (1.0, 0.0);
        END (*FOR*);
        TransformAndWrite (4);

        (* Test on a sine function. *)

        WriteString ("Cosine function, ");
        WriteCard (testsize);
        WriteString (" data points.");
        WriteLn;
        FOR j := 0 TO testsize-1 DO
            theta := PI*VAL(LONGREAL,j)/VAL(LONGREAL,testsize DIV 2);
            TestData[j] := CMPLX (Cos(theta), 0.0);
        END (*FOR*);
        TransformAndWrite (testsize);

        (* Test on a ramp function. *)

        WriteString ("Ramp function.");
        WriteLn;
        FOR j := 0 TO testsize-1 DO
            TestData[j] := CMPLX (VAL(LONGREAL,j), 0.0);
        END (*FOR*);
        TransformAndWrite (testsize);

        (*CloseWindow (w);*)

    END PlausibilityTest;

(************************************************************************)
(*                      TEST USING THE INVERSE TRANSFORM                *)
(************************************************************************)

TYPE TransformProc = PROCEDURE (BOOLEAN, CARDINAL, VAR ARRAY OF LONGCOMPLEX);

PROCEDURE OneComparison (N: CARDINAL;  Transform, InvTransform: TransformProc;
                                        title: ARRAY OF CHAR);

    (* Does a transform with "Transform", then an inverse transform     *)
    (* with "InvTransform", and summarises the errors.                  *)

    VAR j: CARDINAL;  error, max, sum: LONGREAL;

    BEGIN
        (* Take a copy of the data. *)

        FOR j := 0 TO N-1 DO
            Results[j] := TestData[j];
        END (*FOR*);

        (* Do a direct and then an inverse transform. *)

        Transform (TRUE, N, Results);
        InvTransform (FALSE, N, Results);

        (* Work out the errors. *)

        max := 0.0;  sum := 0.0;
        FOR j := 0 TO N-1 DO
            error := abs (Results[j] - TestData[j]);
            IF error > max THEN max := error END(*IF*);
            sum := sum + error;
        END (*FOR*);

        WriteString (title);
        FOR j := 0 TO 16 - HIGH(title) DO WriteString (" ") END(*FOR*);
        WriteLongReal (max, 8);
        FOR j := 0 TO 7 DO WriteString (" ") END(*FOR*);
        WriteLongReal (sum, 8);
        WriteLn;

    END OneComparison;

(************************************************************************)

PROCEDURE DoubleComparison (N: CARDINAL);

    (* Compares results of the available methods. *)

    (* Conclusion so far: FFT3 seems to be a little more accurate than  *)
    (* FFT8 (although I suspect that FFT8 is faster).  I'd like to      *)
    (* check out whether further variants are worth following up.       *)

    VAR j: CARDINAL;  max, sum, error: LONGREAL;

    BEGIN
        WriteString ("   Method        max error");
        WriteString ("     sum of errors");
        WriteLn;

        OneComparison (N, FFT3, FFT3, "FFT3/FFT3");
        OneComparison (N, FFT3, FFT8, "FFT3/FFT8");
        OneComparison (N, FFT8, FFT3, "FFT8/FFT3");
        OneComparison (N, FFT8, FFT8, "FFT8/FFT8");

        (* For the slow FT, the calling conventions are different       *)
        (* because we don't have an in-place algorithm.                 *)

        (* Take a copy of the data. *)

        FOR j := 0 TO N-1 DO
            SpareCopy[j] := TestData[j];
        END (*FOR*);

        (* Do a direct and then an inverse transform. *)

        SlowFT (TRUE, N, TestData, Results);
        SlowFT (FALSE, N, Results, TestData);

        (* Work out the errors. *)

        max := 0.0;  sum := 0.0;
        FOR j := 0 TO N-1 DO
            error := abs (SpareCopy[j] - TestData[j]);
            IF error > max THEN max := error END(*IF*);
            sum := sum + error;
        END (*FOR*);

        WriteString ("Slow             ");
        WriteLongReal (max, 8);
        FOR j := 0 TO 7 DO WriteString (" ") END(*FOR*);
        WriteLongReal (sum, 8);
        WriteLn;

        PressAnyKey;

    END DoubleComparison;

(************************************************************************)

PROCEDURE DoubleTransformTest;

    (* FFT followed by inverse FFT. *)

    CONST testsize = 128;       (* I had to cut this down, the slow     *)
                                (* transform was taking too long.       *)
        k = testsize DIV 2;

    VAR j: [0..testsize-1];  theta: LONGREAL;
        (*w: Window;*)

    BEGIN
        (*
        OpenWindow (w, black, green, 0, 24, 0, 79, simpleframe, nodivider);
        SelectWindow (w);
        *)
        WriteString ("DIRECT TRANSFORM FOLLOWED BY INVERSE TRANSFORM");
        WriteLn;

        (* Test on a constant function. *)

        WriteLn;
        WriteString ("Constant function, ");
        WriteCard (testsize);
        WriteString (" data points.");
        WriteLn;
        FOR j := 0 TO testsize-1 DO
            TestData[j] := CMPLX (1.0, 0.0);
        END (*FOR*);
        DoubleComparison (testsize);

        (* Test on a sine function. *)

        WriteLn;
        WriteString ("Cosine function, ");
        WriteCard (testsize);
        WriteString (" data points.");
        WriteLn;
        FOR j := 0 TO testsize-1 DO
            theta := PI*VAL(LONGREAL,j)/VAL(LONGREAL,testsize DIV 2);
            TestData[j] := CMPLX (Cos(theta), 0.0);
        END (*FOR*);
        DoubleComparison (testsize);

        (* Test on some other simple functions. *)

        WriteLn;
        WriteString ("Ramp function.");
        WriteLn;
        FOR j := 0 TO testsize-1 DO
            TestData[j] := CMPLX (VAL(LONGREAL,j), 0.0);
        END (*FOR*);
        DoubleComparison (testsize);

        WriteLn;
        WriteString ("Square wave.");
        WriteLn;
        FOR j := 0 TO k-1 DO
            TestData[j] := CMPLX (1.0, 0.0);
            TestData[j+k] := CMPLX (-1.0, 0.0);
        END (*FOR*);
        DoubleComparison (testsize);

        (*CloseWindow (w);*)

    END DoubleTransformTest;

(************************************************************************)
(*                      TEST OF REAL FFT                                *)
(************************************************************************)

PROCEDURE DoRealTest (N: CARDINAL);

    (* Performs transform and writes results. *)

    VAR j: CARDINAL;

    BEGIN
        WriteString ("Real part         Imag part");
        WriteLn;
        RealFFT (TRUE, N, RealData);
        WriteLongReal (RealData[0], 8);
        WriteString ("        ");
        WriteLongReal (0.0, 8);
        WriteLn;
        FOR j := 1 TO (N DIV 2)-1 DO
            WriteLongReal (RealData[2*j], 8);
            WriteString ("        ");
            WriteLongReal (RealData[2*j + 1], 8);
            WriteLn;
        END (*FOR*);
        WriteLongReal (RealData[1], 8);
        WriteString ("        ");
        WriteLongReal (0.0, 8);
        WriteLn;
        PressAnyKey;
    END DoRealTest;

(************************************************************************)

PROCEDURE RealTest;

    (* FFT of some sample data arrays. *)

    CONST testsize = 16;

    VAR j: [0..testsize-1];  theta: LONGREAL;
        (*w: Window;*)

    BEGIN
        (*
        OpenWindow (w, black, brown, 0, 24, 0, 79, simpleframe, nodivider);
        SelectWindow (w);
        *)
        WriteString ("TEST OF REAL FFT");
        WriteLn;

        (* Test on a constant function. *)

        WriteString ("Constant function, 2 data points.");
        WriteLn;
        FOR j := 0 TO testsize-1 DO
            RealData[j] := 1.0;
        END (*FOR*);
        DoRealTest (2);

        WriteString ("Constant function, 4 data points.");
        WriteLn;
        FOR j := 0 TO testsize-1 DO
            RealData[j] := 1.0;
        END (*FOR*);
        DoRealTest (4);

        (* Test on a sine function. *)

        WriteString ("Cosine function, ");
        WriteCard (testsize);
        WriteString (" data points.");
        WriteLn;
        FOR j := 0 TO testsize-1 DO
            theta := PI*VAL(LONGREAL,j)/VAL(LONGREAL,testsize DIV 2);
            RealData[j] := Cos(theta);
        END (*FOR*);
        DoRealTest (testsize);

        (* Test on a ramp function. *)

        WriteString ("Ramp function.");
        WriteLn;
        FOR j := 0 TO testsize-1 DO
            RealData[j] := VAL(LONGREAL,j);
        END (*FOR*);
        DoRealTest (testsize);

        (* Do an inverse transform on this last result. *)

        WriteString ("Inverse transform of this last result.");
        WriteLn;
        RealFFT (FALSE, testsize, RealData);
        FOR j := 0 TO testsize-1 DO
            WriteLongReal (RealData[j], 12);
            WriteLn;
        END (*FOR*);
        PressAnyKey;

        (*CloseWindow (w);*)

    END RealTest;

(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

BEGIN
    PlausibilityTest;
    DoubleTransformTest;
    RealTest;
END FFTtest.

