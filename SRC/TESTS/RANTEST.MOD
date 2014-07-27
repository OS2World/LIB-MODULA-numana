MODULE RanTest;

        (********************************************************)
        (*                                                      *)
        (*          Test of random number generator             *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        15 August 1996                  *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

IMPORT Rand, Random;

(*
FROM Windows IMPORT
    (* type *)  Window,
    (* proc *)  OpenSimpleWindow, CloseWindow;
*)

FROM MiscM2 IMPORT
    (* proc *)  SelectWindow, WriteString, WriteLn, PressAnyKey,
                WriteCard, WriteReal;

TYPE RandProc = PROCEDURE(): REAL;

(************************************************************************)
(*                              SPEED TEST                              *)
(************************************************************************)

PROCEDURE SpeedTest (proc: RandProc;  TestSize: CARDINAL);

    (* Generates a lot of random numbers.  Relies on the keyboard user  *)
    (* having a stopwatch, because the timing is not built into this    *)
    (* module.                                                          *)

    VAR j: CARDINAL;  result: REAL;

    BEGIN
        FOR j := 1 TO TestSize DO
            result := proc();
        END (*FOR*);
    END SpeedTest;

(************************************************************************)
(*                          PLAUSIBILITY TESTS                          *)
(************************************************************************)

PROCEDURE DisplayNumbers;

    CONST TestSize = 100;

    VAR j: CARDINAL;  result: REAL;

    BEGIN
        WriteString ("Start of plausibility test");  WriteLn;
        FOR j := 1 TO TestSize DO
            result := Rand.RANDOM();
            WriteReal (result, 6);
            IF j MOD 8 <> 0 THEN
                WriteString ("  ");
            ELSE
                WriteLn;
            END (*IF*);
        END (*FOR*);
        WriteLn;
        WriteString ("End of plausibility test");  WriteLn;
    END DisplayNumbers;

(************************************************************************)

PROCEDURE RunTheTests;

    CONST TestSize = 1000000;

    (*VAR w: Window;*)

    BEGIN
        (*
        OpenSimpleWindow (w, 0, 24, 0, 79);
        SelectWindow (w);
        *)
        DisplayNumbers;
        WriteString ("Starting speed test on Rand.RANDOM");  WriteLn;
        PressAnyKey;
        SpeedTest (Rand.RANDOM, TestSize);
        WriteString ("End of speed test, ");
        WriteCard (TestSize);  WriteString (" numbers generated");
        WriteLn;
        WriteString ("Starting speed test on Random.RANDOM");  WriteLn;
        PressAnyKey;
        SpeedTest (Random.RANDOM, TestSize);
        WriteString ("End of speed test, ");
        WriteCard (TestSize);  WriteString (" numbers generated");
        WriteLn;
        PressAnyKey;
        (*  CloseWindow (w); *)
    END RunTheTests;

(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

BEGIN
    RunTheTests;
END RanTest.

