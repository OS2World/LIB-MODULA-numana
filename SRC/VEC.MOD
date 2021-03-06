IMPLEMENTATION MODULE Vec;

        (********************************************************)
        (*                                                      *)
        (*                 Vector arithmetic                    *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        15 August 1996                  *)
        (*  Status:             Seems to be working             *)
        (*                                                      *)
        (*      Portability note: this module contains some     *)
        (*      open array operations which are a language      *)
        (*      extension in XDS Modula-2 but not part of       *)
        (*      ISO standard Modula-2.  So far I haven't worked *)
        (*      out how to do this in the standard language.    *)
        (*                                                      *)
        (********************************************************)

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM MiscM2 IMPORT
    (* proc *)  WriteLn, WriteString, WriteLongReal;

(************************************************************************)

<* m2extensions+ *>
<* storage+ *>

(************************************************************************)
(*                  CREATING AND DESTROYING VECTORS                     *)
(************************************************************************)

PROCEDURE NewVector (N: CARDINAL): VectorPtr;

    (* Creates a vector of N elements. *)

    VAR result: VectorPtr;

    BEGIN
        NEW (result, N);
        RETURN result;
    END NewVector;

(************************************************************************)

PROCEDURE DisposeVector (VAR (*INOUT*) V: VectorPtr;  N: CARDINAL);

    (* Deallocates a vector of N elements. *)

    BEGIN
        DISPOSE (V);
    END DisposeVector;

(************************************************************************)
(*                          COPYING A VECTOR                            *)
(************************************************************************)

PROCEDURE Copy (A: ARRAY OF EltType;  N: CARDINAL;
                         VAR (*OUT*) B: ARRAY OF EltType);

    (* Copies an N-element vector A to B. *)

    VAR i: CARDINAL;

    BEGIN
        FOR i := 0 TO N-1 DO
            B[i] := A[i];
        END (*FOR*);
    END Copy;

(************************************************************************)
(*                          VECTOR ARITHMETIC                           *)
(************************************************************************)

PROCEDURE Add (A, B: ARRAY OF EltType;  elts: CARDINAL;
                      VAR (*OUT*) C: ARRAY OF EltType);

    (* Computes C := A + B.  All vectors have elts elements. *)

    VAR i: CARDINAL;

    BEGIN
        FOR i := 0 TO elts-1 DO
            C[i] := A[i] + B[i];
        END (*FOR*);
    END Add;

(************************************************************************)

PROCEDURE Sub (A, B: ARRAY OF EltType;  elts: CARDINAL;
                      VAR (*OUT*) C: ARRAY OF EltType);

    (* Computes C := A - B.  All vectors have elts elements.  *)

    VAR i: CARDINAL;

    BEGIN
        FOR i := 0 TO elts-1 DO
            C[i] := A[i] - B[i];
        END (*FOR*);
    END Sub;

(************************************************************************)

PROCEDURE Mul (A: ARRAY OF ARRAY OF EltType;  B: ARRAY OF EltType;
                      N1, N2: CARDINAL;
                      VAR (*OUT*) C: ARRAY OF EltType);

    (* Computes C := A*B, where A is N1xN2 and B is N2x1. *)

    VAR i, j: CARDINAL;  sum: EltType;

    BEGIN
        FOR i := 0 TO N1-1 DO
            sum := 0.0;
            FOR j := 0 TO N2-1 DO
                sum := sum + A[i,j]*B[j];
            END (*FOR*);
            C[i] := sum;
        END (*FOR*);
    END Mul;

(************************************************************************)

PROCEDURE ScalarMul (A: EltType;  B: ARRAY OF EltType;  elts: CARDINAL;
                                  VAR (*OUT*) C: ARRAY OF EltType);

    (* Computes C := A*B, where A is scalar and B has elts elements. *)

    VAR i: CARDINAL;

    BEGIN
        FOR i := 0 TO elts-1 DO
            C[i] := A * B[i];
        END (*FOR*);
    END ScalarMul;

(************************************************************************)
(*                              OUTPUT                                  *)
(************************************************************************)

PROCEDURE Write (V: ARRAY OF EltType;  N: CARDINAL;  places: CARDINAL);

    (* Writes the N-element vector V to the screen, where each  *)
    (* column occupies a field "places" characters wide.        *)

    VAR i: CARDINAL;

    BEGIN
        FOR i := 0 TO N-1 DO
            WriteString ("  ");
            WriteLongReal (V[i], places-2);
        END (*FOR*);
        WriteLn;
    END Write;

(************************************************************************)

END Vec.

