IMPLEMENTATION MODULE Rational;

        (********************************************************)
        (*                                                      *)
        (* Operations on rational functions of a single variable*)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        14 March 2015                   *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

(************************************************************************)
(*                      IMPORTANT CONVENTION                            *)
(*                                                                      *)
(* Most of the procedures in this module use "INOUT" parameters for     *)
(* their results.  It is assumed that you are likely to want to re-use  *)
(* variables (especially where a calculation involves temporary         *)
(* variables for intermediate results), so it's likely that the         *)
(* variable that will receive the result already has a valid value      *)
(* before the call.  In such a case there is an implicit "Destroy"      *)
(* operation that recovers the space occupied by the old value, before  *)
(* that value is replaced by the new result.                            *)
(*                                                                      *)
(* The internal calculations are ordered in such a way that the new     *)
(* result is calculated before the old value is destroyed.  This allows *)
(* you to use operations like Add(A,B,A) safely; the old value of A     *)
(* is fetched correctly before it is overwritten by the result.         *)
(*                                                                      *)
(* The price to be paid for this flexibility is that you _must_ do an   *)
(* "Init" operation on every variable before the first time you use it. *)
(* If you forget the "Init", this module might try to do a "DISPOSE"    *)
(* using an invalid pointer.                                            *)
(*                                                                      *)
(************************************************************************)

IMPORT Poly;

FROM MiscM2 IMPORT
    (* proc *)  WriteString, WriteLn;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

TYPE
    Rational = POINTER TO
                   RECORD
                       num, denom: Poly.Polynomial;
                   END (*RECORD*);

(************************************************************************)
(*           CREATING AND DESTROYING RATIONAL FUNCTIONS                 *)
(************************************************************************)

PROCEDURE Init (VAR (*OUT*) R: Rational);

    (* This should be the first operation performed on R, since this    *)
    (* module needs to keep track of which objects have already had     *)
    (* space allocated for them.  It creates the zero function.         *)

    BEGIN
        R := NIL;
    END Init;

(************************************************************************)

PROCEDURE Assign (VAR (*INOUT*) R: Rational;
                         numcoeffs, denomcoeffs: ARRAY OF Poly.CoeffType);

    (* Creates a rational function with specified numerator and         *)
    (* denominator coefficients.  The previous value, if any, is lost.  *)
    (* The coefficients are specified from low to high degree; for      *)
    (* example, the coefficient set specified by the array              *)
    (* (1.0, 2.0, 3.0) gives the second-degree polynomial               *)
    (* 1.0 + 2.0*x + 3.0*x^2.                                           *)

    BEGIN
        Destroy (R);
        NEW (R);
        Poly.Init (R^.num);
        Poly.Assign (R^.num, numcoeffs);
        Poly.Init (R^.denom);
        Poly.Assign (R^.denom, denomcoeffs);
    END Assign;

(************************************************************************)

PROCEDURE Destroy (VAR (*INOUT*) R: Rational);

    (* Deallocates the space occupied by R.  R is still considered to   *)
    (* exist, and its value is the zero function.  The difference       *)
    (* between Init and Destroy is that Init assumes that the input     *)
    (* value of R is random rubbish, whereas Destroy assumes that R     *)
    (* is properly structured (i.e. that an Init has previously been    *)
    (* done on it).                                                     *)

    BEGIN
        IF R <> NIL THEN
            Poly.Destroy (R^.num);
            Poly.Destroy (R^.denom);
            DISPOSE (R);
        END (*IF*);
    END Destroy;

(************************************************************************)
(*                          CREATING A COPY                             *)
(************************************************************************)

PROCEDURE Copy (R: Rational): Rational;

    (* Creates a copy of R. *)

    VAR result: Rational;

    BEGIN
        result := NIL;
        IF R <> NIL THEN
            NEW (result);
            result^.num := Poly.Copy (R^.num);
            result^.denom := Poly.Copy (R^.denom);
        END (*IF*);
        RETURN result;
    END Copy;

(************************************************************************)
(*                  CANCELLING OUT COMMON FACTORS                       *)
(************************************************************************)

PROCEDURE Simplify (VAR (*INOUT*) R: Rational);

    (* Reduces the degree of numerator and denominator, if possible. *)

    VAR Unit: ARRAY [0..0] OF Poly.CoeffType;
        gcd, dummy: Poly.Polynomial;

    BEGIN
        IF R <> NIL THEN
            IF Poly.Degree (R^.denom) < 0 THEN
                Poly.Destroy (R^.num);
                Unit[0] := 1.0;
                Poly.Assign (R^.num, Unit);
            ELSIF Poly.Degree (R^.num) < 0 THEN
                Destroy (R);
            ELSE
                Poly.Init (dummy);
                gcd := Poly.GCD (R^.num, R^.denom);
                Poly.Div (R^.num, gcd, R^.num, dummy);
                Poly.Div (R^.denom, gcd, R^.denom, dummy);
            END (*IF*);
        END (*IF*);
    END Simplify;

(************************************************************************)
(*                      THE BASIC OPERATIONS                            *)
(************************************************************************)

PROCEDURE Negate (R: Rational);

    (* R := -P.  This is an in-place operation, i.e. the original       *)
    (* value of R is overwritten.                                       *)

    BEGIN
        IF R <> NIL THEN
            Poly.Negate (R^.num);
        END (*IF*);
    END Negate;

(************************************************************************)

PROCEDURE Add (A, B: Rational;  VAR (*INOUT*) C: Rational);

    (* Computes C := A + B. *)

    VAR result: Rational;  temp: Poly.Polynomial;

    BEGIN
        IF A = NIL THEN
            result := Copy(B);
        ELSIF B = NIL THEN
            result := Copy(A);
        ELSE
            NEW (result);
            Poly.Init (result^.num);
            Poly.Init (result^.denom);
            Poly.Init (temp);
            Poly.Mul (A^.denom, B^.denom, result^.denom);
            Poly.Mul (A^.num, B^.denom, result^.num);
            Poly.Mul (A^.denom, B^.num, temp);
            Poly.Add (result^.num, temp, result^.num);
            Poly.Destroy (temp);
        END (*IF*);
        Simplify (result);
        Destroy (C);
        C := result;
    END Add;

(************************************************************************)

PROCEDURE Sub (A, B: Rational;  VAR (*INOUT*) C: Rational);

    (* Computes C := A - B. *)

    VAR temp: Rational;

    BEGIN
        temp := Copy (B);  Negate (temp);
        Add (A, temp, C);
        Destroy (temp);
    END Sub;

(************************************************************************)

PROCEDURE Mul (A, B: Rational;  VAR (*INOUT*) C: Rational);

    (* Computes C := A*B. *)

    VAR result: Rational;

    BEGIN
        IF A = NIL THEN
            result := NIL;
        ELSIF B = NIL THEN
            result := NIL;
        ELSE
            NEW (result);
            Poly.Init (result^.num);
            Poly.Init (result^.denom);
            Poly.Mul (A^.num, B^.num, result^.num);
            Poly.Mul (A^.denom, B^.denom, result^.denom);
        END (*IF*);
        Simplify (result);
        Destroy (C);
        C := result;
    END Mul;

(************************************************************************)

PROCEDURE Div (A, B: Rational;  VAR (*INOUT*) Q: Rational);

    (* Computes Q = A/B.  *)

    VAR Unit: ARRAY [0..0] OF Poly.CoeffType;
        result: Rational;

    BEGIN
        IF A = NIL THEN
            result := NIL;
        ELSE
            NEW (result);
            Poly.Init (result^.num);
            Poly.Init (result^.denom);
            IF B = NIL THEN
                Unit[0] := 1.0;
                Poly.Assign (result^.num, Unit);
            ELSE
                Poly.Mul (A^.num, B^.denom, result^.num);
                Poly.Mul (A^.denom, B^.num, result^.denom);
            END (*IF*);
        END (*IF*);
        Simplify (result);
        Destroy (Q);
        Q := result;
    END Div;

(************************************************************************)
(*                          SCREEN OUTPUT                               *)
(************************************************************************)

PROCEDURE Write (R: Rational;  places, linesize: CARDINAL);

    (* Writes R to the screen, where each coefficient is allowed to be  *)
    (* up to "places" characters wide, and "linesize" is the number of  *)
    (* characters allowed before we have to wrap onto a new line.       *)

    BEGIN
        IF R = NIL THEN
            WriteString ("0");  WriteLn;
        ELSE
            WriteString ("Numerator:");  WriteLn;
            Poly.Write (R^.num, places, linesize);
            WriteString ("Denominator:");  WriteLn;
            Poly.Write (R^.denom, places, linesize);
        END (*IF*);
    END Write;

(************************************************************************)

BEGIN
    Poly.SetVariableName ('s');
END Rational.

