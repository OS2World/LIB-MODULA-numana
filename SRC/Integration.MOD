IMPLEMENTATION MODULE Integration;

        (********************************************************)
        (*                                                      *)
        (*              Solving differential equations          *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        16 August 1996                  *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (*      Original source: many of the procedures in      *)
        (*      this module are adaptations of Fortran code     *)
        (*      published in the book "Numerical Recipes"       *)
        (*      by Press, Flannery, Teutolsky, and Vetterling.  *)
        (*      The implementation details are a little         *)
        (*      different from their code, but the underlying   *)
        (*      algorithms are pretty much the same.            *)
        (*                                                      *)
        (*      Portability problem: I've had to use an XDS     *)
        (*      language extension (open arrays) here; I        *)
        (*      haven't yet figured out how to do the job       *)
        (*      in ISO standard Modula-2.                       *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT
    (* type *)  ADDRESS;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM MiscM2 IMPORT
    (* proc *)  Power;

FROM Vec IMPORT
    (* type *)  VectorPtr,
    (* proc *)  NewVector, DisposeVector, Add, Sub, ScalarMul, Copy;

FROM Mat IMPORT
    (* type *)  ArrayPtr,
    (* proc *)  NewArray, DisposeArray;

<* m2extensions+ *>

(************************************************************************)

TYPE
    DiffEqn = POINTER TO
                    RECORD
                        order: CARDINAL;
                        Derivs: RHSproc;
                        t: LONGREAL;
                        stepmin, step, stepmax: LONGREAL;
                        errbound: LONGREAL;
                        y, dydt, yscale: VectorPtr;
                    END (*RECORD*);

    StepDriver = PROCEDURE (DiffEqn);

(************************************************************************)
(*              FOURTH ORDER RUNGE-KUTTA, FIXED STEP SIZE               *)
(************************************************************************)

PROCEDURE RK4step (DE: DiffEqn;  y, dydt: VectorPtr;
                                t, h: LONGREAL;
                                yout: VectorPtr);

    (* One step of a 4th order Runge-Kutta integration. *)

    VAR N: CARDINAL;
        tmid, hh: LONGREAL;  dym, dyt, yt, temp: VectorPtr;

    BEGIN
        N := DE^.order;
        dym := NewVector (N);
        dyt := NewVector (N);
        yt := NewVector (N);
        temp := NewVector (N);

        hh := 0.5*h;  tmid := t + hh;

        (* Let yt be the first estimate of y at the midpoint. *)

        ScalarMul (hh, dydt^, N, temp^);
        Add (y^, temp^, N, yt^);

        (* Use this to get an estimate dyt of the derivative at the     *)
        (* midpoint, and from that get an improved yt.                  *)

        DE^.Derivs (tmid, yt^, dyt^);
        ScalarMul (hh, dyt^, N, temp^);
        Add (y^, temp^, N, yt^);

        (* From the updated yt, get another estimate dym of the         *)
        (* derivative of the midpoint; and from this estimate yt as     *)
        (* the endpoint value of y.                                     *)

        DE^.Derivs (tmid, yt^, dym^);
        ScalarMul (h, dym^, N, temp^);
        Add (y^, temp^, N, yt^);

        (* Save the sum of dyt and dym, then let dyt be an estimate     *)
        (* of the derivative of the endpoint.                           *)

        Add (dyt^, dym^, N, dym^);
        DE^.Derivs (t+h, yt^, dyt^);

        (* We now have three estimates of the derivative: dydt, dyt,    *)
        (* and dym.  (Actually, there are four, because by now dym is   *)
        (* the sum of two earlier estimates.)  Use a weighted sum of    *)
        (* these to get the new y.                                      *)

        ScalarMul (2.0, dym^, N, temp^);
        Add (dydt^, temp^, N, temp^);
        Add (dyt^, temp^, N, temp^);
        ScalarMul (h/6.0, temp^, N, temp^);
        Add (y^, temp^, N, yout^);

        DisposeVector (temp, N);
        DisposeVector (dym, N);
        DisposeVector (dyt, N);
        DisposeVector (yt, N);

    END RK4step;

(************************************************************************)
(*              FIFTH ORDER RUNGE-KUTTA, VARIABLE STEP SIZE             *)
(************************************************************************)

PROCEDURE RK5step (DE: DiffEqn);

    (* One step of a Runge-Kutta method with variable step size.        *)
    (* Updates DE^.t, DE^.y, DE^.step.                                  *)

    CONST pgrow = -0.2;  pshrink = -0.25;
          safety = 0.9;  errcon = 6.0E-4;
          (* errcon is Power(4.0/safety, 1.0/pgrow) *)

    VAR ynew, temp2: VectorPtr;
        tmid, h, hh, errmax, test: LONGREAL;
        i, N: CARDINAL;  satisfied: BOOLEAN;

    BEGIN
        N := DE^.order;
        ynew := NewVector (N);
        temp2 := NewVector (N);
        h := DE^.step;

        REPEAT
            (* Take two half steps. *)

            hh := 0.5*h;
            RK4step (DE, DE^.y, DE^.dydt, DE^.t, hh, ynew);
            tmid := DE^.t + hh;
            DE^.Derivs (tmid, ynew^, temp2^);
            RK4step (DE, ynew, temp2, tmid, hh, ynew);

            (* For comparison, take one full step. *)

            RK4step (DE, DE^.y, DE^.dydt, DE^.t, h, temp2);

            (* Evaluate the error. *)

            Sub (ynew^, temp2^, N, temp2^);
            errmax := 0.0;
            FOR i := 0 TO N-1 DO
                test := ABS(temp2^[i]/DE^.yscale^[i]);
                IF test > errmax THEN errmax := test END(*IF*);
            END (*FOR*);
            errmax := errmax/DE^.errbound;
            IF errmax <= 1.0 THEN

                (* Error acceptable.  Estimate new value for    *)
                (* step size, and exit loop.                    *)

                IF errmax > errcon THEN
                    DE^.step := safety * h * Power(errmax,pgrow);
                ELSE
                    DE^.step := 4.0*h;
                END (*IF*);
                IF DE^.step > DE^.stepmax THEN
                    DE^.step := DE^.stepmax;
                END (*IF*);
                satisfied := TRUE;

            ELSIF h <= DE^.stepmin THEN

                DE^.step := DE^.stepmin;
                satisfied := TRUE;

            ELSE

                (* Error too large, reduce step size. *)

                h := safety * h * Power(errmax,pshrink);
                IF h < DE^.stepmin THEN
                    h := DE^.stepmin;
                END (*IF*);
                satisfied := FALSE;

            END (*IF*);

        UNTIL satisfied;

        (* Final correction to the answer, to get fifth-order accuracy. *)

        ScalarMul (1.0/15.0, temp2^, N, temp2^);
        Add (ynew^, temp2^, N, DE^.y^);
        DE^.t := DE^.t + h;

        DisposeVector (ynew, N);  DisposeVector (temp2, N);

    END RK5step;

(************************************************************************)
(*              MODIFIED MIDPOINT METHOD, FIXED STEP SIZE               *)
(************************************************************************)

PROCEDURE MMid (DE: DiffEqn;  ystart: VectorPtr;
                        htotal: LONGREAL;  NumberOfSteps: CARDINAL;
                        yout: VectorPtr);

    (* Modified midpoint method of solving a DE.  The initial value for *)
    (* y is ystart^, and the initial values for t, and dy/dt are        *)
    (* specified as part of the first parameter.  By the time we've     *)
    (* returned yout^ is the solution at a time htotal later.  The      *)
    (* number of substeps to be used is specified as NumberOfSteps.     *)

    VAR ym, yn, temp: VectorPtr;
        h, t: LONGREAL;
        j, N: CARDINAL;

    BEGIN
        N := DE^.order;
        ym := NewVector (N);
        yn := NewVector (N);
        temp := NewVector (N);
        h := htotal/VAL(LONGREAL,NumberOfSteps);

        (* First step.  Throughout this computation yn holds the        *)
        (* latest output estimate, and ym holds the previous estimate.  *)

        Copy (ystart^, N, ym^);
        ScalarMul (h, DE^.dydt^, N, yn^);
        Add (yn^, ystart^, N, yn^);

        (* We temporarily use yout as a temporary variable to   *)
        (* hold derivative information.                         *)

        t := DE^.t + h;
        DE^.Derivs (t, yn^, yout^);
        FOR j := 2 TO NumberOfSteps DO
            ScalarMul (2.0*h, yout^, N, temp^);
            Add (ym^, temp^, N, temp^);
            Copy (yn^, N, ym^);
            Copy (temp^, N, yn^);
            t := t + h;
            DE^.Derivs (t, yn^, yout^);
        END (*FOR*);

        (* Last step. *)

        ScalarMul (h, yout^, N, yout^);
        Add (yout^, yn^, N, yout^);
        Add (yout^, ym^, N, yout^);
        ScalarMul (0.5, yout^, N, yout^);

        DisposeVector (ym, N);
        DisposeVector (yn, N);
        DisposeVector (temp, N);

    END MMid;

(************************************************************************)
(*              BULIRSCH-STOER METHOD, VARIABLE STEP SIZE               *)
(*                                                                      *)
(* It looks to me as if I actually have the Bulirsch-Stoer method       *)
(* working, but the quality of the coding is still pretty poor.         *)
(* (I started the job by translating some old Fortran code, and it      *)
(* shows.)                                                              *)
(************************************************************************)

CONST nuse = 7;

PROCEDURE RZextract (trynumber: CARDINAL;  stepsize: LONGREAL;
                                yest, yz, yerror: VectorPtr;
                                nv: CARDINAL;
                                history: ArrayPtr);

    (* Rational function extrapolation.  This is intended to be called  *)
    (* repeatedly with increasing values of trynumber, and              *)
    (* correspondingly smaller and smaller values of stepsize.  The     *)
    (* input datum is yest^, and this is combined with values from      *)
    (* earlier calls to produce an extrapolated value yz^ and an error  *)
    (* estimate yerror^.                                                *)
    (* nv is the number of rows of the vector result.  The procedure    *)
    (* uses at most the last nuse estimates.                            *)
    (* Array "history" is for remembering the results of earlier calls. *)
    (* (In effect it is a local static variable of this procedure, but  *)
    (* the space has to be allocated by the caller so that information  *)
    (* is saved across calls.)  The last row of this array holds the    *)
    (* sequence of squares of step sizes.  In the remaining rows, the   *)
    (* first column holds a projected y vector, and the remaining       *)
    (* columns accumulate a set of correction terms.                    *)

    VAR M1, j, k: CARDINAL;
        yy, v, c, ddy, b1, b: LONGREAL;
        fx: ARRAY [1..nuse-1] OF LONGREAL;

    BEGIN
        stepsize := stepsize*stepsize;
        history^[nv,trynumber] := stepsize;
        IF trynumber = 0 THEN
            FOR j := 0 TO nv-1 DO
                b := yest^[j];
                yz^[j] := b;
                yerror^[j] := b;
                history^[j,0] := b;
            END (*FOR*);
        ELSE
            M1 := trynumber;
            IF M1 >= nuse THEN M1 := nuse-1 END(*IF*);
            FOR k := 1 TO M1 DO
                fx[k] := history^[nv,trynumber-k]/stepsize;
            END (*FOR*);
            FOR j := 0 TO nv-1 DO
                yy := yest^[j];
                v := history^[j,0];
                c := yy;
                history^[j,0] := yy;
                FOR k := 1 TO M1 DO
                    b1 := fx[k] * v;
                    b := b1 - c;
                    IF b <> 0.0 THEN
                        b := (c - v)/b;
                        ddy := c*b;
                        c := b1*b;
                    ELSE
                        ddy := v;
                    END (*IF*);
                    IF k <> M1 THEN
                        v := history^[j,k];
                    END (*IF*);
                    history^[j,k] := ddy;
                    yy := yy + ddy;
                END (*FOR*);
                yerror^[j] := ddy;
                yz^[j] := yy;
            END (*FOR*);
        END (*IF*);
    END RZextract;

(************************************************************************)

PROCEDURE BSstep (DE: DiffEqn);

    (* One step of the Bulirsch-Stoer method.  The basic idea is to     *)
    (* apply the modified midpoint method with successively finer and   *)
    (* finer subdivisions, and extrapolate the results to an estimate   *)
    (* of what should happen with zero stepsize.                        *)

    CONST imax = 10;
          shrink = 0.95;  grow = 1.2;

    TYPE StepSequence = ARRAY [0..imax] OF CARDINAL;
    CONST Nseq = StepSequence {2, 4, 6, 8, 12, 16, 24, 32, 48, 64, 96};

    VAR ysave, yseq, yerr: VectorPtr;  workspace: ArrayPtr;
        h, errmax, temp: LONGREAL;
        i, j, nv: CARDINAL;  converged: BOOLEAN;

    BEGIN
        WITH DE^ DO
            nv := order;
            yseq := NewVector (nv);
            ysave := NewVector (nv);
            yerr := NewVector (nv);
            workspace := NewArray (nv+1, nuse);
            h := step;
            Copy (y^, nv, ysave^);
            converged := FALSE;
            REPEAT
                i := 0;
                LOOP
                    MMid (DE, ysave, h, Nseq[i], yseq);
                    RZextract (i, h/VAL(LONGREAL,Nseq[i]), yseq, y, yerr, nv, workspace);
                    errmax := 0.0;
                    FOR j := 0 TO nv-1 DO
                        temp := ABS(yerr^[j]/yscale^[j]);
                        IF temp > errmax THEN
                            errmax := temp;
                        END (*IF*);
                    END (*FOR*);
                    IF errmax < errbound THEN

                        (* We've met the prescribed error tolerance.  Adjust the step   *)
                        (* size so that in the long term we'll tend to use about "nuse" *)
                        (* iterations per step.                                         *)

                        t := t + h;
                        IF i = nuse-1 THEN
                            step := h*shrink;
                        ELSIF i = nuse-2 THEN
                            step := h*grow;
                        ELSE
                            step := h*VAL(LONGREAL,Nseq[nuse-1])
                                        /VAL(LONGREAL,Nseq[i]);
                        END (*IF*);
                        IF step < stepmin THEN step := stepmin
                        ELSIF step > stepmax THEN step := stepmax
                        END (*IF*);
                        converged := TRUE;
                        EXIT (*LOOP*);

                    ELSIF i = imax THEN

                        (* If we reach here, the step has failed (which *)
                        (* should not happen often).  Try again with a  *)
                        (* smaller step size.                           *)

                        IF h = stepmin THEN
                            converged := TRUE;
                        ELSE
                            h := 0.0625*h;
                            IF h < stepmin THEN h := stepmin END(*IF*);
                        END (*IF*);
                        EXIT(*LOOP*);

                    ELSE
                        INC(i);
                    END (*IF*);

                END (*LOOP*);

            UNTIL converged;

        END (*WITH*);
        DisposeVector (ysave, nv);
        DisposeVector (yseq, nv);
        DisposeArray (workspace, nv, nuse);
        DisposeVector (yerr, nv);

    END BSstep;

(************************************************************************)
(*              STEP DRIVERS FOR EACH SOLUTION METHOD                   *)
(************************************************************************)

PROCEDURE SetScalingVector (DE: DiffEqn);

    (* Computes the scaling vector for error control. *)

    VAR j: CARDINAL;

    BEGIN
        WITH DE^ DO
            FOR j := 0 TO order-1 DO
                yscale^[j] := ABS(y^[j]) + step * ABS(dydt^[j]);
            END (*FOR*);
        END (*WITH*);
    END SetScalingVector;

(************************************************************************)

PROCEDURE DoRK4step (DE: DiffEqn);

    BEGIN
        WITH DE^ DO
            RK4step (DE, y, dydt, t, step, y);
            t := t + step;
        END (*WITH*);
    END DoRK4step;

(************************************************************************)

PROCEDURE DoRK5step (DE: DiffEqn);

    BEGIN
        SetScalingVector (DE);
        RK5step (DE);
    END DoRK5step;

(************************************************************************)

PROCEDURE DoMMstep (DE: DiffEqn);

    VAR nstep: CARDINAL;

    BEGIN
        WITH DE^ DO
            nstep := TRUNC (stepmax/stepmin);
            MMid (DE, y, stepmax, nstep, y);
            t := t + stepmax;
        END (*WITH*);
    END DoMMstep;

(************************************************************************)

PROCEDURE DoBSstep (DE: DiffEqn);

    BEGIN
        SetScalingVector (DE);
        BSstep (DE);
    END DoBSstep;

(************************************************************************)
(*              THE USER-CALLABLE SOLUTION PROCEDURE                    *)
(************************************************************************)

PROCEDURE Solve (N: CARDINAL;  RHS: RHSproc;  method: SolutionMethod;
                        Plot: PlotProc;  Extra: ADDRESS;
                        t0, tf, minstep, maxstep: LONGREAL;
                        y0: ARRAY OF LONGREAL;  eps: LONGREAL);

    (* See definition module for the meanings of the parameters. *)

    VAR DE: DiffEqn;  DoOneStep: StepDriver;

    BEGIN
        NEW (DE);
        WITH DE^ DO

            (* Set up the parameters and initial conditions. *)

            order := N;
            Derivs := RHS;
            t := t0;
            y := NewVector (N);  Copy (y0, N, y^);
            dydt := NewVector (N);
            yscale := NewVector (N);
            stepmin := minstep;  stepmax := maxstep;
            step := maxstep;
            errbound := eps;
            CASE method OF
                | RK4:  DoOneStep := DoRK4step;
                | RK5:  DoOneStep := DoRK5step;
                | MM:   DoOneStep := DoMMstep;
                | BS:   DoOneStep := DoBSstep;
            END (*CASE*);

            (* Now for the main solution loop. *)

            LOOP
                Plot (Extra, t, y^);
                IF t >= tf THEN EXIT(*LOOP*) END(*IF*);
                IF t + step > tf THEN
                    step := tf - t;
                    stepmax := step;
                END (*IF*);
                Derivs (t, y^, dydt^);
                DoOneStep (DE);
            END (*LOOP*);

            (* Throw away all temporary data. *)

            DisposeVector (y, N);
            DisposeVector (dydt, N);
            DisposeVector (yscale, N);

        END (*WITH*);
        DISPOSE (DE);

    END Solve;

(************************************************************************)

END Integration.

