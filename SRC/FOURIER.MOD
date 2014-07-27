IMPLEMENTATION MODULE Fourier;

        (********************************************************)
        (*                                                      *)
        (*              Fast Fourier Transform                  *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        31 July 1996                    *)
        (*  Status:             Working, but I'd like to check  *)
        (*              further variants, also get a better     *)
        (*              scrambling algorithm.                   *)
        (*                                                      *)
        (*      Desirable additions:                            *)
        (*       - special cases for real data.                 *)
        (*       - convolutions.                                *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT
    (* proc *)  ADR;

FROM LongComplexMath IMPORT
    (* proc *)  scalarMult, exp;

FROM MiscM2 IMPORT
    (* const*)  PI,
    (* proc *)  Error, Cos, Sin, Sqrt;

(************************************************************************)
(*                      MISCELLANEOUS UTILITIES                         *)
(*                                                                      *)
(*  Not all of the next few procedures are used; they're there mainly   *)
(*  as intermediate steps in my development work, and might be          *)
(*  removed from a final version.                                       *)
(*                                                                      *)
(************************************************************************)

(*
PROCEDURE Log2 (N: CARDINAL): CARDINAL;

    (* Logarithm to base 2 (rounded down to the next-lower integral     *)
    (* value, in case N is not a power of 2.)  The result is in the     *)
    (* range 0..31.                                                     *)

    VAR test, result: CARDINAL;

    BEGIN
        test := MAX(CARDINAL) DIV 2 + 1;  result := 31;
        WHILE test > N DO
            DEC (result);  test := test DIV 2;
        END (*WHILE*);
        RETURN result;
    END Log2;
*)
(************************************************************************)
(*
PROCEDURE BitRev (num, bits: CARDINAL): CARDINAL;

    (* Takes the last "bits" bits of "num", and reverses their order.   *)
    (* The legal range of "bits" is 0..31.                              *)

    VAR j, result: CARDINAL;

    BEGIN
        result := 0;
        FOR j := 1 TO bits DO
            result := 2*result;
            IF ODD(num) THEN INC(result) END(*IF*);
            num := num DIV 2;
        END (*FOR*);
        RETURN result;
    END BitRev;
*)
(************************************************************************)
(*
PROCEDURE Scramble (N, m: CARDINAL;  VAR (*INOUT*) data: ARRAY OF LONGCOMPLEX);

    (* Re-sorts a data array of size N = 2^m into bit-reversed order,   *)
    (* i.e. for all j data[j] is swapped with data[BitRev(j,m)].        *)

    (* For the moment this is a "proof of concept" version which uses   *)
    (* a crude method for doing the scrambling.  It works, but I've     *)
    (* found a faster method - see Shuffle below.                       *)

    VAR j, newpos: CARDINAL;  temp: LONGCOMPLEX;

    BEGIN
        FOR j := 1 TO N-1 DO
            newpos := BitRev (j, m);
            IF newpos > j THEN
                temp := data[j];  data[j] := data[newpos];
                data[newpos] := temp;
            END (*IF*);
        END (*FOR*);
    END Scramble;
*)
(************************************************************************)
(*
PROCEDURE W(r, N: CARDINAL): LONGCOMPLEX;

    (* Computes W^r as needed for FFT calculations.             *)
    (* W = exp(-2*pi*i/N), therefore W^r = exp(-2*pi*i*r/N)     *)
    (* W^r = cos(t) - i sin(t), where t = 2*pi*r/N.             *)

    (* This procedure is not at present being used, but is here for     *)
    (* new development work.                                            *)

    CONST TwoPi = 2.0*PI;

    VAR theta: LONGREAL;

    BEGIN
        theta := TwoPi*VAL(LONGREAL,r)/VAL(LONGREAL,N);
        RETURN CMPLX(Cos(theta), -Sin(theta));
    END W;
*)
(************************************************************************)
(*                      THE SHUFFLING ALGORITHM                         *)
(*                                                                      *)
(*  The following two procedures work together to put an array into     *)
(*  "bit-reversed" order: for all j, data[j] is swapped with data[k],   *)
(*  where k is obtained by writing j as a binary number and then        *)
(*  reversing the order of its bits.  The method we use here is         *)
(*  admittedly rather obscure - it was derived by a succession of       *)
(*  program transformations starting from a more readable but highly    *)
(*  recursive algorithm - but it has a significant speed advantage      *)
(*  over more obvious ways of doing the job.                            *)
(*                                                                      *)
(************************************************************************)

PROCEDURE Commit (start, step, size: CARDINAL;
                                VAR (*INOUT*) data: ARRAY OF LONGCOMPLEX);

    (* Swaps all elements in seq with their bit-reversed partners.      *)
    (* The difference between this procedure and Shuffle is that here   *)
    (* we are sure that every element in seq will be involved in a      *)
    (* swap, whereas in Shuffle we are dealing with a sequence where    *)
    (* some elements will stay in place.                                *)

    (* The sequence seq is defined to be the sequence of size elements  *)
    (* whose first element is at location (start + step DIV 2), and     *)
    (* subsequent elements occur at subscript increments of step.       *)

    VAR N, destination, depth, oldsize: CARDINAL;
        Stack: ARRAY [1..12] OF CARDINAL;
        temp: LONGCOMPLEX;

    BEGIN
        N := step*size;
        destination := start + N;
        INC (start, step DIV 2);
        depth := 0;

        LOOP
            (* Swap one pair of elements. *)

            temp := data[start];
            data[start] := data[destination];
            data[destination] := temp;

            (* Find the next subsequence to work on. *)

            INC (depth);
            oldsize := size;
            N := N DIV size;
            step := step*size;
            size := 1;

            LOOP
                IF oldsize > 1 THEN
                    EXIT (*LOOP*);
                END(*IF*);

                DEC (depth);
                IF depth = 0 THEN
                    RETURN;
                END (*IF*);

                oldsize := Stack[depth];
                DEC (destination, N);
                step := step DIV 2;
                DEC (start, step);
                N := 2*N;
                size := 2*size;

            END (*LOOP*);

            INC (start, step DIV 2);
            INC (destination, N);
            Stack[depth] := oldsize DIV 2;

        END (*LOOP*);

    END Commit;

(************************************************************************)

PROCEDURE Shuffle (size: CARDINAL;  VAR (*INOUT*) data: ARRAY OF LONGCOMPLEX);

    (* Shuffles an array into bit-reversed order.  The actual work of   *)
    (* moving subsequences is done by procedure Commit.  The present    *)
    (* procedure has the job of working out which subsequences to pass  *)
    (* on to Commit.  This culling procedure means that we step right   *)
    (* past elements that don't need to be moved, and that those that   *)
    (* do need to be moved are moved exactly once.                      *)

    VAR start, step, newstep, level, scale: CARDINAL;
        Stack: ARRAY [1..7] OF CARDINAL;

        (* The upper stack bound needs to be Log2(size) DIV 2. *)

    BEGIN
        start := 0;  step := 1;  level := 0;

        LOOP
            IF size < 4 THEN
                IF level <= 1 THEN
                    RETURN;
                END (*IF*);
                scale := 2*Stack[level];
                newstep := step DIV scale;
                INC (start, 2*(size-1)*step + 3*(step + newstep) DIV 2);
                step := newstep;
                size := scale*scale*size;
                DEC (level);
                Stack[level] := 2*Stack[level];
            ELSE
                INC (level);  Stack[level] := 1;
            END (*IF*);

            step := 2*step;
            size := size DIV 4;

            (* For the current subsequence, swap all odd elements in    *)
            (* the first half with even elements in the second half.    *)

            Commit (start, step, size, data);

        END (*LOOP*);

    END Shuffle;

(************************************************************************)
(*                 "DECIMATION IN TIME" ALGORITHM                       *)
(************************************************************************)

PROCEDURE FFTDTSN (Direct: BOOLEAN;  N: CARDINAL;
                        VAR (*INOUT*) data: ARRAY OF LONGCOMPLEX);

    (* Fast Fourier transform of an array of N complex data points.     *)
    (* The result is returned in the same array.  N must be a power of  *)
    (* two.  The algorithm is essentially a form of one of the          *)
    (* Cooley-Tukey transforms, but I've set up the computations in     *)
    (* such a way that there are no sine/cosine calculations.  This     *)
    (* should, I think, improve both speed and accuracy.                *)

    (* This version assumes scrambled input data.                       *)

    (* Status: working. *)

    VAR gap, j, k, pos1, pos2, blocksize, groups: CARDINAL;
        temp1, temp2, multiplier, mstep: LONGCOMPLEX;  scale, re, im: LONGREAL;

    BEGIN
        (* For extra speed, we pull out the first pass as a special     *)
        (* case.                                                        *)

        IF N >= 2 THEN
            FOR pos1 := 0 TO N-2 BY 2 DO
                temp1 := data[pos1];
                temp2 := data[pos1+1];
                data[pos1] := temp1 + temp2;
                data[pos1+1] := temp1 - temp2;
            END (*FOR*);
        END (*IF*);

        (* Now for the second and subsequent passes. *)

        groups := N DIV 4;  gap := 2;
        IF Direct THEN
            mstep := CMPLX (0.0, -1.0);
        ELSE
            mstep := CMPLX (0.0, +1.0);
        END (*IF*);

        WHILE groups >= 1 DO
            multiplier := CMPLX (1.0, 0.0);
            blocksize := 2*gap;
            FOR j := 0 TO gap-1 DO
                pos1 := j;
                pos2 := pos1 + gap;
                FOR k := 0 TO groups-1 DO
                    temp1 := data[pos1];
                    temp2 := multiplier * data[pos2];
                    data[pos1] := temp1 + temp2;
                    data[pos2] := temp1 - temp2;
                    INC (pos1, blocksize);
                    INC (pos2, blocksize);
                END (*FOR*);
                multiplier := multiplier * mstep;
            END (*FOR*);
            groups := groups DIV 2;

            (* This next calculation gets the square root of mstep.     *)
            (* It should be faster than the more general square root    *)
            (* calculation.                                             *)

            re := Sqrt (0.5*(1.0 + RE(mstep)));
            im := 0.5 * IM(mstep) / re;
            mstep := CMPLX (re, im);
            gap := blocksize;

        END (*WHILE*);

        (* For an inverse transform, the results have to be scaled by   *)
        (* a factor 1/N.                                                *)

        IF NOT Direct THEN
            scale := 1.0 / VAL(LONGREAL, N);
            FOR k := 0 TO N-1 DO
                data[k] := scalarMult (scale, data[k]);
            END (*FOR*);
        END (*IF*);

    END FFTDTSN;

(************************************************************************)

PROCEDURE FFT3 (Direct: BOOLEAN;  N: CARDINAL;
                        VAR (*INOUT*) data: ARRAY OF LONGCOMPLEX);

    (* Fast Fourier transform of an array of N complex data points.     *)
    (* The result is returned in the same array.  N must be a power of  *)
    (* two.                                                             *)

    (* Status: Working. *)

    BEGIN
        Shuffle (N, data);
        FFTDTSN (Direct, N, data);
    END FFT3;

(************************************************************************)
(*              "DECIMATION IN FREQUENCY" ALGORITHM                     *)
(************************************************************************)

PROCEDURE FFT8G (Direct: BOOLEAN;  N: CARDINAL;
                        VAR (*INOUT*) data: ARRAY OF LONGCOMPLEX);

    (* Fast Fourier transform of an array of N complex data points.     *)
    (* The result is returned in the same array.  N must be a power of  *)
    (* two.  The output is in scrambled order.                          *)

    (* Status: Working. *)

    VAR k, pos1, pos2, g, groups, halfN: CARDINAL;
        scale, temp1, temp2, WW: LONGCOMPLEX;  theta: LONGREAL;

    BEGIN
        IF N <= 1 THEN RETURN END(*IF*);
        groups := 1;  halfN := N DIV 2;
        theta := PI/VAL(LONGREAL,halfN);
        IF Direct THEN
            WW := CMPLX (Cos(theta), -Sin(theta));
        ELSE
            WW := CMPLX (Cos(theta), Sin(theta));
        END (*IF*);
        WHILE N > 1 DO
            scale := CMPLX (1.0, 0.0);
            FOR k := 0 TO halfN-1 DO
                pos1 := k;
                FOR g := 0 TO groups-1 DO
                    pos2 := pos1 + halfN;
                    temp1 := data[pos1];
                    temp2 := data[pos2];
                    data[pos1] := temp1 + temp2;
                    data[pos2] := scale * (temp1 - temp2);
                    INC (pos1, N);
                END (*FOR*);
                scale := scale*WW;
            END (*FOR*);
            groups := 2*groups;  N := halfN;  halfN := N DIV 2;
            WW := WW * WW;
        END (*WHILE*);

        (* For an inverse transform, the results have to be scaled by   *)
        (* a factor 1/N.  By now groups = original N.                   *)

        IF NOT Direct THEN
            theta := 1.0 / VAL(LONGREAL, groups);
            FOR k := 0 TO groups-1 DO
                data[k] := scalarMult (theta, data[k]);
            END (*FOR*);
        END (*IF*);

    END FFT8G;

(************************************************************************)

PROCEDURE FFT8 (Direct: BOOLEAN;  N: CARDINAL;
                                VAR (*INOUT*) data: ARRAY OF LONGCOMPLEX);

    (* Fast Fourier transform of an array of N complex data points.     *)
    (* The result is returned in the same array.  N must be a power of  *)
    (* two.  This is based on the Sande variant of the FFT.             *)

    (* Status: Working. *)

    BEGIN
        FFT8G (Direct, N, data);
        Shuffle (N, data);
    END FFT8;

(************************************************************************)
(*              NAIVE FORM OF THE DISCRETE FOURIER TRANSFORM            *)
(************************************************************************)

PROCEDURE SlowFT (Direct: BOOLEAN;  N: CARDINAL;
                                VAR (*IN*) X: ARRAY OF LONGCOMPLEX;
                                VAR (*OUT*) A: ARRAY OF LONGCOMPLEX);

    (* For comparison: a Fourier transform by the slow method. *)

    VAR r, k: CARDINAL;  temp: LONGREAL;
        sum, power, weight: LONGCOMPLEX;

    BEGIN
        FOR r := 0 TO N-1 DO
            sum := CMPLX (0.0, 0.0);
            FOR k := 0 TO N-1 DO
                temp := 2.0*PI*VAL(LONGREAL,r)*VAL(LONGREAL,k)/VAL(LONGREAL,N);
                IF Direct THEN
                    power := CMPLX (0.0, -temp);
                ELSE
                    power := CMPLX (0.0, temp);
                END (*IF*);
                weight := exp (power);
                sum := sum + weight * X[k];
            END (*FOR*);
            temp := 1.0/VAL(LONGREAL,N);
            IF Direct THEN
                A[r] := sum;
            ELSE
                A[r] := scalarMult (temp, sum);
            END (*IF*);
        END (*FOR*);
    END SlowFT;

(************************************************************************)
(*                      FFT ON REAL INPUT DATA                          *)
(************************************************************************)

PROCEDURE RealFFT (Direct: BOOLEAN;  N: CARDINAL;
                                VAR (*INOUT*) data: ARRAY OF LONGREAL);

    (* Fast Fourier transform of an array of N real data points.        *)
    (* N must be a power of two.  The complex result is returned in the *)
    (* same array, by storing each complex number in two successive     *)
    (* elements of data (the real part first, then the imaginary part). *)
    (* This means that we can return only (N DIV 2) answers, but this   *)
    (* is usually acceptable: because of the symmetries for real input  *)
    (* data, it suffices to confine our attention to the positive half  *)
    (* of the frequency spectrum.                                       *)

    (* Status: working.  *)

    TYPE CxPtr = POINTER TO ARRAY [0..2048] OF LONGCOMPLEX;

    CONST C1 = 0.5;

    VAR p: CxPtr;  k1, k2: CARDINAL;
        sumr, sumi, diffr, diffi, WR, WI, KR, KI, C2: LONGREAL;

    BEGIN
        IF N < 2 THEN RETURN END(*IF*);
        p := ADR(data);
        N := N DIV 2;
        sumr := PI / VAL(LONGREAL, N);
        IF Direct THEN
            C2 := -0.5;
            FFT3 (TRUE, N, p^);
        ELSE
            C2 := 0.5;
            sumr := -sumr;
        END (*IF*);

        IF N > 2 THEN
            KR := Sin(0.5*sumr);  KR := 2.0*KR*KR;  WR := 1.0 - KR;
            KI := Sin(sumr);  WI := -KI;
            k2 := 2*(N-1);
            FOR k1 := 2 TO N-2 BY 2 DO
                sumr := C1*(data[k1] + data[k2]);
                sumi := C1*(data[k1+1] - data[k2+1]);
                diffr := -C2*(data[k1+1] + data[k2+1]);
                diffi := C2*(data[k1] - data[k2]);
                data[k1] := sumr + WR*diffr - WI*diffi;
                data[k1+1] := sumi + WR*diffi + WI*diffr;
                data[k2] := sumr - WR*diffr + WI*diffi;
                data[k2+1] := -sumi + WR*diffi + WI*diffr;
                sumr := WR - WR*KR + WI*KI;
                WI := WI - WR*KI - WI*KR;
                WR := sumr;
                DEC (k2, 2);
            END (*FOR*);
        END (*IF*);
        IF N > 1 THEN data[N+1] := -data[N+1] END(*IF*);
        sumr := data[0];
        data[0] := sumr + data[1];
        data[1] := sumr - data[1];

        IF NOT Direct THEN
            data[0] := 0.5*data[0];  data[1] := 0.5*data[1];
            FFT3 (FALSE, N, p^);
        END (*IF*);

    END RealFFT;

(************************************************************************)

END Fourier.

