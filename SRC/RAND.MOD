IMPLEMENTATION MODULE Rand;

        (********************************************************)
        (*                                                      *)
        (*              Random number generator                 *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        7 August 1996                   *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (*      The algorithm is the method of Pierre L'Ecuyer, *)
        (*      Efficient and Portable Combined Random Number   *)
        (*      Generators, CACM 31(6), June 1988, 742-749.     *)
        (*      This is his version for 32-bit machines.        *)
        (*                                                      *)
        (*      This version is about 50% slower than the       *)
        (*      PMOS random number generator, but it's more     *)
        (*      portable.  I haven't done enough tests to       *)
        (*      judge which one is "more random".               *)
        (*                                                      *)
        (********************************************************)

VAR s1, s2: INTEGER;

    (* These are the two seed values.  The basis of the algorithm       *)
    (* below is to use two separate random number generators, and       *)
    (* to combine their output; the result is a random number stream    *)
    (* with a very long cycle time.                                     *)

(************************************************************************)

PROCEDURE RANDOM(): REAL;

    CONST scale = 4.656613E-10;

    VAR Z, k: INTEGER;

    BEGIN
        k := s1 DIV 53668;
        s1 := 40014 * (s1 - k*53668) - k*12211;
        IF s1 < 0 THEN INC (s1, 2147483563) END(*IF*);

        k := s2 DIV 52774;
        s2 := 40692 * (s2 - k*52774) - k*3791;
        IF s2 < 0 THEN INC (s2, 2147483399) END(*IF*);

        Z := s1 - s2;
        IF Z < 1 THEN INC (Z,  2147483562) END(*IF*);

        RETURN scale*FLOAT(Z);

    END RANDOM;

(************************************************************************)

BEGIN
    s1 := 1;  s2 := 1;
END Rand.

