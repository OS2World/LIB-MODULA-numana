IMPLEMENTATION MODULE LowLevel;

        (********************************************************)
        (*                                                      *)
        (*         Miscellaneous low-level procedures           *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        30 July 1996                    *)
        (*  Status:             Working on XDS port             *)
        (*                                                      *)
        (*      Now appears to be working, but:                 *)
        (*       (a) untested, more checking needed;            *)
        (*       (b) it's still not clear that what's           *)
        (*           provided is what the clients needed,       *)
        (*           particularly in relation to 16 bit/32 bit  *)
        (*           distinctions.                              *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT
    (* type *)  BYTE, CARD8, CARD16, INT16, WORD, ADDRESS,
    (* proc *)  CAST, ROTATE, SHIFT, ADDADR, SUBADR, MOVE, FILL;

FROM Types IMPORT
    (* type *)  FarPointer;

TYPE
    t01 = [0..1];  t02 = [0..2];  t03 = [0..3];  t04 = [0..4];

    Table = ARRAY [0..15] OF CARDINAL;

    Word8 =  RECORD
                CASE :t02 OF
                  | 0:  bits: SET OF [0..7];
                  | 1:  b: BYTE;
                  | 2:  c: CARD8;
                END (*CASE*);
             END (*RECORD*);

    Word16 =  RECORD
                CASE :t03 OF
                  | 0:  bits: BITSET;
                  | 1:  low, high: BYTE;
                  | 2:  w: CARD16;
                  | 3:  c: CARD16;
                END (*CASE*);
              END (*RECORD*);

    Word32 =  RECORD
                CASE :t04 OF
                  | 0:  bits: BITSET;
                  | 1:  low, high, higher, highest: BYTE;
                  | 2:  w: WORD;
                  | 3:  c: CARDINAL;
                  | 4:  a: ADDRESS;
                END (*CASE*);
              END (*RECORD*);

    (* The following needs fixing up, because with this compiler two    *)
    (* WORDs don't make a LONGCARD.  In fact I need to look at the      *)
    (* client modules to see what is really needed here.                *)

    Double = RECORD
                CASE :t02 OF
                  | 0:  low, high: CARD16;
                  | 1:  lw: CARDINAL;
                  | 2:  a: ADDRESS;
                END (*CASE*);
             END (*RECORD*);

CONST power2 = Table {1, 2, 4, 8, 16, 32, 64, 128, 256,
                     512, 1024, 2048, 4096, 8192, 16384, 32768};

(************************************************************************)
(*                      INTERNAL TYPE CONVERSIONS                       *)
(************************************************************************)

PROCEDURE ByteToWord (b: BYTE): WORD;

    (* Converts a byte to a word (with no sign extension). *)

    VAR result: Word32;

    BEGIN
        result.c := 0;
        result.low := b;
        RETURN result.w;
    END ByteToWord;

(************************************************************************)

PROCEDURE ByteToCard (b: BYTE): CARDINAL;

    (* Converts a byte to a cardinal. *)

    BEGIN
        RETURN VAL(CARDINAL,CAST(CARD8,b));
    END ByteToCard;

(************************************************************************)
(*                          BITWISE LOGIC                               *)
(************************************************************************)

PROCEDURE IAND (first, second: CARDINAL): CARDINAL;

    (* Bit-by-bit logical AND.  *)

    VAR a, b, result: Word32;

    BEGIN
        a.c := first;  b.c := second;
        result.bits := a.bits * b.bits;
        RETURN result.c;
    END IAND;

(************************************************************************)

PROCEDURE IANDB (first, second: BYTE): BYTE;

    (* Bit-by-bit logical AND for bytes.        *)

    BEGIN
        RETURN LowByte(IAND(ByteToCard(first), ByteToCard(second)));
    END IANDB;

(************************************************************************)

PROCEDURE IOR (first, second: WORD): WORD;

    (* Bit-by-bit inclusive OR. *)

    VAR a, b, result: Word32;

    BEGIN
        a.w := first;  b.w := second;
        result.bits := a.bits + b.bits;
        RETURN result.w;
    END IOR;

(************************************************************************)

PROCEDURE IORB (first, second: BYTE): BYTE;

    (* Bit-by-bit inclusive OR. *)

    BEGIN
        RETURN LowByte(IOR(ByteToWord(first), ByteToWord(second)));
    END IORB;

(************************************************************************)

PROCEDURE IXOR (first, second: WORD): WORD;

    (* Bit-by-bit exclusive OR. *)

    VAR a, b, result: Word32;

    BEGIN
        a.w := first;  b.w := second;
        result.bits := a.bits / b.bits;
        RETURN result.w;
    END IXOR;

(************************************************************************)

PROCEDURE IXORB (first, second: BYTE): BYTE;

    (* Bit-by-bit exclusive OR. *)

    BEGIN
        RETURN LowByte(IXOR(ByteToWord(first), ByteToWord(second)));
    END IXORB;

(************************************************************************)

PROCEDURE INOT (value: WORD): WORD;

    (* Bit-by-bit Boolean complement.   *)

    VAR temp: Word32;

    BEGIN
        temp.w := value;
        temp.c := MAX(CARDINAL)-temp.c;
        RETURN temp.w;
    END INOT;

(************************************************************************)

PROCEDURE INOTB (value: BYTE): BYTE;

    (* Bit-by-bit Boolean complement.   *)

    BEGIN
        RETURN LowByte(0FFH-CAST(CARD8,value));
    END INOTB;

(************************************************************************)

PROCEDURE ROL (value: WORD;  count: CARDINAL): WORD;

    (* Left rotation of "value" by "count" bit positions.       *)

    VAR temp: Word32;

    BEGIN
        count := count MOD 32;
        temp.w := value;
        temp.bits := ROTATE (temp.bits, VAL(INTEGER,count));
        RETURN temp.w;
    END ROL;

(************************************************************************)

PROCEDURE ROLB (value: BYTE;  count: CARDINAL): BYTE;

    (* Left rotation of "value" by "count" bit positions.       *)

    VAR temp: Word8;

    BEGIN
        count := count MOD 8;
        temp.b := value;
        temp.bits := ROTATE (temp.bits, VAL(INTEGER,count));
        RETURN temp.b;
    END ROLB;

(************************************************************************)

PROCEDURE LS (value: WORD;  count: CARDINAL): WORD;

    (* Left shift of "value" by "count" bit positions, with zero fill.  *)

    VAR temp: Word32;

    BEGIN
        temp.w := value;
        IF count > 31 THEN temp.c := 0;
        ELSIF count > 0 THEN
            temp.bits := SHIFT (temp.bits, VAL(INTEGER,count));
        END (*IF*);
        RETURN temp.w;
    END LS;

(************************************************************************)

PROCEDURE LSB (value: BYTE;  count: CARDINAL): BYTE;

    (* Left shift of "value" by "count" bit positions, with zero fill.  *)

    VAR temp: Word8;

    BEGIN
        temp.b := value;
        IF count > 7 THEN temp.c := 0;
        ELSIF count > 0 THEN
            temp.bits := SHIFT (temp.bits, VAL(INTEGER,count));
        END (*IF*);
        RETURN temp.b;
    END LSB;

(************************************************************************)

PROCEDURE ROR (value: WORD;  count: CARDINAL): WORD;

    (* Right rotation of "value" by "count" bit positions.      *)

    VAR temp: Word32;

    BEGIN
        count := count MOD 32;
        temp.w := value;
        temp.bits := ROTATE (temp.bits, -VAL(INTEGER,count));
        RETURN temp.w;
    END ROR;

(************************************************************************)

PROCEDURE RORB (value: BYTE;  count: CARDINAL): BYTE;

    (* Right rotation of "value" by "count" bit positions.      *)

    VAR temp: Word8;

    BEGIN
        count := count MOD 8;
        temp.b := value;
        temp.bits := ROTATE (temp.bits, -VAL(INTEGER,count));
        RETURN temp.b;
    END RORB;

(************************************************************************)

PROCEDURE RS (value, count: CARDINAL): CARDINAL;

    (* Right shift of "value" by "count" bit positions, with zero fill. *)

    VAR temp: Word32;

    BEGIN
        temp.c := value;
        IF count > 31 THEN temp.c := 0;
        ELSIF count > 0 THEN
            temp.bits := SHIFT (temp.bits, -VAL(INTEGER,count));
        END (*IF*);
        RETURN temp.c;
    END RS;

(************************************************************************)

PROCEDURE RSB (value: BYTE;  count: CARDINAL): BYTE;

    (* Right shift of "value" by "count" bit positions, with zero fill. *)

    VAR temp: Word8;

    BEGIN
        temp.b := value;
        IF count > 7 THEN temp.c := 0;
        ELSIF count > 0 THEN
            temp.bits := SHIFT (temp.bits, -VAL(INTEGER,count));
        END (*IF*);
        RETURN temp.b;
    END RSB;

(************************************************************************)
(*                          POINTER OPERATIONS                          *)
(*   Remark: this group of procedures was originally designed for a     *)
(*   segmented memory model.  XDS uses flat 32-bit addresses, and we    *)
(*   handle this by treating the segment part of an address as a dummy. *)
(************************************************************************)

PROCEDURE Far (A: ADDRESS): FarPointer;

    (* Converts a pointer to a far pointer. *)

    BEGIN
        RETURN A;
    END Far;

(************************************************************************)

PROCEDURE MakePointer (segment, offset: WORD): FarPointer;

    (* Creates a pointer, given the segment and offset within segment.  *)

    VAR value: Word32;

    BEGIN
        value.w := offset;
        RETURN value.a;
    END MakePointer;

(************************************************************************)

PROCEDURE SEGMENT (A: ADDRESS): CARD16;

    (* Returns the segment part of an address.  *)

    BEGIN
        RETURN 0;
    END SEGMENT;

(************************************************************************)

PROCEDURE FarSEGMENT (A: FarPointer): CARD16;

    (* Returns the segment part of an address.  *)

    BEGIN
        RETURN 0;
    END FarSEGMENT;

(************************************************************************)

PROCEDURE OFFSET (A: ADDRESS): WORD;

    (* Returns the offset part of an address.   *)

    VAR value: Word32;

    BEGIN
        value.a := A;
        RETURN value.w;
    END OFFSET;

(************************************************************************)

(*
PROCEDURE Virtual (PA: LONGCARD): FarPointer;

    (* Converts a physical address to a virtual address, if possible.   *)
    (* There are no guarantees in the case where there is no such       *)
    (* virtual address.                                                 *)

    CONST Sixteen = (*<FST 16L >*)  (*<~FST*) 16 (*>*);

    VAR value: Double;

    BEGIN
        value.low := LowWord(PA MOD Sixteen);
        value.high := LowWord(PA DIV Sixteen);
        RETURN value.a;
    END Virtual;
*)
(************************************************************************)
(*
PROCEDURE Physical (A: ADDRESS): LONGCARD;

    (* Converts a virtual address to a physical address.  Use with care!*)
    (* WARNING: This is going to give the wrong answer in a protected   *)
    (* mode environment.  I'm still not sure how to fix this.           *)

    VAR value: Double;

    BEGIN
        value.a := A;
        RETURN 16*CARDINAL(value.high) + CARDINAL(value.low);
    END Physical;
*)
(************************************************************************)

PROCEDURE AddOffset (A: ADDRESS;  increment: CARDINAL): ADDRESS;

    (* Returns a pointer to the memory location whose physical address  *)
    (* is Physical(A)+increment.  In the present version, it is assumed *)
    (* that the caller will never try to run off the end of a segment.  *)

    BEGIN
        RETURN ADDADR (A, increment);
    END AddOffset;

(************************************************************************)

PROCEDURE SubtractOffset (A: ADDRESS;  decrement: CARDINAL): ADDRESS;

    (* Like AddOffset, except that we go backwards in memory.  Running  *)
    (* off the beginning of the segment is an undetected error.         *)

    BEGIN
        RETURN SUBADR (A, decrement);
    END SubtractOffset;

(************************************************************************)

PROCEDURE FarAddOffset (A: FarPointer;  increment: CARDINAL): FarPointer;

    (* Like AddOffset, except for the parameter types. *)

    BEGIN
        RETURN ADDADR (A, increment);
    END FarAddOffset;

(************************************************************************)

PROCEDURE FarSubtractOffset (A: FarPointer; decrement: CARDINAL): FarPointer;

    (* Like SubtractOffset, except for the parameter types. *)

    BEGIN
        RETURN SUBADR (A, decrement);
    END FarSubtractOffset;

(************************************************************************)
(*                      BYTE/WORD/LONGCARD CONVERSIONS                  *)
(************************************************************************)

PROCEDURE LowByte (w: WORD): BYTE;

    (* Returns the low-order byte of its argument.      *)

    VAR value: Word32;

    BEGIN
        value.w := w;
        RETURN value.low;
    END LowByte;

(************************************************************************)

PROCEDURE HighByte (w: WORD): BYTE;

    (* Returns the high-order byte of its argument.     *)

    VAR value: Word32;

    BEGIN
        value.w := w;
        RETURN value.high;
    END HighByte;

(************************************************************************)

PROCEDURE MakeWord (high, low: BYTE): CARD16;

    (* Combines two bytes into a word.  The first argument becomes the  *)
    (* most significant byte of the result.                             *)

    VAR value: Word16;

    BEGIN
        value.low := low;
        value.high := high;
        RETURN value.w;
    END MakeWord;

(************************************************************************)

PROCEDURE SignExtend (val: BYTE): INTEGER;

    (* Converts a signed 8-bit number to signed integer. *)

    VAR result: INTEGER;

    BEGIN
        result := ByteToCard(val);
        IF result > 127 THEN
            DEC (result, 256);
        END (*IF*);
        RETURN result;
    END SignExtend;

(************************************************************************)

(*
PROCEDURE LowWord (w: LONGCARD): WORD;

    (* Returns the low-order word of its argument.      *)

    VAR value: Double;

    BEGIN
        value.lw := w;
        RETURN ORD(value.low);
    END LowWord;
*)
(************************************************************************)
(*
PROCEDURE HighWord (w: LONGCARD): WORD;

    (* Returns the high-order word of its argument.     *)

    VAR value: Double;

    BEGIN
        value.lw := w;
        RETURN ORD(value.high);
    END HighWord;
*)
(************************************************************************)
(*
PROCEDURE MakeLongword (high, low: WORD): LONGCARD;

    (* Combines two words into a longword.  The first argument becomes  *)
    (* the most significant word of the result.                         *)

    VAR value: Double;

    BEGIN
        value.low := VAL(CARD16,low);
        value.high := VAL(CARD16,high);
        RETURN value.lw;
    END MakeLongword;
*)
(************************************************************************)
(*                      MISCELLANEOUS ARITHMETIC                        *)
(************************************************************************)

PROCEDURE INCV (VAR (*INOUT*) dest: CARDINAL;  src: CARDINAL): BOOLEAN;

    (* Computes dest := dest + src, and returns TRUE iff the addition   *)
    (* produced a carry.                                                *)

    BEGIN
        IF dest > MAX(CARDINAL) - src THEN
            DEC (dest, MAX(CARDINAL) - src + 1);
            RETURN TRUE;
        ELSE
            INC (dest, src);
            RETURN FALSE;
        END (*IF*);
    END INCV;

(************************************************************************)

PROCEDURE INCVB (VAR (*INOUT*) dest: BYTE;  src: BYTE): BOOLEAN;

    (* Computes dest := dest + src, and returns TRUE iff the addition   *)
    (* produced a carry.                                                *)

    VAR sum: CARDINAL;

    BEGIN
        sum := ByteToCard(dest) + ByteToCard(src);
        dest := LowByte (sum);
        RETURN sum>255;
    END INCVB;

(************************************************************************)

PROCEDURE DECV (VAR (*INOUT*) dest: CARDINAL;  src: CARDINAL): BOOLEAN;

    (* Computes dest := dest - src, and returns TRUE iff the            *)
    (* subtraction produced a borrow.                                   *)

    BEGIN
        IF dest < src THEN
            INC (dest, MAX(CARDINAL) - src + 1);  RETURN TRUE;
        ELSE
            DEC (dest, src);  RETURN FALSE;
        END (*IF*);
    END DECV;

(************************************************************************)

PROCEDURE DECVB (VAR (*INOUT*) dest: BYTE;  src: BYTE): BOOLEAN;

    (* Computes dest := dest - src, and returns TRUE iff the            *)
    (* subtraction produced a borrow.                                   *)

    VAR sval, dval: CARDINAL;

    BEGIN
        sval := ByteToCard (src);
        dval := ByteToCard (dest);
        IF dval < sval THEN
            dest := LowByte(dval + (256 - sval));
            RETURN TRUE;
        ELSE
            dest := LowByte(dval - sval);
            RETURN FALSE;
        END (*IF*);
    END DECVB;

(************************************************************************)

PROCEDURE Mul (A, B: CARD16): CARDINAL;

    (* Same as A*B, except for the type of the result.  We provide this *)
    (* as a general-purpose function since this combination of operands *)
    (* is often precisely what is wanted.                               *)

    BEGIN
        RETURN VAL(LONGCARD,A) * VAL(LONGCARD,B);
    END Mul;

(************************************************************************)

PROCEDURE MulB (A, B: BYTE): CARD16;

    (* Same as A*B, except for the type of the result.  We provide this *)
    (* as a general-purpose function since this combination of operands *)
    (* is often precisely what is wanted.                               *)

    BEGIN
        RETURN ByteToCard(A) * ByteToCard(B);
    END MulB;

(************************************************************************)

PROCEDURE IMul (A, B: INT16): INTEGER;

    (* Like Mul, but signed. *)

    BEGIN
        RETURN VAL(INTEGER,A) * VAL(INTEGER,B);
    END IMul;

(************************************************************************)

PROCEDURE IMulB (A, B: BYTE): INT16;

    (* Like MulB, but signed. *)

    BEGIN
        RETURN SignExtend(A) * SignExtend(B);
    END IMulB;

(************************************************************************)

PROCEDURE DivB (A: CARD16;  B: BYTE): BYTE;

    (* Same as A DIV B, except for the type of A.  We provide this as   *)
    (* a general-purpose function since this combination of operands    *)
    (* is often precisely what is wanted.                               *)

    BEGIN
        RETURN LowByte(A DIV ByteToCard(B));
    END DivB;

(************************************************************************)

PROCEDURE Div (A: CARDINAL;  B: CARD16): CARD16;

    (* Same as A DIV B, except for the type of A.  We provide this as   *)
    (* a general-purpose function since this combination of operands    *)
    (* is often precisely what is wanted.                               *)

    BEGIN
        RETURN VAL(CARD16, A DIV VAL(CARDINAL,B));
    END Div;

(************************************************************************)
(*                           BLOCK MOVES                                *)
(************************************************************************)

PROCEDURE Copy (source, destination: ADDRESS;  bytecount: CARDINAL);

    (* Copies an array of bytes from the source address to the          *)
    (* destination address.  In the case where the two arrays overlap,  *)
    (* the destination address should be lower in physical memory than  *)
    (* the source address.                                              *)

    BEGIN
        MOVE (source, destination, bytecount);
    END Copy;

(************************************************************************)

PROCEDURE FarCopy (source, destination: FarPointer;  bytecount: CARDINAL);

    (* Copies an array of bytes from the source address to the          *)
    (* destination address.  In the case where the two arrays overlap,  *)
    (* the destination address should be lower in physical memory than  *)
    (* the source address.                                              *)

    BEGIN
        MOVE (source, destination, bytecount);
    END FarCopy;

(************************************************************************)

PROCEDURE CopyUp (source, destination: FarPointer;  bytecount: CARDINAL);

    (* A variant of Copy which does the move backwards, in order        *)
    (* to handle the case where the destination address is inside the   *)
    (* source array.  In this special case Copy cannot be used,         *)
    (* because it would overwrite data it was about to copy.            *)

    BEGIN
        MOVE (source, destination, bytecount);
    END CopyUp;

(************************************************************************)

PROCEDURE BlockFill (destination: FarPointer;
                                bytecount: CARDINAL;  value: BYTE);

    (* Fills the destination array with the given value.        *)

    BEGIN
        FILL (destination, value, bytecount);
    END BlockFill;

(************************************************************************)

PROCEDURE BlockFillWord (destination: FarPointer;  wordcount: CARDINAL;
                                                        value: WORD);

    (* Fills the destination array with the given value.        *)

    VAR p: POINTER TO WORD;  j: CARDINAL;

    BEGIN
        p := destination;
        FOR j := 1 TO wordcount DO
            p^ := value;  p := AddOffset(p, 2);
        END (*FOR*);
    END BlockFillWord;

(************************************************************************)
(*                          INPUT AND OUTPUT                            *)
(************************************************************************)
(*
PROCEDURE OutByte (port: CARDINAL; value: BYTE);

    (* Puts the value out to an output port.    *)

    BEGIN
        (*<TopSpeed*) SYSTEM.Out (port, value); (*>*)
        (*<FST
        ASM
            MOV DX, port
            MOV AL, value
            OUT DX, AL
        END (*ASM*);
        >*)
    END OutByte;

(************************************************************************)

PROCEDURE InByte (port: CARDINAL): BYTE;

    (* Reads a byte from an input port. *)

    (*<FST VAR result: BYTE; >*)

    BEGIN
        RETURN SYSTEM.In (port);
    END InByte;

*)

(************************************************************************)
(*
PROCEDURE InStringWord (port: CARDINAL;  BufferAddress: ADDRESS;
                                                count: CARDINAL);

    (* Reads count words from an input port.    *)

    VAR j: CARDINAL;  p: POINTER TO WORD;
        lobyte, hibyte: BYTE;

    BEGIN
        p := BufferAddress;
        FOR j := 1 TO count DO
            p^ := SYSTEM.InW (port);
            Lib.IncAddr (p, 2);
        END (*FOR*);
    END InStringWord;
*)
(************************************************************************)
(*
PROCEDURE OutStringWord (port: CARDINAL;  BufferAddress: ADDRESS;
                                                count: CARDINAL);

    (* Writes count words to an output port.    *)

    VAR j: CARDINAL;  p: POINTER TO WORD;

    BEGIN
        p := BufferAddress;
        FOR j := 1 TO count DO
            SYSTEM.OutW (port, p^);
            Lib.IncAddr (p, 2);
        END (*FOR*);
    END OutStringWord;
*)
(************************************************************************)

END LowLevel.
