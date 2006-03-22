--: std.tm
--: Standard Tesla defining basic
--:   logic circuit components
--: Tesla v0.05
--: Agent2002. All rights reserved.
--: 04-11-13 04-12-08

--------------------------
-- Half Adder:
--------------------------
component HalfAdder( X, Y; Cout, Sum )
    Cout <= AND( X, Y );
    Sum  <= XOR( X, Y );
end component;

--------------------------
-- Full Adder:
--------------------------
component FullAdder( X, Y, Cin; Cout, Sum )
    signal sig1, sig2, sig3;
    Sum <= XOR( X, Y, Cin );
    sig1 <= AND( X, Y );
    sig2 <= AND( X, Cin );
    sig3 <= AND( Y, Cin );
    Cout <= OR( sig1, sig2, sig3 );
end component;

--------------------------
-- Set-Reset Latch:
--------------------------
component SRLatch( S, R; Q, Qp )
    Q  <= NOR( R, Qp );
    Qp <= NOR( S, Q  );
    Q <= [ 0@0 ];
    Qp <= [ 1@0 ];
end component;

--------------------------------------------
-- Set-Reset Latch with Additional Inputs:
--------------------------------------------
component SRLatchEx( S, R, PreN, ClrN; Q, Qp )
    signal Clr, Pre;
    Clr <= NOT( ClrN );
    Pre <= NOT( PreN );
    Q  <= NOR( Clr, R, Qp );
    Qp <= NOR( Pre, S, Q  );
    Q  <= [ 0@0 ];
    Qp <= [ 1@0 ];
end component;

--------------------------
-- Gated D Latch:
--------------------------
component DLatch( D, G; Q, Qp )
    signal S, R, Dp;
    Dp <= NOT( D );
    S  <= AND( D,  G );
    R  <= AND( Dp, G );
    SRLatch( S, R; Q, Qp );
end component;

component GatedDLatch( D, G; Q, Qp )
    DLatch( D, G; Q, Qp );
end component;

--------------------------------------------
-- Gated D Latch with Additional Inputs:
--------------------------------------------
component DLatchEx( D, G, PreN, ClrN; Q, Qp )
    signal S, R, Dp;
    Dp <= NOT( D );
    S  <= AND( D,  G );
    R  <= AND( Dp, G );
    SRLatchEx( S, R, PreN, ClrN; Q, Qp );
end component;

component GatedDLatchEx( D, G, PreN, ClrN; Q, Qp )
    DLatchEx( D, G, PreN, ClrN; Q, Qp );
end component;

--------------------------
-- Gated S-R Latch:
--------------------------
component GatedSRLatch( S, G, R; Q, Qp )
    signal S1, R1;
    S1 <= AND( S, G );
    R1 <= AND( R, G );
    SRLatch( S1, R1; Q, Qp );
end component;

---------------------------------
-- Edge-Triggered D Fip-Flop:
---------------------------------
component DFlipFlop( D, Ck; Q, Qp )
    signal CkN, P, Pp;
    CkN <= NOT( Ck );
    DLatch( D, CkN; P, Pp );
    DLatch( P, Ck;  Q, Qp );
end component;

---------------------------------------
-- D Fip-Flop with Additional Inputs:
---------------------------------------
component DFlipFlopEx( D, Ck, PreN, ClrN; Q, Qp )
    signal CkN, P, Pp;
    CkN <= NOT( Ck );
    DLatchEx( D, CkN, PreN, ClrN; P, Pp );
    DLatchEx( P, Ck,  PreN, ClrN; Q, Qp );
end component;

--------------------------
-- S-R Flip-Flop:
--------------------------
component SRFlipFlop( S, Ck, R; Q, Qp )
    signal CkN, S1, R1, P, Pp;
    CkN <= NOT( Ck );
    S1 <= AND( S, CkN );
    R1 <= AND( R, CkN );
    SRLatch( S1, R1; P, Pp );

    signal S2, R2;
    S2 <= AND( P,  Ck );
    R2 <= AND( Pp, Ck );
    SRLatch( S2, R2; Q, Qp );
end component;

--------------------------
-- J-K Flip-Flop:
--------------------------
component JKFlipFlop( J, Ck, K; Q, Qp )
    signal CkN, S1, R1, P, Pp;
    CkN <= NOT( Ck );
    S1 <= AND( Qp, J, CkN );
    R1 <= AND( Q,  K, CkN );
    SRLatch( S1, R1; P, Pp );

    signal S2, R2;
    S2 <= AND( P,  Ck );
    R2 <= AND( Pp, Ck );
    SRLatch( S2, R2; Q, Qp );
end component;

--------------------------
-- T Flip-Flop:
--------------------------
component TFlipFlop( T, Ck; Q, Qp )
    JKFlipFlop( T, Ck, T; Q, Qp );
end component;

