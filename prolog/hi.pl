:- use_module(library(clpfd)).

s :-
(T4bs in 0),
(T4ints in 0),
(T6bs in (0)..(520)),
(T6ints in (-549755813887)..(549755813887)),
(T23bs in (0)..(1)),
(T23ints in (0)..(1)),
(T14bs in (0)..(1)),
(T14ints in (0)..(1)),
(T2bs in 4),
(T2ints in 499999998),
(T7bs in (0)..(4)),
(T7ints in (0)..(4294967295)),
(T26bs in (0)..(1)),
(T26ints in (0)..(1)),
(T24bs in (0)..(1)),
(T24ints in (0)..(1)),
(T15bs in (0)..(1)),
(T15ints in (0)..(1)),
(T0bs in (0)..(4)),
(T0ints in 500000000),
(T17bs in (0)..(1)),
(T17ints in (0)..(1)),
(T22bs in (0)..(1)),
(T22ints in (0)..(1)),
(T13bs in (0)..(1)),
(T13ints in (0)..(1)),
(T16bs in (0)..(1)),
(T16ints in (0)..(1)),
(T20bs in (0)..(1)),
(T20ints in (0)..(1)),
(T11bs in (0)..(1)),
(T11ints in (0)..(1)),
(T1bs in 4),
(T1ints in 500000000),
(T25bs in (0)..(1)),
(T25ints in (0)..(1)),
(T21bs in (0)..(1)),
(T21ints in (0)..(1)),
(T12bs in (0)..(1)),
(T12ints in (0)..(1)),
(T19bs in (0)..(1)),
(T19ints in (0)..(1)),
(T10bs in (0)..(1)),
(T10ints in (0)..(1)),
(T18bs in (0)..(4)),
(T18ints in (-2147483647)..(2147483647)),
(T9bs in (0)..(4)),
(T9ints in (-2147483647)..(2147483647)),
(T3bs in (0)..(4)),
(T3ints in (-2147483647)..(2147483647)),
(T6bs #\= 0),
((T2ints #< T7ints) #/\ (((T2ints #>= T0ints) #/\ (T7ints #>= T0ints) #/\ 1) #\/ ((T2ints #< T0ints) #/\ (T7ints #< T0ints) #/\ 1) #\/ 0) #/\ 1),
((T1ints #< T7ints) #/\ (((T1ints #>= T0ints) #/\ (T7ints #>= T0ints) #/\ 1) #\/ ((T1ints #< T0ints) #/\ (T7ints #< T0ints) #/\ 1) #\/ 0) #/\ 1),
(T18ints #= (T9ints /\ 0x00400000)),
(#\ 0).
