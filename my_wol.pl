test_strategy(0, _, _).
test_strategy(N, S1, S2) :-
	play(quiet, S1, S2, N, _),
    test_strategy(N-1, S1, S2).
