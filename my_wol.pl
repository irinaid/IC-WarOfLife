test_strategy(0, _, _).
test_strategy(N, S1, S2) :-
    test_strategy_helper(N, 0, S1, S2, 0, 0, 0, 0, 0, 0, 0). 


test_strategy_helper(N, 0, S1, S2, W1, W2, D, LM, SM, SUM_MOV, SUM_TIME) :-  
    format('~nblue wins = ~w~nred wins= ~w~ndraws = ~w~n', [W1,W2,D]),
    format('~Shortest moves = ~w~nLonges moves = ~w~nAverage moves = ~w~n',
           [LM, SM, SUM_MOV / N]).
                                                                             
    format('~nblue score = ~w~nredscore = ~w~n~n', [1,2]).
test_strategy_helper(N, CNT, S1, S2, W1, W2, D, LM, SM, SUM_MOV, SUM_TIME) :-
    now(BEFORE),
    play(quiet, S1, S2, N, _),
    now(AFTER),
    NEW_TIME = SUM_TIME + (AFTER - BEFORE),
    
