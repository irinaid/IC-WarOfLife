:- use_module(library(system)).
% Initialising variable LM to a very high value, so that it
% will be probable that some value in the "simulation" is
% smaller than it, therefore setting a new limit. (Ideally
% +infinty should be chosen).
test_strategy(0, _, _).
test_strategy(N, S1, S2) :-
    test_strategy_helper(N, N, S1, S2, 0, 0, 0, 0, 10000000, 0, 0). 


test_strategy_helper(N, 0, _, _, W1, W2, D, LM, SM, Sum_Mov, Sum_Time) :-  
    format('~nblue wins = ~w~nred wins= ~w~ndraws = ~w~n', [W1,W2,D]),
    Avg_Mov is Sum_Mov / N,
    format('~nShortest moves = ~w~nLonges moves = ~w~nAverage moves = ~w~n',
       [SM, LM, Avg_Mov]),
    Avg_Time is Sum_Time / N,
    format('~nAverage play time = ~w', [Avg_Time]).

test_strategy_helper(N, Cnt, S1, S2, W1, W2, D, LM, SM, Sum_Mov, Sum_Time) :-
    now(Before),
    play(quiet, S1, S2, Moves, Winner),
    now(After),
    New_Sum_Time is Sum_Time + (After - Before),
    update_score(Winner, Upd_W1, Upd_W2, Upd_D),
    New_W1 is (Upd_W1 + W1),
    New_W2 is (Upd_W2 + W2),
    New_D is (Upd_D + D),
    ((Moves < SM) -> (New_SM is Moves); (New_SM is SM)),
    ((Moves > LM) -> (New_LM is Moves);(New_LM is LM)),
    New_Sum_Mov is (Sum_Mov + Moves),
    New_Cnt is (Cnt - 1),
    test_strategy_helper(N,New_Cnt,S1,S2,New_W1,New_W2,New_D,New_LM,New_SM, New_Sum_Mov, New_Sum_Time).
    
update_score(Winner, B, R, D) :-
    ((Winner == r) -> (R is 1); (R is 0)),
    ((Winner == b) -> (B is 1); (B is 0)),
    ((Winner == d) -> (D is 1);
     (Winner == s) -> (D is 1);(D is 0)).

in_range(Min,Max,Min) :- Min =< Max.
in_range(Min,Max,N) :-
   Min < Max,
   MinX is Min+1,
   in_range(MinX,Max,N).


forall(P, Q) :- \+ ( call(P), \+ call(Q) ).

min_moves([], []). 
min_moves([[_, R1, C1, R2, C2]], [R1, C1, R2, C2]).
min_moves([[N, R1, C1, R2, C2], [M, R21, C21, R22, C22] | T], Res) :-
  (N < M) -> min_moves([[N, R1, C1, R2, C2] | T], Res); 
              min_moves([[M, R21, C21, R22, C22] | T], Res).

possible_moves(CurrentPlayer, OtherPlayer, Moves) :-
  findall([X, Y, NewX, NewY],
         (member([X,Y], CurrentPlayer),
          in_range(1, 8, NewX),
          in_range(1, 8, NewY),
          neighbour_position(X, Y, [NewX, NewY]),
          \+ member([NewX, NewY], CurrentPlayer),
          \+ member([NewX, NewY], OtherPlayer)
         ),
          Moves).

find_best_move(r, [B, R], Best_Move) :-
  possible_moves(R, B, Moves),
  findall( 
    [N, X, Y, NewX, NewY], 
    ( member([X, Y, NewX, NewY], Moves),
      alter_board([X, Y, NewX, NewY], R, IntR), 
      next_generation([B, IntR], [NewB, _]), 
      length(NewB, N)
    ),
    Counted_Moves
  ),
  min_moves(Counted_Moves, Best_Move).

find_best_move(b, [B, R], Best_Move) :-
  possible_moves(B, R, Moves),
  findall(
    [N, X, Y, NewX, NewY],
    ( member([X, Y, NewX, NewY], Moves),
      alter_board([X, Y, NewX, NewY], B, IntB),
      next_generation([IntB, R], [_, NewR]),
      length(NewR, N)
    ),
    Counted_Moves
  ),
  min_moves(Counted_Moves, Best_Move).

bloodlust(r, [B, R], [B, NewR], [R1, C1, R2, C2]) :-
  find_best_move(r, [B, R], [R1, C1, R2, C2]),
  alter_board([R1, C1, R2, C2], R, NewR).
        
bloodlust(b, [B, R], [NewB, R], [R1, C1, R2, C2]) :-
  find_best_move(b, [B, R], [R1, C1, R2, C2]),
  alter_board([R1, C1, R2, C2], B, NewB).

%% SKELETON DESIGN OF ALGORITHM FOR STRATEGIES
%blood_lust(Board, Best_Move) :-
%    find_best_move(Board, blood_lust_assess_move, Best_Move).

%find_best_move(Board, assess_move_function, Best_Move) :-
%    setof((Score, Move), assess_move_function(Board,Move,Score),Result),
%    Best_Move = get_head(Result).

%blood_lust_assess_move(Board, Move, Score) :-
%    make_move(Board, Move, New_Board),
%    Score is 64 - number_of_opponents(New_Board).
