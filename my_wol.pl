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
min_moves([[_, _, R1, C1, R2, C2]], [R1, C1, R2, C2]).
min_moves([[_, N, R1, C1, R2, C2], [_, M, R21, C21, R22, C22] | T], Res) :-
  (N < M) -> min_moves([[_, N, R1, C1, R2, C2] | T], Res); 
              min_moves([[_, M, R21, C21, R22, C22] | T], Res).

max_moves([], []).
max_moves([[_, _, R1, C1, R2, C2]], [R1, C1, R2, C2]).
max_moves([[N, _, R1, C1, R2, C2], [M, _, R21, C21, R22, C22] | T], Res) :-
  (N > M) -> max_moves([[N, _, R1, C1, R2, C2] | T], Res);
              max_moves([[M, _, R21, C21, R22, C22] | T], Res).

diff_moves([], []).
diff_moves([[_, _, R1, C1, R2, C2]], [R1, C1, R2, C2]).
diff_moves([[Curr1, Other1, R1, C1, R2, C2], [Curr2, Other2, R21, C21, R22, C22] | T], Res) :-
  ((Curr1 - Other1) > (Curr2 - Other2)) ->
    diff_moves([[Curr1, Other1, R1, C1, R2, C2] | T], Res);
    diff_moves([[Curr2, Other2, R21, C21, R22, C22] | T], Res).

player_pieces(r, [_, R], R).
player_pieces(b, [B, _], B).

opponent(r, b).
opponent(b, r).

opp_pieces(P, Board, Pieces) :- 
  player_pieces(opponent(P), Board, Pieces).

% Adapter for call to next_generation when the player is unknown in the
% function call 
next_gen(b, [P1, P2], [B, R]) :-
  next_generation([P1, P2], [B, R]).
next_gen(r, [P1, P2], [B, R]) :-
  next_generation([P2, P1], [R, B]).

intermediary_board(r, NewPieces, [B, _], [B, NewPieces]).
intermediary_board(b, NewPieces, [_, R], [NewPieces, R]).

alter_brd(P, [R1, C1, R2, C2], Board, NewBoard) :-
  player_pieces(P, Board, PlayerPieces),
  alter_board([R1, C1, R2, C2], PlayerPieces, NewPlayerPieces),
  intermediary_board(P, NewPlayerPieces, Board, NewBoard).

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

find_best_move(Player, [B, R], Strategy, Best_Move) :-
  player_pieces(Player, [B, R], P1),
  opp_pieces(Player, [B, R], P2),
  possible_moves(P1, P2, Moves),
  setof( 
    [Score, X, Y, New_X, New_Y], 
    ( member([X, Y, New_X, New_Y], Moves),
      alter_board([X, Y, New_X, New_Y], P1, Int_P1), 
      next_gen(Player, [Int_P1, P2], [New_P1, New_P2]), 
      eval_move([New_P1, New_P2], Strategy, Score)       
    ),
    Scored_Moves
  ),
  head(Scored_Moves).

eval_move([P1, P2], bldlust, Score) :-
  length(P2, L2),
  Score is 64 - L2.
  
eval_move([P1, P2], selfpres, Score) :-
  length(P1, L1),
  Score = L1.

eval_move([P1, P2], lndgrab, Score) :-
  length(P1, L1),
  length(P2, L2),
  Score is L1 - L2.

bloodlust(P, Board, NewBoard, [R1, C1, R2, C2]) :-
  find_best_move(P, Board, bldlust, [R1, C1, R2, C2]),
  alter_brd(P, [R1, C1, R2, C2], Board, NewBoard).        
  
selfpreservation(P, Board, NewBoard, [R1, C1, R2, C2]) :-
  find_best_move(r, Board, selfpres, [R1, C1, R2, C2]),
  alter_brd(P, [R1, C1, R2, C2], Board, NewBoard).  

landgrab(P, Board, NewBoard, [R1, C1, R2, C2]) :-
  find_best_move(r, Board, lndgrab, [R1, C1, R2, C2]),
  alter_brd(P, [R1, C1, R2, C2], Board, NewBoard).  

%% SKELETON DESIGN OF ALGORITHM FOR STRATEGIES
%blood_lust(Board, Best_Move) :-
%    find_best_move(Board, blood_lust_assess_move, Best_Move).

%find_best_move(Board, assess_move_function, Best_Move) :-
%    setof((Score, Move), assess_move_function(Board,Move,Score),Result),
%    Best_Move = get_head(Result).

%blood_lust_assess_move(Board, Move, Score) :-
%    make_move(Board, Move, New_Board),
%    Score is 64 - number_of_opponents(New_Board).
