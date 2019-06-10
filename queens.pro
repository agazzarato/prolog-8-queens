% Anthony Gazzarato
% CS-231 Honors Project
% 8 Queens in Prolog

% Check if two coordinate pairs share a row/column/diagonal.
sameCol((X1, _), (X2, _)) :- X1 == X2.
sameRow((_, Y1), (_, Y2)) :- Y1 == Y2.
sameDiagonal((X1, Y1), (X2, Y2)) :-
    not(sameCol((X1, Y1), (X2,Y2))),
    DY is Y2-Y1, DX is X2-X1,
    (divmod(DY, DX, 1, 0) ; divmod(DY, DX, -1, 0)).

% Plots the list of coordinate pairs Qs on an N-by-N grid.
plotBoard(N, Qs) :-
    forall(
        between(1, N, R),
        forall(between(1, N, C), member((C, R), Qs) -> write('Q|') ; write(' |')) -> write('\n')
    ).

% Base case; no queens means that all of the nonexistent queens are safe.
safeQueens([], _, _).
% Systematically eliminate possible rows and columns until none remain, then validate diagonals.
safeQueens([Q|Qs], Rows, Cols) :-
    % Position Q within the possible rows/columns.
    (X, Y) = Q,
    member(X, Cols), 
    member(Y, Rows),
    % Remove selected row/column from possible selections.
    subtract(Cols, [X], NewCols),
    subtract(Rows, [Y], NewRows),
    % Need to get rest of queens before we can check diagonals.
    safeQueens(Qs, NewRows, NewCols),
    % Now check diagonals
    forall(member((QX, QY), Qs), not(QX < X ; sameDiagonal(Q, (QX, QY)))).

% Probably the query you'll want to run.
nQueens(N) :- nQueens(N, Qs), plotBoard(N, Qs).
% The query that actually finds N queens and lists them in Qs.
nQueens(N, Qs) :-
    length(Qs, N),
    findall(Row, between(1, N, Row), Rows),
    findall(Col, between(1, N, Col), Cols),
    safeQueens(Qs, Rows, Cols).