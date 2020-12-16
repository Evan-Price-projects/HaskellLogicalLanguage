% Let's set up a family tree

parent(lee, virginia).
parent(lee, bob).
parent(lee, suzy).
parent(virginia, garrett).
parent(bob, robby).
parent(suzy, chris).
parent(suzy, jamie).

% Let's introduce some derived ideas.

grandparent(X,Z) :- parent(X,Y), parent(Y,Z).

grandparent(lee, garrett)?
        % Yes.
grandparent(virginia, garrett)?
        % No.
grandparent(lee, X)?
        % Yes:
        % X = garrett;
        % X = robby;
        % X = chris;
        % X = jamie.
grandparent(X, garrett)?
        % Yes:
        % X = lee.
grandparent(suzy, X)?
        % No.
