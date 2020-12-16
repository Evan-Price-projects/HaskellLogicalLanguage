children(lee, [virginia, bob, suze]).
children(suzy, [chris, jamie]).

children(suzy, [chris, jamie])?           % Yes.
children(lee, [virginia, bob])?           % No.
children(lee, [bob, suzy])?               % No.
children(lee, [bob, virginia, suzy])?     % No.
