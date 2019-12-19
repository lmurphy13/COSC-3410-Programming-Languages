%
% membr(X,L) is true if X is an element of the list L.
%
% (funny name used to avoid built-in predicate)

membr(Element, [Element | _] ).
membr(Element, [_ | List]) :- membr(Element, List).
