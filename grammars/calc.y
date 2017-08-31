%start Expr
%implicit_tokens WHITESPACE
%%
Expr : Term "PLUS" Expr | Term;
Term : Factor "MULT" Term | Factor;
Factor : "INT" | ;
