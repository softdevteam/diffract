%start Expr
%%
Expr : Term "PLUS" Expr | Term;
Term : Factor "MULT" Term | Factor;
Factor : "INT" | ;
