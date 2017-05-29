%start document
%%
document : paragraph "END" | paragraph "END" document;
paragraph : "WORD" | "WORD" "SPACE" paragraph;
