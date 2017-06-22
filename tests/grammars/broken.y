%start document
%%
document : paragraph "END_PARAGRAPH" | document;
paragraph : sentence | paragraph;
sentence : "WORD" | sentence;
