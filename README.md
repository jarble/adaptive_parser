# adaptive_parser
This is a very simple [semantic parser](https://en.wikipedia.org/wiki/Semantic_parsing) in Prolog, implemented using an [adaptive grammar](https://en.wikipedia.org/wiki/Adaptive_grammar). In other words, this parser is able to "learn" new grammar rules from its input.

The grammar rules for this parser are defined in [input_file.txt](input_file.txt).

This parser is relatively slow, but it could be much faster if it were re-written as an adaptive [Earley parser](https://en.wikipedia.org/wiki/Earley_parser).
