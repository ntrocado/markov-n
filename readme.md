# markov-n

*n*th order Markov chains on sample level.

## Usage

Load with:
```common lisp
(asdf:load-system "markov-n")
```
and run:
```common lisp
(markov-n:main <order> <input file> <output file> <size>)
```
where `order` is the number of previous states considered in the transition matrix, `input file` and `output file` are headless signed 16-bit PCM audio file paths and `size` is the intended size of the output file, in bytes.

## Author

[Nuno Trocado](http://ntrocado.com)

## License

This project is licensed under the GPL 3.0 license.