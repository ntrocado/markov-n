# [DEPRECATED]

_This project is not going to be maintained or developed further. Use [cl-markov-chains](https://github.com/ntrocado/cl-markov-chains "Markov chains in Common Lisp") instead, which offers the same basic functionality, and e.g. [cl-wave](https://github.com/RyanTKing/cl-wave) if you need to read/write .wav files._


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
where `order` is the number of previous states considered in the transition matrix, `input file` and `output file` are paths to header-less signed 16-bit PCM audio files, and `size` is the intended size of the output file, in bytes.

## Author

[Nuno Trocado](http://ntrocado.com)

## License

This project is licensed under the GPL 3.0 license.