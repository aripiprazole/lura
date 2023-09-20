## Lura

Incremental/single-pass based compiler, the API can be either used for Single-Pass Compiling and for building LSP, or
things that would need incremental pipelines. It's a study project of mine for studying incremental compilers and
package-managers.

The project is still being developed, and the table of contents is:

- [Garbage Collection](#garbage-collection)
- [License](#license)

The objective of this project is:

- Dealing with language-servers, and direct support for [Visual Studio Code](https://code.visualstudio.com/)
- Having a full-featured CLI for building, and packaging libraries for Lura
- Lowering to [LLVM-IR](https://llvm.org/), and executing JIT
- Error-resistant compiler, like having a compile-time error, and still can compile, because the error fall onto the
  runtime
- Interpreter for basic things like macro-expanding.
- Query-based architecture.

It's all based on the works:

- [Resilient parsing](https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html)
- [Typing Haskell in Haskell](http://web.cecs.pdx.edu/~mpj/thih/thih.pdf)
- [Sixty](https://github.com/ollef/sixty)
- [Query-based compiler architecture](https://ollef.github.io/blog/posts/query-based-compilers.html)
- [Rust](https://github.com/rust-lang/rust)
- [Crafting Interpreters](https://craftinginterpreters.com)
- [Semantic](https://github.com/github/semantic)
- [Data types à la carte](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/data-types-a-la-carte/14416CB20C4637164EA9F77097909409)
- [Salsa](https://salsa-rs.github.io/salsa)
- [OutsideIn(X)](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=6c214019a649c52341d92bd73140de7ad0c292f0)
- [How to implement dependent types in 80 lines of code](https://gist.github.com/Hirrolot/27e6b02a051df333811a23b97c375196)
- [Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism](https://www.cl.cam.ac.uk/~nk480/bidir.pdf)
- [Bidirectional typechecking for higher-rank polymorphism](https://gist.github.com/mb64/87ac275c327ea923a8d587df7863d8c7#file-tychk_v2-ml)
- [Practical type inference for arbitrary-rank types](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/putting.pdf)

And have some great inspirations in [rust-analyzer](https://github.com/rust-lang/rust-analyzer) code-base..

### Running

Clone the repository with `git submodule update --recursive --remote`

### Garbage-Collection

The goal of this compiler isn't optimizing things, it's more like an IDE, so the garbage collector is based on Reference
Counting.

### License

Lura is distributed under the terms of the [MIT license](LICENSE).
