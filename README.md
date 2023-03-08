# coreLang
I am implementing the Core Language by Simon Peyton Jones and David Lester through their book "Implementing Functional Languages: A Tutorial"
(ISBN-13: 978-0137219520)

There is basically no new knowledge added by me in this repository, I am just following the book and learning. For some parts I also used the tutors guide found on the books [microsoft page](https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial/).

My goal is to learn some techniques for implementing functional languages, the book contains also a 'parallel G-Machine', which I will not implement. After finishing TIM I will try implementing the [Spineless Tagless G-Machine](https://www.microsoft.com/en-us/research/publication/implementing-lazy-functional-languages-on-stock-hardware-the-spineless-tagless-g-machine/), which is the underlying machine of Haskell, and according to this book combines principles of the G-Machine and TIM.

## Progress
- [X] Syntax, Lexer, Parser
- [X] Pretty Printer
- [X] Compiler frontend (callable program ./coreLang <inputfile> [cmdline options]
- [ ] Template instantiation (chapter 2)
  - [X] Mark 1: Minimal template instantiation graph reducer
  - [X] Mark 2: Adding let(rec)
  - [X] Mark 3: Adding updating
  - [X] Mark 4: Adding arithmetic
  - [ ] Mark 5: Structured data
- [X] G-Machine (Chapter 3)
  - [X] G1: Minimal G-Machine
  - [X] G2: Making it lazy
  - [X] G3: Adding let(rec)
  - [X] G4: Adding primitives
  - [X] G5: Better arithmetic
  - [X] G6: Adding data structures
  - [X] G7: Further improvements
- [ ] Three instruction machine
  - [X] TIM1: Minimal TIM
  - [X] TIM2: Adding arithmetic
  - [X] TIM3: Adding let(rec)
  - [ ] TIM4: Updating
  - [ ] TIM5: Structured data
  - [ ] TIM6: CAF
