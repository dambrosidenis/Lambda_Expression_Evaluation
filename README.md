# Evaluating Untyped Lambda-Expressions in Haskell

GitHub repository for a project about comparing various methods for evaluating untyped lambda expressions. The algorithms are implemented in Haskell, whereas the lexer/parser stack chosen is Alex+Happy. The complete though process for this project, along with a brief introduction to the world of lambda calculus, is described in the article and presentation.

## Table of Contents

- [Introduction](#introduction)
- [Repository Structure](#repository-structure)
- [Getting Started](#getting-started)
- [Article](#article)
- [Presentation](#presentation)
- [Source Code](#source-code)
    - [Usage](#usage)
    - [Test Generation](#test-generation)
- [Citation](#citation)
- [License](#license)

## Introduction

This repository explores different methods for evaluating lambda expressions, including two algebraic and a computational approach. It includes an article detailing the theoretical aspects, a presentation summarizing key points, and the source code to experiment with the various evaluation methods described.

## Repository Structure

The repository is organized as follows:

```
.
├── article
│   ├── article.pdf
│   └── src
│       ├── main.tex
│       └── references.bib
├── presentation
│   ├── presentation.pdf
│   └── presentation.pptx
├── src
│   ├── AlgebraicAlphaConversion.hs
│   ├── AlgebraicDeBrujin.hs
│   ├── Computational.hs
│   ├── LambdaTerm.hs
│   ├── parser.hs
│   ├── parser.y
│   ├── scanner.hs
│   ├── scanner.x
│   └── test
│       └── testGenerator.py
├── README.md
├── CITATION.cff
└── LICENSE
```

- `article/`: Contains the article PDF and its source files.
- `presentation/`: Contains the presentation in PDF and PPTX formats.
- `src/`: Contains the source code for different methods of evaluating lambda expressions.
- `README.md`: This README file.
- `CITATION.cff`: Citation information for this repository.
- `LICENSE`: The license under which this project is distributed.

## Getting Started

To explore the contents of this repository, follow the steps below:

1. **Clone the Repository:**

   ```bash
   git clone https://github.com/dambrosidenis/Lambda_Expression_Evaluation.git
   cd Lambda_Expression_Evaluation
   ```

2. **View the Article:**

   Open the `article/article.pdf` file to read the full article.

3. **View the Presentation:**

   Open the `presentation/presentation.pdf` or `presentation/presentation.pptx` file to review the presentation.

4. **Explore the Source Code:**

   Navigate to the `src/` directory to review the source code implementing various methods for evaluating lambda expressions. More information below.

## Article

The detailed article explaining the different methods for evaluating lambda expressions can be found in the `article/` directory:

- [Article PDF](article/article.pdf)

The source files for the article, including LaTeX and bibliography, are located in `article/src/`.

## Presentation

The presentation summarizing the key points of the project can be found in the `presentation/` directory:

- [Presentation PDF](presentation/presentation.pdf)
- [Presentation PPTX](presentation/presentation.pptx)

## Source Code

The `src/` directory contains the source code for various methods of evaluating lambda expressions, including:

- Algebraic Alpha Conversion method (`AlgebraicAlphaConversion.hs`)
- Algebraic De Bruijn method (`AlgebraicDeBrujin.hs`)
- Computational Evaluation method (`Computational.hs`)
- Common algebraic Lambda Term Definitions to parse the terms (`LambdaTerm.hs`)
- Parser written in Happy (`parser.y`) and compiled (`parser.hs`)
- Scanner written in Alex (`scanner.x`) and compiled (`scanner.hs`)
- Test instance Generator (`test/testGenerator.py`)

### Usage

To run the program on an input file containing various lambda expression to evaluate, it is sufficient to execute

```bash
ghc parser.hs
./parser <input-file>
```

If, on the other hand, you prefer to recompile the lexer and the parser, you have to execute

```bash
alex scanner.x
```

to compile the scanner into `scanner.hs`, and then

```bash
happy parser.y
```

to obtain `parser.hs`.

### Test Generation

The parser takes as input a text file with a series of lambda expressions. If you want to check your own expressions, the correct syntax for the file is unambiguously described in the [article](article/article.pdf). If, on the other hand, you are good with randomly generated instances, you can use the Python script `testGeneration.py` to generate a list of random expressions obtained through a random walk on the grammar. The usage of the file is the following:

```bash
python3 testGeneration.py <output-file> <number-of-expressions> <max-depth>
```

where:
- `output-file` is the name of the file where you want to save the expressions
- `number-of-expressions` is the number of examples you want
- `max-depth` specifies the highest level of term nesting possible in order to avoid excessively lengthy expressions

> Note that in absence of any of the arguments, the script will use the default values `./test.txt`, `10` and `5`

## Citation

If you use this work in your research, please cite it as follows:

```
@software{D_Ambrosi_Evaluating_Untyped_Lambda_2023,
    author = {D'Ambrosi, Denis},
    month = sep,
    title = {{Evaluating Untyped Lambda Expressions in Haskell}},
    url = {https://github.com/dambrosidenis/Lambda_Expression_Evaluation},
    version = {1.0.0},
    year = {2023}
}
```

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.