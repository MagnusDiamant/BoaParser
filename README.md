# BoaParser

## Lexical Specifications of Complex Terminals

There are three terminal symbols with non-trivial internal structure:

### 1. `ident`
   - **Identifiers** (used for variable and function names) consist of one or more letters, digits, and underscores, where the first character must not be a digit.
   - Additionally, an identifier must not be one of the Boa reserved words: `None`, `True`, `False`, `for`, `if`, `in`, and `not`.
   - For compatibility, it may be advisable to also avoid using other Python reserved words in Boa programs, but your parser should not treat those specially.

### 2. `numConst`
   - **Numeric constants** consist of an optional negative sign (`-`), followed (without any intervening whitespace) by one or more decimal digits.
   - The first digit must not be a zero unless it is the only digit.
   - Thus, “-0” or “100” are well-formed `numConsts`, while “007”, “+2”, or “- 4” are not.
   - Do not worry about Int-overflows.

### 3. `stringConst`
   - **String constants** in Boa are written between single quotes (`'`).
   - Inside a string constant, all printable ASCII characters are allowed, except for single quotes and backslashes, which must be escaped as `\'` and `\\`, respectively.
   - Raw newline characters (being non-printable) are not allowed, but can be included by the sequence `\n`.
   - A backslash followed immediately by a newline causes both characters to be ignored.
   - Thus, the Boa string constant `'fo\\o\ b\na\'r'` should be parsed as the 9 characters `fo\ob a'r` (where ‘<- ’ represents a single newline character).
   - All other escape sequences (i.e., a backslash followed by anything other than a single quote, another backslash, the lowercase letter ‘n’, or a newline) are illegal.
     
## General Lexical Conventions

- All identifiers and keywords are **case sensitive**.
- Tokens of the grammar may be surrounded by arbitrary **whitespace** (spaces, tabs, and newlines).
- Some whitespace is required between identifiers/keywords and any immediately following letters, digits, or underscores. 
  - No whitespace is needed after a numeric constant, unless it is immediately followed by another digit.

### Comments
- Comments start with a hash character (`#`) and run until the end of the line.
- A comment counts as whitespace for the purpose of separating tokens.
- For example, the input `not#comment cool` parses like `not cool`, not like `notcool`.
- Comments are not recognized within string constants, i.e., `#` behaves like any other character in that context.

### Indentation
- Unlike Python, Boa is not indentation-sensitive. Thus, there might be Boa programs that would not be valid Python programs due to, e.g., leading whitespace on a line.


## Disambiguation

The raw grammar of `Expr` is highly ambiguous. To remedy this, we specify the following operator precedences, from loosest to tightest grouping:

1. **The logical-negation operator `not`**
   - Nesting is allowed, so `not not x < 3` parses like `not (not (x < 3))`.

2. **All relational operators (`==`, etc., including `in` and `not in`)**
   - These are all non-associative, i.e., chains like `x < y < z` are syntactically illegal (unlike in Python).

3. **Additive arithmetic operators (`+` and `-`)**
   - These are left-associative, e.g., `x - y + z` parses like `(x - y) + z`.

4. **Multiplicative arithmetic operators (`*`, `//`, and `%`)**
   - These are also left-associative.

## Correspondence between Concrete and Abstract Syntax

- The relational operators `!=`, `<=`, `>=`, and `not in` should be treated as syntactic sugar for the immediate negations of `==`, `>`, `<`, and `in`, respectively.
  - For example, the input string `x <= y` should parse as the `Exp`-typed value `Not (Oper Greater (Var "x") (Var "y"))`.

- Numeric, string, and atomic constants should parse as the `Exp` constructor `Const` with a suitable `Value`-typed argument.
  - For example, the Boa expression `1 == True` should parse as `Oper Eq (Const (IntVal 1)) (Const TrueVal)`, and the Boa string constant `'a"b\n'` as `Const (StringVal "a\"b\n")`.

- On the other hand, list constructors (penultimate alternative for `Expr` in the grammar) should always parse as `List [...]`, never `Const (ListVal [...])`, even if all the list elements are already constants.

### Constraints in Syntax
- In the concrete syntax, a program must contain at least one statement, while in the abstract one, it is allowed to be completely empty.
- Likewise, the concrete syntax is more restrictive than the abstract one by specifying that a comprehension must always contain one or more clauses, starting with a `for`.
- Your parser should enforce these constraints, even though your interpreter was expected to also work without them.

