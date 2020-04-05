# budget

## to do

- [X] total all categories at the bottom of the report "TOTAL         : 4807.00"
- [X] filter on one or several categories: `budget <CSVFILE> <CATEGORIES-FILE>` and total these categories

## how to run the tests

from the budget repository directory

    cd haskell/budget ⏎
    stack test ⏎

## how to install the executable

from the budget repository directory

    cd haskell ⏎
    stack build ⏎
    stack install ⏎

    Copied executables to /Users/<you>/.local/bin:
    - budget-exe

(make sure that your path contains `$HOME/.local/bin` or `/Users/<you>/.local/bin`)

## usage

    budget-exe <CSVFILE>

will print the total by category for the given bank CSV file

    budget-exe <CSVFILE> <CATEGORIES>

will print the total by category for the given bank CSV file and the selection of categories included in the categories file.

Typical categories file should contain one category per line, in double quotes:

    "My category"
    "My other category"

