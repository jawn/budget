# budget

## to do

- [X] total all categories at the bottom of the report "TOTAL         : 4807.00"
- [X] filter on one or several categories: `budget <CSVFILE> <CATEGORIES-FILE>` and total these categories
- [X] display the name of the csv file the report is made on, and the name of the categories selection file
- [X] include a header : REPORT from YYYY/MM/DD to YYYY/MM/DD and footer : TOTAL from YYYY/MM/DD to YYYY/MM/DD
- [X] display each categories average amount, which is total (per category) / number of months of the period
- [X] rename budget-exe to budget
- [X] change "budget <CSVFILE> [CATEGORIES-SELECTION-FILE]" into  "budget summary <CSVFILE> [CATEGORIES-SELECTION-FILE]"
- [X] add a help command
- [X] have the budget app find its main CSV file in ~/.budget/budget_conf containing TRANSACTIONS:file/path/to/transactions.CSV 
- [X] add a qualifier for specific parameters in the command  like -t for transaction file -c for category selection file -p for period, e.g. budget summary -c MyCategories.csv -t MySpecificTransaction.csv
- [ ] add a command "budget import <DOWNLOADED-CSVFILE-FOLDER>" that adds all the new transactions files from that folder to the transaction csv file, checking for format and duplicates

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

Typical categories file should contain one category per line, optionally in double quotes:

    My category
    "Small Expenses"
    Transfers

