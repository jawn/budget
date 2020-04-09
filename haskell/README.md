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

## how to make the main transactions file permanent

let's say the main transaction csv file, called `Transactions.csv` resides in a folder `Documents/MyData/Budget` on your personnal folder.

edit the file `~/.budget_conf` (on your home directory) to contain:

    transactions:.~/Documents/MyData/Budget/Transactions.csv

then the command `budget summary` will display a summary report of these transactions.

## usage

    budget-exe summary 

will print the total by category for all the transactions in the main transactions csv file.

    budget-exe summary -t <OtherTransactions.csv>

will print the total by category for all the transactions in the alternative csv file.


    budget-exe summary -c <Categories.csv>

will print the total by category for categories given in the selection file only.

## File Format

### Transactions

Comma separated values file, without header.

| column | information | used | format | comment |
| :--    | :--         | :--  | :--    | :--     |
| 0  |  Status |  | string | "posted" |
| 1 |  ??? |     | | |
| 2 |  Date | yes |     | MM/DD/YYYY |
| 3 |  Notes |   | | |
| 4 |  Name |    | | |
| 5 |  Category | yes       | string | |
| 6 |  Amount | yes        | --999999.99 | (yes, possibly double negative) |

### Category

Comma separated values file, without header.

Should contain one category per line, optionally in double quotes:

    Groceries
    "Small Expenses"
    Transfers



