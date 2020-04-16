# budget

## to do

- [X] total all categories at the bottom of the report "TOTAL         : 4807.00"
- [X] filter on one or several categories: `budget <CSVFILE> <CATEGORIES-FILE>` and total these categories
- [X] display the name of the csv file the report is made on, and the name of the categories selection file
- [X] include a header : REPORT from YYYY/MM/DD to YYYY/MM/DD and footer : TOTAL from YYYY/MM/DD to YYYY/MM/DD
- [X] display each categories average amount, which is total (per category) / number of months of the period
- [X] rename budget-exe to budget
- [X] change `budget <CSVFILE> [CATEGORIES-SELECTION-FILE]` into  budget summary `<CSVFILE> [CATEGORIES-SELECTION-FILE]`
- [X] add a help command
- [X] have the budget app find its main CSV file in `~/.budget/budget_conf` containing `TRANSACTIONS:file/path/to/transactions.CSV` 
- [X] add a qualifier for specific parameters in the command  like `-t` for transaction file `-c` for category selection file `-p` for period, e.g. `budget summary -c MyCategories.csv -t MySpecificTransaction.csv`
- [X] add a command `budget import <DOWNLOADED-CSVFILE-FOLDER>` that adds all the new transactions files from that folder to the transaction csv file, checking for format and duplicates
- [X] add a command `budget details [-p MM/DD/YYYY MM/DD/YYYY] [-m YYYY MM] [-c CATEGORY]` that shows all the transactions for a given months or a given category (or for all months or for all categories)
- [X] strip the spaces from the labels name and notes from import files
- [X] sorting options!
- [X] update Help
- [X] show the file path of the transaction file that was just displayed by detail 
- [X] import all the csv files in a directory, to account names defined by the alphabetical part of the file name
- [X] add sorting criteria for summary
- [ ] add period selection for summary
- [X] bug if an imported file's contains a field in quotes followed by a space bad things happen
- [X] bug if a file contains a date with space and year on 2 digits, bad things happen
- [X] bug if a file contains more than 7 fields bad things happen
- [X] buf if an import file contains already an Account in first field (hence not equal to "posted") import this file nonetheless, but keep the existing account name 

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

edit the file `~/.budget_conf` (on your home directory) to contain (here `you` is your user name; to know your user name, type `whoami`):

    transactions:/Users/you/Documents/MyData/Budget/Transactions.csv

then the command `budget summary` will display a summary report of these transactions.

## usage

### Summary

    budget summary 

or 
    budget sum

or 
    budget s

will print the total by category for all the transactions in the main transactions csv file.

    budget summary -t OtherTransactions.csv

will print the total by category for all the transactions in the alternative file `OtherTransactions.csv`.


    budget summary -c Categories.csv

will print the total by category for categories given in the selection file `Categories.csv` only.

### Detail

    budget detail

or
    budget d

will print the transactions in the main transactions file.

    budget d -t OtherTransactions.csv

will print the transactions in the alternative file `OtherTransactions.csv`.

    budget d -c "Business Expenses"

will print the transactions having the category *Business Expenses*.

    budget d -p 01/01/2020 03/31/2020

will print the transactions having their date between the January 1st 2020 and the March 31st 2020.

    budget d -m 2020 4

will print the transactions having their date in April 2020. 

    budget d -s AD

will print the transactions sorted by Account, then Date

    budget d -s Cm

will print the transactions sorted by Category (ascending) then Amount (descending) 

Sort criteria (they can all be mixed) :

- A : Account (asc)
- a : Account (desc)
- C : Category (asc)
- c : Category (desc)
- D : Date (asc)
- d : Date (desc)
- M : Amount (asc)
- m : Amount (desc)
- N : Name (asc)
- n : Name (desc)
- O : Note (asc)
- o : Note (desc)

### Import

    budget import MyTransactions.csv Checking

will import the transactions from the file `MyTransactions.csv` into the main transactions file, assigning *Checking* as the Account for these transactions. If the transactions have already been imported (based on the same Name, Date, and Amount) the import will be cancelled. Any transaction with a status different from "posted" will not be imported.


## File Format

### Transactions

Comma separated values file, without header.

| column | information | used | format | comment |
| :--    | :--         | :--  | :--    | :--     |
| 0  |  Status/Account |  | string | should always be "posted" in bank files, replaced with account when importing transactions, do not import transactions that are "pending" |
| 1 |  ??? |     | | |
| 2 |  Date | yes |     | MM/DD/YYYY |
| 3 |  Notes |   |S42 | |
| 4 |  Name |    | | |
| 5 |  Category | yes       | string | |
| 6 |  Amount | yes        | --999999.99 | (yes, possibly double negative) |

### Category

Comma separated values file, without header.

Should contain one category per line, no quotes:

    Groceries
    Small Expenses
    Transfers



