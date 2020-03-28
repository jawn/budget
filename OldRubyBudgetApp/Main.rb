load 'Report.rb'
load 'ExpensesPerCategory.rb'



--year = ARGV[0].to_i
year=2020
categories = ImportCategoryCSV("./Data/Categories.csv")

expenses = ImportExpenseCSV("./Data/Budget Transactions.csv", categories)
minDate    = Date.new(year,01,01)
maxDate    = Date.new(year,12,31)
minDate    = Date.new(2020,01,01)
maxDate    = Date.new(2020,02,29)




myquarter = selectExpenses(expenses, minDate, 	maxDate)
puts "#{myquarter.size}"

updateCategoriesActualAmount(categories, myquarter)

myFavoriteIncomeFilter = [ "Business Income",
						"Paychecks/Salary",
						"Other Income"]

myTransfersFilter = [	"ATM/Cash Withdrawals", 
						"Checks",
						"Credit Card Payments",
						"Deposits",
						"Pets/Pet Care",
						"Transfers",
						"HSA Funding and Reimb",
						"Expense Reimbursement",
						]

myBusinessExpensesFilter = [
						"Other Income",
						"Business Income",
						"Business Miscellaneous",
						"Books related to work",
						"Conference",
						"Dining",
						"Parking",
						"Travel - Lodging",
						"Travel - Transport",
						"Uber",
						"Liability Insurance",
						"Meals and Entertainment",
						"Marketing Advertising",
						"Training",
						"Other Transportation",
						"Local Transportation for Business",
						"Cable/Satellite Services",
						"Telephone Services",
						"Office Supplies",
						"Online Services",
						"Dues and Subscriptions",
						"Office Maintenance",
						"Office Supplies",
						"Postage and Shipping",
						"Printing",
						"Service Charges/Fees"]

				

mynonTransfersnonBusinessFilter = ["Automotive Expenses",
						"Other Expenses",
						"Charitable Giving",
						"Dues and Subscriptions",
						"Clothing/Shoes",
						"Education",
						"Electronics",
						"Entertainment",
						"Gasoline/Fuel",
						"General Merchandise",
						"Gifts",
						"Groceries",
						"Insurance",
						"Personal Care",
						"Restaurants/Dining",
						"Service Charges/Fees",
						"Travel",
						"Utilities",
						"Home Maintenance",
						"Repair-Honda",
						"Tax - Honda",
						"Furniture",
						"Refunds/Adjustments",
						"Hobbies",
						"Groceries",
						"Health Insurance Premium",
						"Retirement Contributions",
						"Home Improvement",
						"Interest",
						"Investment Income",
						"Paychecks/Salary",
						"Rent",
						"Other Income",
						"Healthcare/Medical",
						"HSA Funding and Reimb",
						"Other Bills",
						"Taxes",
						"Expense Reimbursement"
				]				
mynonTransfersFilter = ["Business Income",
						"Other Income",
						"Other Business Expenses",
						"Business Miscellaneous",
						"Books related to work",
						"Conference",
						"Dining",
						"Parking",
						"Travel - Lodging",
						"Travel - Transport",
						"Uber",
						"Liability Insurance",
						"Meals and Entertainment",
						"Marketing Advertising",
						"Training",
						"Other Transportation",
						"Local Transportation for Business",
						"Cable/Satellite Services",
						"Telephone Services",
						"Office Supplies",
						"Online Services",
						"Dues and Subscriptions",
						"Office Maintenance",
						"Office Supplies",
						"Postage and Shipping",
						"Printing",
						"Service Charges/Fees",
						"Automotive Expenses",
						"Other Expenses",
						"Charitable Giving",
						"Dues and Subscriptions",
						"Clothing/Shoes",
						"Education",
						"Electronics",
						"Entertainment",
						"Gasoline/Fuel",
						"General Merchandise",
						"Gifts",
						"Groceries",
						"Healthcare/Medical",
						"HSA Funding and Reimb",
						"Insurance",
						"Personal Care",
						"Restaurants/Dining",
						"Service Charges/Fees",
						"Travel",
						"Utilities",
						"Other Income",
						"Paychecks/Salary",
						"Home Maintenance",
						"Repair-Honda",
						"Tax - Honda",
						"Furniture",
						"Refunds/Adjustments",
						"Hobbies",
						"Groceries",
						"Health Insurance Premium",
						"Retirement Contributions",
						"Home Improvement",
						"Interest",
						"Investment Income",
						"Rent",
						"Other Bills",
						"Taxes",
						"Expense Reimbursement",
						"Christie - Assistance"
				]			
removed_lines=["Tax - Sienna",
"Tax - Camry",
"Repair-Sienna",
"Repair-Camry",
"Cleaner",
"Lawn",
"Pool",
"Divorce",
"Eric Counselling",
"Eric - Education",
"Eric - Misc eg help w clothes",
"Eric - Sperm Storage",
"Eric - Travel",
"Home - HOA",
"Christie - Assistance",
"Advertising",
"Child/Dependent Expenses"]


budgetReport =  BudgetReport.new(minDate, maxDate, categories,myquarter, nil , nil  ) # nil, or myFavoriteFilter
topLevel = true	
budgetReport.taxExpensesOnly = false


budgetReport.print(topLevel)
# what we will have after story is done:
# businessCategoriesIds = ImportCategoryIDListCSV("./Data/2016 Business Categories IDs.csv")
# budgetReport.print(topLevel, businessCategoriesIds)
