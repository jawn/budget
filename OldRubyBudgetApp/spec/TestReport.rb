require "./ExpensesPerCategory.rb"
require "./Category.rb"
require "./Expense.rb"
require "./Report.rb"

noSelection = nil
topLevelOnly = true
allLevels= false
describe "a budget report" do
	before(:each) do
    	@my_categories  = {}
		@automotiveExpense = Category.new("Automotive Expenses", "Top", nil, -1000, 1)
		repairCamry       = Category.new("Repair-Camry","Sub",@automotiveExpense, -300, 1)
		repairHonda       = Category.new("Repair-Honda","Sub",@automotiveExpense, -700, 1)
		@my_categories["Automotive Expenses"] = @automotiveExpense
		@my_categories["Repair-Camry"]       = repairCamry
		@my_categories["Repair-Honda"]       = repairHonda
		
		@my_expenses  = []
		expense = Expense.new("PersonalChecking",Date.new(2016,01,03),"We dont care", repairCamry,-150,nil)
		@my_expenses <<  expense
		repairCamry.spend(expense.amount)

		expense = Expense.new("PersonalChecking",Date.new(2016,01,03),"We dont care", repairHonda,-250,nil)
		@my_expenses <<  expense
		repairHonda.spend(expense.amount)


  	end

	it "should know its min and max dates and period in months" do 
		categories = ImportCategoryCSV("TestData/SampleCategories.csv")
		expenses   = ImportExpenseCSV("TestData/SampleExpenses.csv", categories)
		minDate    = Date.new(2016,01,03)
		maxDate    = Date.new(2016,03,24)
		budgetReport = BudgetReport.new(minDate, maxDate, categories, expenses, topLevelOnly, noSelection)
		expect(budgetReport.expensesPeriod).to eq 3
	end


	it "should have lines with amounts corresponding to expenses by category for top categories" do
		minDate    = Date.new(2016,01,03)
		maxDate    = Date.new(2016,01,30)
		budgetReport = BudgetReport.new(minDate, maxDate, @my_categories, @my_expenses, topLevelOnly, noSelection)
		expect(budgetReport.reportLines.size).to eq 1
		expect(budgetReport.reportLines[0].categoryName).to eq "Automotive Expenses"
		expect(budgetReport.reportLines[0].budgeted).to eq -2000 
		expect(budgetReport.reportLines[0].spent).to eq -400
		expect(budgetReport.reportLines[0].remainder).to eq -1600
	end

	it "should have lines with amounts corresponding to expenses by category for top and sub categories" do
		minDate    = Date.new(2016,01,03)
		maxDate    = Date.new(2016,01,30)		
		budgetReport = BudgetReport.new(minDate, maxDate, @my_categories, @my_expenses,allLevels,noSelection)
		expect(budgetReport.reportLines.size).to eq 3
		expect(budgetReport.reportLines[0].categoryName).to eq "Automotive Expenses"
		expect(budgetReport.reportLines[0].budgeted).to eq -2000 
		expect(budgetReport.reportLines[0].remainder).to eq -1600
		expect(budgetReport.reportLines[0].spent).to eq -400
		expect(budgetReport.reportLines[1].categoryName).to eq "Repair-Camry"
		expect(budgetReport.reportLines[1].budgeted).to eq -300 
		expect(budgetReport.reportLines[1].remainder).to eq -150
		expect(budgetReport.reportLines[1].spent).to eq -150
		expect(budgetReport.reportLines[2].categoryName).to eq "Repair-Honda"
		expect(budgetReport.reportLines[2].budgeted).to eq -700 
		expect(budgetReport.reportLines[2].remainder).to eq -450
		expect(budgetReport.reportLines[2].spent).to eq -250
		expect(budgetReport.total_budgeted).to eq -2000
		expect(budgetReport.total_expenses).to eq -400
		expect(budgetReport.total_remainder).to eq -1600
		expect(budgetReport.total_deductible).to eq -400
	end	
	it "should have lines with amounts corresponding to expenses by category for top and sub categories including expenses on top categories as well" do

		expense = Expense.new("PersonalChecking",Date.new(2016,01,03),"We dont care", @automotiveExpense,-350,nil)
		@my_expenses <<  expense
		@automotiveExpense.spend(expense.amount)

		minDate    = Date.new(2016,01,03)
		maxDate    = Date.new(2016,01,30)		
		budgetReport = BudgetReport.new(minDate, maxDate, @my_categories, @my_expenses,allLevels,noSelection)
		expect(budgetReport.reportLines.size).to eq 3
		expect(budgetReport.reportLines[0].categoryName).to eq "Automotive Expenses"
		expect(budgetReport.reportLines[0].budgeted).to eq -2000 
		expect(budgetReport.reportLines[0].remainder).to eq -1250
		expect(budgetReport.reportLines[0].spent).to eq -750
		expect(budgetReport.reportLines[1].categoryName).to eq "Repair-Camry"
		expect(budgetReport.reportLines[1].budgeted).to eq -300 
		expect(budgetReport.reportLines[1].remainder).to eq -150
		expect(budgetReport.reportLines[1].spent).to eq -150
		expect(budgetReport.reportLines[2].categoryName).to eq "Repair-Honda"
		expect(budgetReport.reportLines[2].budgeted).to eq -700 
		expect(budgetReport.reportLines[2].remainder).to eq -450
		expect(budgetReport.reportLines[2].spent).to eq -250
		expect(budgetReport.total_budgeted).to eq -2000
		expect(budgetReport.total_expenses).to eq -750
		expect(budgetReport.total_remainder).to eq -1250
		expect(budgetReport.total_deductible).to eq -750
	end	

	it "should filter for categories given by the user" do
		minDate    = Date.new(2016,01,03)
		maxDate    = Date.new(2016,01,30)

		houseMaintenance = Category.new("House Maintenance", "Top", nil, 200, 1)
		lawn       = Category.new("Lawn","Sub",houseMaintenance, 400, 1)
		pool       = Category.new("Pool","Sub",houseMaintenance, 400, 1)
		@my_categories["House Maintenance"] = houseMaintenance
		@my_categories["Lawn"]       = lawn
		@my_categories["Pool"]       = pool
		selectThese =["Automotive Expenses"]
		budgetReport = BudgetReport.new(minDate, maxDate, @my_categories, @my_expenses, topLevelOnly, selectThese)
		expect(budgetReport.reportLines.size).to eq 1
		expect(budgetReport.reportLines[0].categoryName).to eq "Automotive Expenses"
	end
	it "should have a deductible column reflecting the deductible factor of those category" do
		minDate    = Date.new(2016,01,03)
		maxDate    = Date.new(2016,01,30)

		my_categories = {} 
		my_categories["Mileage"] = Category.new("Mileage","Top",nil,500,0.75)
		mileage = my_categories["Mileage"]
		my_expenses = []
		my_expenses << Expense.new("bank", Date.new(2016,8,5),"Trip to Meetup",mileage,100,nil)
		mileage.spend(100)
		budgetReport = BudgetReport.new(minDate, maxDate, my_categories, my_expenses,allLevels, noSelection)
		expect(budgetReport.expensesPeriod).to eq 1
		expect(budgetReport.reportLines[0].budgeted).to eq 500
		expect(budgetReport.reportLines[0].spent).to eq 100
		expect(budgetReport.reportLines[0].deductible).to eq 75
	end
	it "should have an average monthly expended to date per category for given time period" do
		minDate    = Date.new(2016,01,03)
		maxDate    = Date.new(2016,04,30)

		my_categories = {} 
		my_categories["Mileage"] = Category.new("Mileage","Top",nil,500,0.75)
		mileage = my_categories["Mileage"]
		my_expenses = []
		my_expenses << Expense.new("bank", Date.new(2016,8,5),"Trip to Meetup",mileage,100,nil)
		mileage.spend(100)
		budgetReport = BudgetReport.new(minDate, maxDate, my_categories, my_expenses,allLevels, noSelection)
		expect(budgetReport.expensesPeriod).to eq 4
		expect(budgetReport.reportLines[0].budgeted).to eq 2000
		expect(budgetReport.reportLines[0].avgmonthlyspent).to eq 25
	end
end