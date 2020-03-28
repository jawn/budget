require "./ExpensesPerCategory.rb"

# Class definition
class Student
end

# Student class tests
RSpec.describe Student do
  it 'creates a student class' do
    student = Student.new
    expect(student).to be_kind_of(Student)
  end
end

describe "importing a CSV category file" do
	it "should read all the category items" do 
		categories = ImportCategoryCSV("TestData/SampleCategories.csv")
		expect(categories.size).to eq 18
	#	Automotive Expenses,Top,,200
		automotive = categories["Automotive Expenses"]
		expect(automotive.name).to eq "Automotive Expenses"
		expect(automotive.type).to eq "Top"
		expect(automotive.parent).to eq nil
		expect(automotive.budgeted_amount).to eq 340
		repairCamry = categories["Repair-Camry"]
		expect(repairCamry.parent).to be automotive
	end
end


describe "importing a CSV expense file" do
	it "should read all the expenses" do 
		categories = ImportCategoryCSV("TestData/Categories.csv")
		expenses = ImportExpenseCSV("TestData/2016 Budget Transactions.csv", categories)
		expect(expenses[0].amount).to eq 10
		expect(expenses[0].date).to eq Date.new(2016,03,03)
		expect(expenses[0].category).to be categories["Christie - Assistance"]
		christieAssistance = categories["Christie - Assistance"]
		expect(christieAssistance.actual_amount).to eq -45.03
	end
	
end

describe "filtering an expense list" do
	it "should limit by date range of three months" do
		categories = ImportCategoryCSV("TestData/Categories.csv")
		expenses = ImportExpenseCSV("TestData/FilteredSampleExpenses.csv", categories)
		expect(expenses.size).to eq 10
		myfirstquarter = selectExpenses(expenses, Date.new(2016,01,01), Date.new(2016,03,31))
		expect(myfirstquarter.size).to eq 1
		expect(myfirstquarter[0].date).to be_between(Date.new(2016,01,01), Date.new(2016,03,31))
		myfirstquarter = selectExpenses(expenses, Date.new(2017,01,01), Date.new(2017,12,31))
		expect(myfirstquarter.size).to eq 3
	end
end

describe "updating category spent amount" do
	it "should recalculate the spent amount for categories according to a list of expenses" do
		categories = ImportCategoryCSV("TestData/Categories.csv")
		expenses = ImportExpenseCSV("TestData/FilteredSampleExpenses.csv", categories)
		expect(categories["Personal Care"].actual_amount).to eq 25
		myfirstquarter = selectExpenses(expenses, Date.new(2016,01,01), Date.new(2016,03,31))

		updateCategoriesActualAmount(categories, myfirstquarter)
		expect(categories["Personal Care"].actual_amount).to eq 10
	end
end
	

