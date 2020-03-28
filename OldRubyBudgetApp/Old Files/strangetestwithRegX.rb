	# it "should report half the amount of expenses for some business categories with .5 as allowable expense factor" do 
	# 	categories = ImportCategoryCSV("TestData/SampleCategories.csv")
	# 	expect(categories["Business Miscellaneous"].pct_deductible).to eq 0.5
	# 	expenses   = ImportExpenseCSV("TestData/SampleExpenses.csv", categories)
	# 	budgetReport = BudgetReport.new(categories, expenses, topLevelOnly, noSelection)
	# 	printResult = StringIO.new
	# 	budgetReport.taxExpensesOnly = true
	# 	budgetReport.print(false, printResult)
	# 	position = /Business Miscellaneous.*7.5/ =~ printResult.string 
	# 	if position.nil? # didn't find the correct amount?
	# 		puts printResult.string
	# 	end
	# 	expect(position).not_to be_nil
	# 	# if we set TaxExepnsesOnly to False, those expenses are as expended - actuals...
	#     printResult = StringIO.new
	# 	budgetReport.taxExpensesOnly = false
	# 	budgetReport.print(false, printResult)
	# 	position = /Business Miscellaneous.*15.0/ =~ printResult.string 
	# 	if position.nil? # didn't find the correct amount?
	# 		puts printResult.string
	# 	end
	# 	expect(position).not_to be_nil
	# end

	# it "should know how to report on business only" do 
	# 	categories = ImportCategoryCSV("TestData/SampleCategories.csv")
	# 	expenses   = ImportExpenseCSV("TestData/SampleExpenses.csv", categories)
	# 	# businessCategoriesIds = ImportCategoryIDListCSV("./TestData/BusinessCategoryIDs.csv")
	# 	budgetReport = BudgetReport.new(categories, expenses, topLevelOnly, noSelection)
	# 	#budgetReport.onlyCategories = businessCategoriesIds;
	# 	budgetReport.filter= ["Business Miscellaneous"]
	# 	printResult = StringIO.new
	# 	budgetReport.print(false, printResult)
	# 	position = /TOTAL.*15.00/ =~ printResult.string 
	# 	if position.nil? # didn't find the correct amount?
	# 		puts printResult.string
	# 	end
	# 	expect(position).not_to be_nil

	# end