require "./Category.rb"
require "./Expense.rb"
require "./Report.rb"

Given(/^the report start date is "([^"]*)"$/) do |startDate|
	@startDate = Date.parse(startDate)
  
end

Given(/^the report end date is "([^"]*)"$/) do |endDate|
  	@endDate = Date.parse(endDate)
end

Given(/^the budget for Home Maintenance is "([^"]*)"$/) do |budget|
  @budget = budget.to_f	
end

When(/^the report is run$/) do
  @categories = {}
  @categories["Home Maintenance"] = Category.new("Home Maintenance", "Top", nil, @budget)
  @report = BudgetReport.new(@startDate, @endDate, @categories,[], false , nil)
end

Then(/^the budget for Home Maintenance should be "([^"]*)"$/) do |budgeted_amount|
	expect(@report.reportLines[0].budgeted).to eq budgeted_amount.to_f
  
end
