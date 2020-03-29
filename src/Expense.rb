class Expense
	attr_reader :account, :date, :description,:category,:amount,:notes
	def initialize (account,date,description,category,amount,notes)
		@account = account
		@date=date
		@description=description
		@category=category
		@amount=amount
		@notes=notes

	end
end