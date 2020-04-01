class Expense
	attr_reader :status, :account, :date, :description,:category,:amount,:notes
	def initialize (status, account,date,description,category,amount,notes)
        @status = status
		@account = account
		@date=date
		@description=description
		@category=category
		@amount=amount
		@notes=notes

	end
end
