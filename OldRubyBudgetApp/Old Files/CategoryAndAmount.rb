require 'csv'
Category_column = 3
Amount_column   = 4
Comment_column  = 5
Date_column     = 1
Account_column  = 0
Description_column = 2
Comments_column = 6

def printTotalCategory(category, amount)
	printf("%-28s %10s %14.2s\n","Total", category, amount)
end

amount_total=0

budget_arr=CSV.read("2016 Budget Transactions.csv")[1..-1]

budget_arr.sort! do |a,b| 
	a[Category_column] <=> b[Category_column]
end


current_category = budget_arr[0][Category_column]
amount_category = 0
budget_arr.each do |row|
  if !(row[Category_column]==(current_category))
  	printTotalCategory(current_category, amount_category)
 	amount_category = 0
  	current_category = row[Category_column]
  end
  amount_category += row[Amount_column].to_f   
 printf("  %-28s %10.2f %10s\n", row[Category_column], row[Amount_column], row[Comment_column])
  amount_total+=row[Amount_column].to_f  

end
printTotalCategory(current_category, amount_category)
printf("%41.2f\n", amount_total)
