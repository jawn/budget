

Feature: PrintDetailedExpenses
	
  Scenario: Print one category at a time

  	Given the following transactions exist:
  	|account|date|notes|description|category|amount|
	  |PersonalChecking|12/20/16|Connections At Work infusion|CHECK # 0058|Transfers|-500|
    |ChasePersonalCredit|5/18/16||SAFEWAY STORExxxx4316|Groceries|-57.43|
	  |BusinessSparkVisa|5/19/16||Mobile Deposit|Transfers|500|
	  |PersonalChecking|5/19/16||CHECK # 0057|Lawn|-600|
    
    And I select the category "Transfers"
    
    When I run the report
    
    Then I should see the following data:
    |account|date|notes|description|category|amount|
    |BusinessSparkVisa|5/19/16||Mobile Deposit|Transfers|500|
    |PersonalChecking|12/20/16|Connections At Work infusion|CHECK # 0058|Transfers|-500|
    And the display should look like:
    """
Transfers

Account              Date       Notes                                      Description                       Amount
=======              ====       =====                                      ===========                       ======
BusinessSparkVisa    05/19/2016                                            Mobile Deposit                    500.00
PersonalChecking     12/20/2016 Connections At Work infusion               CHECK # 0058                     -500.00

"""

