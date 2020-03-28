Feature: CalculateBudgetPerPeriod

  Scenario: Three Months Budget
    Given the report start date is "2016/01/01"
    And   the report end date is "2016/03/31"
    And   the budget for Home Maintenance is "500"
    When the report is run
    Then the budget for Home Maintenance should be "1500"

