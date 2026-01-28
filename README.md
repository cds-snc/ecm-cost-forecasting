# ECM Partner Cost Forecasting

This repository contains scripts to clean and analyze data about historical
costs billed to External Credential Management partners. It uses those
historical costs to create forecasts of future costs, that can be compared
against MOU estimates to gauge whether a partner is at risk of exceeding their
budget.

# Data and Outputs
The cost data is not included in this repository. It is written to an on-prem
SQL Server database, accessible when connected to the ESDC database.



## Analysis Examples

*Note: These are _examples_ of the analysis in this repository.*

### Cost Forecasts
![A example of the historical and forecasted monthly costs for CRA](images/cra-forecast.png)

### Year-over-year Comparisons
![A example of the year-over-year monthly costs for CRA](images/cra-forecast-seasonal.png)



 
