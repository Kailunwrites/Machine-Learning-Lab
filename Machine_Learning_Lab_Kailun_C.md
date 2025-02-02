# Machine Learning Lab

This lab is aimed to walk you through the complete workflow of a machine learning project; from data wrangling, exploratory data analysis (EDA), model training and model evaluation/comparison. 

You will work with your machine project teamates for this lab and your team needs to decide whether to use either R or Python as the main programming language. **Each team memeber needs to work on his/her own submission.**

We will use Github for team collaboration and it can be broken down into following steps:

1. The team leader creates a public Github repository under his/her account first.

2. All the other team members fork the repo so you will have a COPY of the repo under your account

3. Git clone YOUR OWN repo otherwise you won't be able to push later.

4. Create a subfolder under your name and finish your code. Push the changes to Github. *Note*: you might want to put csv file and system file in the gitignore.

5. Go to the Github page of YOUR OWN repository and click the "Pull Request" tab. You can find the details [here](https://help.github.com/articles/creating-a-pull-request-from-a-fork/)

6. Submit the pull request so you can see it under team leader's repository.

7. Pair review each other's code before merging it to the master branch.


**Homework**

- To sync all the changes made by your teammates to your own repository, follow the instructions under the "Keeping Your Fork Up to Date" section [here](https://gist.github.com/Chaser324/ce0505fbed06b947d962).

- To understand fork, pull request and branch better, review [this video](https://youtu.be/_NrSWLQsDL4) in 1.25X speed.


## Part I: Preprocessing and EDA

- The data comes from a global e-retailer company, including orders from 2012 to 2015. Import the **Orders** dataset and do some basic EDA. 
- For problem 1 to 3, we mainly focus on data cleaning and data visualizations. You can use all the packages that you are familiar with to conduct some plots and also provide **brief interpretations** about your findings.

### Problem 1: Dataset Import & Cleaning
Check **"Profit"** and **"Sales"** in the dataset, convert these two columns to numeric type. 

library(dplyr)
orders=read.csv('Orders.csv')
orders$Profit <- as.numeric(gsub('[$,]', '', orders$Profit))
orders$Sales <- as.numeric(gsub('[$,]', '', orders$Sales))

### Problem 2: Inventory Management
- Retailers that depend on seasonal shoppers have a particularly challenging job when it comes to inventory management. Your manager is making plans for next year's inventory.
- He wants you to answer the following questions:
    1. Is there any seasonal trend of inventory in the company?
    2. Is the seasonal trend the same for different categories?

- ***Hint:*** For each order, it has an attribute called `Quantity` that indicates the number of product in the order. If an order contains more than one product, there will be multiple observations of the same order.

library(lubridate)
orders$Order.Date= mdy(orders$Order.Date)

# extract month from the date 
orders$month=month(orders$Order.Date)

plot_season= orders %>% group_by(month) %>% summarise(count_orders = sum(Quantity))

plot(plot_season) # from the plot, we observe a clear trend in the increase in quantity of sales as the year progresses from Jan to December. Note that bar plot here works also  

# need to group_by months and categories, within each month 
plot_season_2= orders %>% group_by(month, Category) %>% summarise(count_orders= sum(Quantity))

ggplot(data = plot_season_2) +
  geom_bar(aes(x=month, y = count_orders, fill = Category), stat = 'identity', position = 'dodge') +
  ggtitle('Number of Orders per Month') + 
  labs(x = 'month', y = 'number of orders') +
  scale_x_discrete(limits = 1:12)


### Problem 3: Why did customers make returns?
- Your manager required you to give a brief report (**Plots + Interpretations**) on returned orders.

	1. How much profit did we lose due to returns each year?
	
returns= read.csv('Returns.csv')
orders_returns= left_join(orders, returns, by="Order.ID")
orders_returns$Returned = ifelse(is.na(orders_returns$Returned), 0, 1)

orders_returns$year=year(orders_returns$Order.Date)
orders_returns_plot= orders_returns %>% group_by(year,Returned) %>%  summarise(sum(Profit))

ggplot(data = orders_returns_plot) +
  geom_bar(aes(x=year, y = sum(Profit), fill = Returned), stat = 'identity', position = 'dodge') +
  ggtitle('Profit of Returns vs Not Returned per Year') + 
  labs(x = 'year', y = 'Sum of Profit') +
  scale_x_discrete(limits = 1:12)

# 1. a lot less returned items than kept items 
# 2. Because of #1, Much more profit comes from kept items than returned items orders_returns
	
	2. How many customer returned more than once? more than 5 times?

cust_returns= orders_returns %>% filter(Returned==1) %>% group_by(Customer.ID) %>% summarise(Returned_times=n())

cust_returns_fivetimes= cust_returns %>% filter(Returned_times>5)

	3. Which regions are more likely to return orders?

return_percent_region=orders_returns %>% group_by(Region.x) %>% 
summarise(Percent_of_Orders_Returned=mean(Returned)*100)

	4. Which categories (sub-categories) of products are more likely to be returned?

return_percent_category=orders_returns %>% group_by(Sub.Category) %>% 
summarise(Percent_of_Orders_Returned=mean(Returned)*100)

- ***Hint:*** Merge the **Returns** dataframe with the **Orders** dataframe using `Order.ID`.


## Part II: Machine Learning and Business Use Case

Now your manager has a basic understanding of why customers returned orders. Next, he wants you to use machine learning to predict which orders are most likely to be returned. In this part, you will generate several features based on our previous findings and your manager's requirements.

### Problem 4: Feature Engineering
#### Step 1: Create the dependent variable
- First of all, we need to generate a categorical variable which indicates whether an order has been returned or not.
- ***Hint:*** the returned orders’ IDs are contained in the dataset “returns”
orders_returns$Returned = ifelse(is.na(orders_returns$Returned), 0, 1)

#### Step 2:
- Your manager believes that **how long it took the order to ship** would affect whether the customer would return it or not. 
- He wants you to generate a feature which can measure how long it takes the company to process each order.
- ***Hint:*** Process.Time = Ship.Date - Order.Date

orders$Ship.Date= mdy(orders$Ship.Date)
orders_returns$Process.Time= orders$Ship.Date- orders$Order.Date
# returns a column in unit of days 

#### Step 3:

- If a product has been returned before, it may be returned again. 
- Let us generate a feature indictes how many times the product has been returned before.
- If it never got returned, we just impute using 0.
- ***Hint:*** Group by different Product.ID

Product_returned=orders_returns %>% group_by(Product.ID) %>% select(Product.ID,Returned) %>% summarise(num_times_returned=sum(Returned)) 

orders_returns=left_join(orders_returns, Product_returned, by="Product.ID")

### Problem 5: Fitting Models

- You can use any binary classification method you have learned so far.
- Use 80/20 training and test splits to build your model. 
- Double check the column types before you fit the model.
- Only include useful features. i.e all the `ID`s should be excluded from your training set.
- Note that there are only less than 5% of the orders have been returned, so you should consider using the [createDataPartition](https://www.rdocumentation.org/packages/caret/versions/6.0-80/topics/createDataPartition) function from `caret` package and [StratifiedKfold](http://scikit-learn.org/stable/modules/generated/sklearn.model_selection.StratifiedKFold.html#sklearn-model-selection-stratifiedkfold) from sklearn when running cross-validation.
- Do forget to `set.seed()` before the spilt to make your result reproducible.
- **Note:** We are not looking for the best tuned model in the lab so don't spend too much time on grid search. Focus on model evaluation and the business use case of each model.

# Psudo code: 
partition data into equal folds, within each fold, appropriate equal proportions of response variable= Returned vs Non Returned  

#Then return  
classify_model= glm(y=Returned~ , data= )

### Problem 6: Evaluating Models
- What is the best metric to evaluate your model. Is accuracy good for this case?
- Now you have multiple models, which one would you pick? 
- Can you get any clue from the confusion matrix? What is the meaning of precision and recall in this case? Which one do you care the most? How will your model help the manager make decisions?
- **Note:** The last question is open-ended. Your answer could be completely different depending on your understanding of this business problem.

### Problem 7: Feature Engineering Revisit
- Is there anything wrong with the new feature we generated? How should we fix it?
- ***Hint***: For the real test set, we do not know it will get returned or not.
