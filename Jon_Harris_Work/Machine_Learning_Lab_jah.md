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

orders = read.csv('../Orders.csv')
View(orders)

### Problem 1: Dataset Import & Cleaning
Check **"Profit"** and **"Sales"** in the dataset, convert these two columns to numeric type. 

orders$Profit = as.numeric(gsub('[$,]', '', orders$Profit))
orders$Sales = as.numeric(gsub('[$,]', '', orders$Sales))

### Problem 2: Inventory Management
- Retailers that depend on seasonal shoppers have a particularly challenging job when it comes to inventory management. Your manager is making plans for next year's inventory.
- He wants you to answer the following questions:
    1. Is there any seasonal trend of inventory in the company?
    2. Is the seasonal trend the same for different categories?
    
- ***Hint:*** For each order, it has an attribute called `Quantity` that indicates the number of product in the order. If an order contains more than one product, there will be multiple observations of the same order.

#2.1 - IS THERE A SEASONAL TREND OF INVENTORY?
  library(dplyr)
  library(lubridate)
  library(ggplot2)
#Convert string to date and separate month
  orders$Order.Date = as.Date(orders$Order.Date, format = "%m/%d/%y") #convert str to date
  orders$order_month = month(orders$Order.Date)

#Plot bar graph of sum quantities 
orders %>% group_by(order_month) %>% select(order_month, Quantity) %>% summarize(quant = sum(Quantity)) %>%
ggplot(data=., aes(x = order_month, y = quant)) + geom_bar(stat='identity') + 
xlab('Months') + ylab('Number of Orders')

#2.2 - IS THERE A SEASONAL TREND OF OTHER CATEGORIES?
orders %>% group_by(order_month, Category) %>% select(order_month, Category, Quantity) %>% summarize(quant = sum(Quantity)) %>%
ggplot(data=., aes(x = order_month, y = quant, fill = Category)) + geom_bar(stat='identity', position='dodge') + xlab('Months') + ylab('Number of Orders')


### Problem 3: Why did customers make returns?
- Your manager required you to give a brief report (**Plots + Interpretations**) on returned orders.
returns = read.csv('../Returns.csv')
View(returns)

	1. How much profit did we lose due to returns each year?
combined_data = left_join(orders, returns, by = 'Order.ID')
combined_data$Returned = ifelse(is.na(combined_data$Returned), 0, 1)

combined_data %>% mutate(order_year = year(Order.Date)) %>% filter(Returned == 1) %>% group_by(order_year) %>% summarise(loss = sum(Profit)) %>% ggplot(data = ., aes(x=order_year, y = loss)) + geom_bar(stat = 'identity') + xlab('Years') + ylab('Lossed Profit')

	2. How many customer returned more than once? more than 5 times?
returned_summary = combined_data %>% filter(Returned == 1) %>% group_by(Customer.ID) %>% summarise(num_return = sum(Returned))

morethan1 = sum(returned_summary$num_return > 1) #547 individual customer
morethan5 = sum(returned_summary$num_return > 5) #46 individual customers

	3. Which regions are more likely to return orders?
combined_data %>% group_by(Region.x) %>% summarise(perc_return = mean(Returned)*100) %>% ggplot(data=., aes(x=reorder(Region.x, perc_return), y = perc_return, fill=Region.x)) + geom_bar(stat='identity')  + xlab('Region') + ylab('Percent Returns')

	4. Which categories (sub-categories) of products are more likely to be returned?
combined_data %>% group_by(Sub.Category) %>% summarise(perc_return = mean(Returned)*100) %>% ggplot(data=., aes(x=reorder(Sub.Category, perc_return), y = perc_return, fill=Sub.Category)) + geom_bar(stat='identity')  + xlab('Product Sub-Category') + ylab('Percent Returns')


## Part II: Machine Learning and Business Use Case

Now your manager has a basic understanding of why customers returned orders. Next, he wants you to use machine learning to predict which orders are most likely to be returned. In this part, you will generate several features based on our previous findings and your manager's requirements.

### Problem 4: Feature Engineering
#### Step 1: Create the dependent variable
- First of all, we need to generate a categorical variable which indicates whether an order has been returned or not.
- ***Hint:*** the returned orders’ IDs are contained in the dataset “returns”


#### Step 2:
- Your manager believes that **how long it took the order to ship** would affect whether the customer would return it or not. 
- He wants you to generate a feature which can measure how long it takes the company to process each order.
- ***Hint:*** Process.Time = Ship.Date - Order.Date
combined_data %>% mutate(Ship.Date = as.Date(orders$Ship.Date, format = "%m/%d/%y")) %>% mutate(Process.Time = difftime(Ship.Date , Order.Date , units = c("days")))


#### Step 3:

- If a product has been returned before, it may be returned again. 
- Let us generate a feature indictes how many times the product has been returned before.
- If it never got returned, we just impute using 0.
- ***Hint:*** Group by different Product.ID
combined_data %>% group_by(Product.ID) %>% select(Product.ID, %>% summary(sumz = sum(returned))

### Problem 5: Fitting Models

- You can use any binary classification method you have learned so far.
- Use 80/20 training and test splits to build your model. 
- Double check the column types before you fit the model.
- Only include useful features. i.e all the `ID`s should be excluded from your training set.
- Note that there are only less than 5% of the orders have been returned, so you should consider using the [createDataPartition](https://www.rdocumentation.org/packages/caret/versions/6.0-80/topics/createDataPartition) function from `caret` package and [StratifiedKfold](http://scikit-learn.org/stable/modules/generated/sklearn.model_selection.StratifiedKFold.html#sklearn-model-selection-stratifiedkfold) from sklearn when running cross-validation.
- Do forget to `set.seed()` before the spilt to make your result reproducible.
- **Note:** We are not looking for the best tuned model in the lab so don't spend too much time on grid search. Focus on model evaluation and the business use case of each model.


### Problem 6: Evaluating Models
- What is the best metric to evaluate your model. Is accuracy good for this case?
- Now you have multiple models, which one would you pick? 
- Can you get any clue from the confusion matrix? What is the meaning of precision and recall in this case? Which one do you care the most? How will your model help the manager make decisions?
- **Note:** The last question is open-ended. Your answer could be completely different depending on your understanding of this business problem.

### Problem 7: Feature Engineering Revisit
- Is there anything wrong with the new feature we generated? How should we fix it?
- ***Hint***: For the real test set, we do not know it will get returned or not.
