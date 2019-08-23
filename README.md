# Guide
> Link of the website : https://letourmyvincent-r.shinyapps.io/ShinyApp/ 

This present guide will help you:
- First, to know the aim of the program 
- Second, how to use it
- and Third, to undersand the structure of the code.

# I. Objective

The goal of the code is to allow the user to create **different configurations** of a unique database according to missing or inconsistent values, with the aim of **testing** them on a prediction algorithm.

**The configurations** are created according to the presence or not of missing and/or inconsistent values. To remove them of the database, we will separate two case:

- Remove only columns one by one
- Remove columns one by one **and** remove rows with missing or inconsistent values.

The difference is the number of data we will keep: in the first one, we keep all the rows but we have some inconsistent values, and in the second one, we don’t have any problem but we have less data to use for the prediction algorithm.

**The results** we want are:

- Cost (according to the values entered, see step 5)
- Accuracy
- Sensitivity
- Specificity
- Area Under the Curve (AUC)

We will have these results in line charts, with two lines according to the two cases of removing missing values (columns only or columns and rows).

# II. How to use it


### Step 1: 
Select the good parameters and upload your database. You can define what is a missing value in your data from 3 choices: “?”, “NA” or “ “. Optional: If you have the same database but with no missing or inconsistent values to compare themselves, you can upload it.

### Step 2: 
Select your data quality configuration. You have two choices: upload the types and ranges files (information about them are given on the program) to detect missing and inconsistent values, or just detect missing values. If you choose the first option, you upload them.

### Step 3: 
Now missing (and inconsistent) values are detected, you can see selected all the columns where there are present. If you do nothing, all the selection will be removed, one by one, to create as many configurations as there are columns, two times, one with removing rows, one without.

### Step 4: 
Select the target of the prediction algorithm, and remove the column you want if you need it. For example, if there are 4 targets in your initial database, you have to select one and remove 3  to avoid corrupting the results.

### Step 5: 
Select the cost of failing of the prediction. The cost of an error could be not the same than the cost of another. For example, if the real value is true but the prediction says false, the cost could be very high, but the opposite could be not true. So, you can enter the cost you want for each case.

### Step 6: 
Results. You have five line charts and two tabs, corresponding to the results of cost, accuracy, sensitivity, specificity and AUC for each configuration of the database.



# III. Structure of  the code

The code is speared in ten files:

- One file of the **main code** of the app (app.R)
- Nine files of **functions** used in the main code (funct_ .... .R)

### app.R:

Here are called all the others files of **functions** and all the **libraries** that we need, are created the **ui** and the **server**, and **run** the application due to the final line code "shinyApp(ui,server)”.

- **ui**: build the user inferface with a header, a sidebar and a body.
- **server**: build all the outputs necessary for the ui.

### Functions:

The functions are in different files, and the names of these are ranked by number, to facilitate legibility and maintenance of the code.

- **funct_0downloadFile.R**: includes functions to save data in a tab and download it

- **funct_1UI.R**: includes the functions header, sidebar and body for the user interface. Body is the most important, includes all the outputs created in the server in app.R to show them in the right way

- **funct_2reactivevalues.R**: includes the function reactiveValues, which lists all the reactive values necessary in the server

- **funct_3initStep.R**: includes functions needed for the Step 1

- **funct_4dataquality.R**: includes functions needed for Step 2

- **funct_5CVNaiveBayes**: includes functions of the predictive model, naïve bayes, with cross validation and a function to generate statistics results (accuracy, sensitivity, specificity)

- **funct_6loopResults.R**: includes functions to generate and to show the results

- **funct_7fixing.R**: includes the functions to calculate the cost for a fixed database

- (**funct_other.R**: not called in app.R, includes all the functions we don’t need but could be useful for the maintenance)

#
*Vincent LETOURMY, 20/08/2019*
