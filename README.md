# How to use it

## Objective:

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

## Steps:


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
