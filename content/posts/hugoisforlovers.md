---
date: "2019-12-06"
tags:
- python
- scripting
- control_flow
- functions
title: Python Basics 1
toc: true
---


Download available at [My Github Page](https://github.com/alswh0031/Data-Analysis-R-and-Python/blob/master/Python_basic1.ipynb)


First, load the dataset *faithful*. 
```{python}
import pandas as pd
faithful=pd.read_csv("https://vincentarelbundock.github.io/Rdatasets/csv/datasets/faithful.csv")

x=faithful
type(x)
```
Check the dataset by loading the first five rows. 
```{python}
faithful.head()
```
