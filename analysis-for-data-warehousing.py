#!/usr/bin/env python
# coding: utf-8

# In[1]:


import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)

import os
for dirname, _, filenames in os.walk('/kaggle/input'):
    for filename in filenames:
        print(os.path.join(dirname, filename))


# In[2]:


df=pd.read_csv("/kaggle/input/nyc-parking-tickets/Parking_Violations_Issued_-_Fiscal_Year_2014__August_2013___June_2014_.csv")


# In[3]:


import pandas as pd
import numpy as np

df['Feet From Curb'] = df['Feet From Curb'].fillna(0)
df['Meter Number'] = df['Meter Number'].fillna(0)

df['Vehicle Make'] = df['Vehicle Make'].fillna('Unknown')
df['Violation Description'] = df['Violation Description'].fillna('Unknown')

df['Issue Date'] = pd.to_datetime(df['Issue Date'], errors='coerce')
df['year'] = df['Issue Date'].dt.year
df['month'] = df['Issue Date'].dt.month

df['Vehicle Year'] = pd.to_numeric(df['Vehicle Year'], errors='coerce')
df['Feet From Curb'] = pd.to_numeric(df['Feet From Curb'], errors='coerce')

df['Plate Type'] = df['Plate Type'].astype('category')
df['Violation Code'] = df['Violation Code'].astype('int64')
df['Vehicle Body Type'] = df['Vehicle Body Type'].astype('category')
df['Issuing Agency'] = df['Issuing Agency'].astype('category')

df.drop_duplicates(inplace=True)

df['Vehicle Make'] = df['Vehicle Make'].str.lower()
df['Violation Description'] = df['Violation Description'].str.lower()

boolean_columns = ['Unregistered Vehicle?', 'No Standing or Stopping Violation', 'Hydrant Violation', 'Double Parking Violation']
for col in boolean_columns:
    if col in df.columns:
        df[col] = df[col].map({'True': True, 'False': False})
        df[col] = df[col].fillna(False)
        
df['Vehicle Year'] = df['Vehicle Year'].replace(9999, np.nan)
df['Vehicle Year'] = df['Vehicle Year'].fillna(df['Vehicle Year'].median())

df.head()


# In[4]:


import matplotlib.pyplot as plt

violations_per_year = df.groupby('year')['Violation Code'].count().sort_values(ascending=False).head(2)

plt.figure(figsize=(10, 6))
violations_per_year.plot(kind='bar', color='green')
plt.title('Total Violations Per Year')
plt.xlabel('Year')
plt.ylabel('Total Violations')
plt.xticks(rotation=45)
plt.grid(axis='y', linestyle='--', alpha=0.7)
plt.show()


# In[5]:


top_5_vehicle_types = df['Vehicle Body Type'].value_counts().head(5)

top_5_vehicle_types.plot(kind='area', figsize=(10, 6), color='skyblue', alpha=0.7)

plt.title('Top 5 Most Produced Vehicle Body Types ', fontsize=16, fontweight='bold')
plt.xlabel('Vehicle Body Type', fontsize=14)
plt.ylabel('Count', fontsize=14)
plt.xticks(rotation=45)
plt.grid(axis='y', linestyle='--', alpha=0.7)

plt.show()


# In[7]:


import matplotlib.pyplot as plt

violation_counts = df['Violation Code'].value_counts()

top_violation_codes = violation_counts.head(10)

plt.figure(figsize=(8, 8))
plt.pie(
    top_violation_codes, 
    labels=top_violation_codes.index,  
    autopct='%.2f%%', 
    startangle=140,
    shadow=True, 
    wedgeprops={'edgecolor': 'black', 'linewidth': 1}
)

plt.title('Top 10 Violation Codes Distribution', fontsize=16, fontweight='bold')

plt.show()


# In[8]:


import matplotlib.pyplot as plt
import pandas as pd
from mpl_toolkits.mplot3d import Axes3D

violations_by_make = df.groupby('Vehicle Make')['Violation Code'].count().sort_values(ascending=False).head(5)

fig = plt.figure(figsize=(12, 6))
ax = fig.add_subplot(111, projection='3d')

x = violations_by_make.index
y = violations_by_make.values 

ax.bar(x, y, zs=0, zdir='y', color='skyblue', edgecolor='black')

ax.set_title('Total Violations by Vehicle Make', fontsize=16)
ax.set_zlabel('Counts', fontsize=12)

ax.set_xticklabels(x, rotation=90)

plt.tight_layout()

plt.show()


# In[9]:


import matplotlib.pyplot as plt

violations_per_states = df.groupby('Registration State')['Violation Code'].count().sort_values(ascending=False).head(10)


plt.figure(figsize=(12, 6))
violations_per_states.plot(kind='barh', color='lightcoral')

plt.title('Total Violations Per Registration State', fontsize=16)
plt.xlabel('Total Violations', fontsize=12)
plt.ylabel('Registration State', fontsize=12)

plt.grid(axis='x', linestyle='--', alpha=0.7)

plt.tight_layout()
plt.show()


# In[10]:


import matplotlib.pyplot as plt

violations_per_year = df.groupby('year')['Violation Code'].count().sort_values(ascending=False).head(2)

plt.figure(figsize=(8, 8))
plt.pie(
    violations_per_year, 
    labels=violations_per_year.index,  
    autopct='%1.1f%%', 
    startangle=140,  
    wedgeprops={'width': 0.2},
    colors=plt.cm.Paired.colors 
)

plt.title('Total Violations Per Year', fontsize=16)

plt.show()


# In[11]:


import matplotlib.pyplot as plt
import calendar

violations_per_month_code = df.groupby(['month', 'Violation Code']).size().unstack(fill_value=0)

month_names = [calendar.month_name[i] for i in violations_per_month_code.index]

plt.figure(figsize=(14, 8)) 
violations_per_month_code.plot(kind='bar', stacked=True, colormap='Set3', width=0.8, legend=False)

plt.xticks(ticks=range(len(month_names)), labels=month_names, rotation=45, fontsize=10)

plt.title('Total Violations Per Month by Violation Code', fontsize=16)
plt.xlabel('Month', fontsize=12)
plt.ylabel('Total Violations', fontsize=12)

plt.grid(axis='y', linestyle='--', alpha=0.7)

plt.show()

