import pandas as pd

# 1. Load from Google Drive

from google.colab import drive
drive.mount('/content/drive')

df = pd.read_csv('/content/drive/MyDrive/DATA/olympic_medals.csv')

# 2(a). Size of the data frame
rows, cols = df.shape
print("Number of rows:", rows)
print("Number of columns:", cols)

# 2(b). Data types of all columns
print("\nData types:")
print(df.dtypes)

# 2(c). Number of unique cities where matches were held
unique_cities = len(df['Location'].unique())
print("\nNumber of unique cities:", unique_cities)

# 2(d). Total number of medals won by USA
usa_medals = df[df['Nationality'] == 'USA'].shape[0]
print("\nTotal medals won by USA:", usa_medals)

# 2(e). Total number of medals for each nationality
medals_by_country = df.groupby('Nationality')['Medal'].count()
print("\nTotal medals by nationality:")
print(medals_by_country)