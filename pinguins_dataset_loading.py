import pandas as pd

# Загрузим небольшой датасет из seaborn
import seaborn as sns

df = sns.load_dataset('penguins')

# Сохраним датасет в формате CSV
df.to_csv('penguins.csv', index=False)
