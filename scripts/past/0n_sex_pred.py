import pandas as pd

file_path = '/Users/varun/Library/CloudStorage/Dropbox/USC/quota_raj/data/sarpanch_election_data/sp_2005_2010_2015_2020_fin.csv'
df = pd.read_csv(file_path)
df.head()
len(df.name_2015)



names_list = df["name_2015"].tolist()
print(names_list[:5]) 
len(names_list) == len(df.name_2015) # if TRUE next, else re-check

from naampy import predict_fn_gender
sex_15 = predict_fn_gender(names_list) #"keras" related error

final_sex_15 = pd.DataFrame({"key_2015": df["key_2015"], "sex_15": sex_15})
final_sex_15.to_csv('/Users/varun/Library/CloudStorage/Dropbox/USC/quota_raj/data/sarpanch_election_data/sex_15.csv', index=False)

