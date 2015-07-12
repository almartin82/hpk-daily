import yahoo_api
import yaml
import functions
import resources
from datetime import date, timedelta
import pandas

#read in credentials
with open("credentials.yml", 'r') as ymlfile:
    creds = yaml.load(ymlfile)

#read the consumer key and secret from yaml
key = creds['consumer_key']
secret = creds['consumer_secret']

#initialize a yahoo session
y = yahoo_api.YahooAPI(
    consumer_key=creds['consumer_key'],
    consumer_secret=creds['consumer_secret'],
    access_token=creds['access_token'],
    access_token_secret=creds['access_token_secret'],
    session_handle=creds['session_handle']
)

stat_df = pandas.DataFrame()

for i in resources.all_hpk_teams:
    r = functions.make_owner_details_req(i)
    raw = y.api_query(r)
    df = functions.process_owner_details(raw)
    stat_df = stat_df.append(df)

stat_df.to_csv('data\\all_owners.csv', index=False, encoding='utf-8')
