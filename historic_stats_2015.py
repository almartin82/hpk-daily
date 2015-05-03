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

d = resources.yr_2014
dd = [d[0] + timedelta(days=x) for x in range((d[1]-d[0]).days + 1)]

stat_df = pandas.DataFrame()

for day in dd:
    print day
    for team in resources.hpk_teams_2014:
        r = functions.make_daily_stats_req(team, day)
        raw = y.api_query(r)
        df = functions.process_team_stats(raw)
        stat_df = stat_df.append(df)

stat_df.to_csv('team_by_date_2014.csv', index=False)

