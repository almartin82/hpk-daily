import yahoo_api
import yaml
import functions
from datetime import date

#read in credentials
with open("credentials.yml", 'r') as ymlfile:
    creds = yaml.load(ymlfile)

#read the consumer key and secret from yaml
key = creds['consumer_key']
secret = creds['consumer_secret']

y = yahoo_api.YahooAPI(
    consumer_key=creds['consumer_key'],
    consumer_secret=creds['consumer_secret'],
    access_token=creds['access_token'],
    access_token_secret=creds['access_token_secret'],
    session_handle=creds['session_handle']
)

r = functions.make_daily_stats_req('346.l.49099.t.1', date.today())
y.api_query(r)