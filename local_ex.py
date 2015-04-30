import yahoo_api
import yaml

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

foo = y.request('http://fantasysports.yahooapis.com/fantasy/v2/league/346.l.49099/standings')

print foo.content