import pandas

def make_daily_stats_req(team, date):
    base = "http://fantasysports.yahooapis.com/fantasy/v2/team/"
    sub_resource = "/stats;type=date;date="
    final = base + team + sub_resource + str(date)
    return final

def process_team_stats(raw):
    stats = raw['fantasy_content']['team']['team_stats']['stats']['stat']
    df = pandas.DataFrame.from_dict(stats)
    df['date'] = raw['fantasy_content']['team']['team_stats']['date']
    df['team_key'] = raw['fantasy_content']['team']['team_key']
    return df

def data_to_csv(target_dir, data_to_write, desired_name):
    """Convenience function to write a dict to CSV with appropriate parameters."""
    #generate directory if doesn't exist
    global d
    if len(data_to_write) == 0:
        return None
    if not os.path.exists(target_dir):
        os.makedirs(target_dir)
    if type(data_to_write) == dict:
        #order dict by keys
        d = OrderedDict(sorted(data_to_write.items()))
        keys = d.keys()
    if type(data_to_write) == list:
        d = data_to_write
        keys = data_to_write[0].keys()
    with open("%s/%s.csv" % (target_dir, desired_name), 'wb') as f:
        dw = unicodecsv.DictWriter(f, keys, dialect='ALM')
        dw.writeheader()
        if type(data_to_write) == dict:
            dw.writerow(d)
        if type(data_to_write) == list:
            dw.writerows(d)
    f.close()
