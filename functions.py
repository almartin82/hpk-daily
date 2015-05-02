def make_daily_stats_req(team, date):
    base = "http://fantasysports.yahooapis.com/fantasy/v2/team/"
    sub_resource = "/stats;type=date;date="
    final = base + team + sub_resource + str(date)
    return(final)

