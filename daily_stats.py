import pandas as pd
import scipy.stats as stats

#data cleaning
fbb_stats = pd.read_csv('data\\team_by_date_all.csv')
era = pd.read_csv('data\\era_sim.csv')
whip = pd.read_csv('data\\whip_sim.csv')

whip['reported_whip'] = whip['reported_whip'].round(decimals=2)
era['reported_era'] = era['reported_era'].round(decimals=2)
whip.rename(
    columns={'true_whip': 'true_value', 'reported_whip': 'reported_value'},
    inplace=True
)
era.rename(
    columns={'true_era': 'true_value', 'reported_era': 'reported_value'},
    inplace=True
)
sim_size = len(era.index)


def stat_percentile(stat_name, stat_value, IP=None):
    """
    wrapper function that determines correct distribution function
    """
    #dispatch (different for ERA, WHIP)
    if stat_name in ['ERA', 'WHIP']:
        p = rate_percentile(stat_name, stat_value, IP)
    else:
        p = counting_percentile(stat_name, stat_value)
    #return percentile
    return p


def counting_percentile(stat_name, stat_value):
    """
    returns the percentile in the distribution of a given value.
    used for counting stats.
    """
    #limit to just the stat
    this_stat = fbb_stats[fbb_stats['stat_name'] == stat_name]
    #ignore NaN
    this_stat = this_stat[pd.notnull(this_stat['value'])]
    #convert to percentile
    return stats.percentileofscore(this_stat['value'], stat_value)


def rate_percentile(stat_name, stat_value, IP):
    """
    recovers 'true' value given rate and IP, then returns value
    used for pitching rate stats.
    """
    if stat_name == 'ERA':
        df = era
    elif stat_name == 'WHIP':
        df = whip
    exact = df[(abs(df['IP'] - IP) < .1) & (df['reported_value'] == stat_value)]
    if len(exact.index) > 0.0005 * sim_size:
        recovered_value = exact['true_value'].mean()
    else:
        df['IP_diff'] = abs(df['IP'] - IP)
        df['v_diff'] = abs(df['reported_value'] - stat_value)
        df['diff_total'] = df['IP_diff'] + 2 * df['v_diff']
        df['diff_rank'] = df['diff_total'].rank(ascending=True, pct=True)
        recovered_value = df[df['diff_rank'] <= 0.0005]['true_value'].mean()
    return round(recovered_value, 2)

rate_percentile('ERA', 0, 27)
rate_percentile('ERA', 0, 1)
rate_percentile('ERA', 0, 0.33)
rate_percentile('ERA', 27, 0.33)

#get distribution of estimated 'true' ERAs for percentile
true_eras = []
true_whips = []

for i in fbb_stats['date'].unique():
    print i
    this_day = fbb_stats[fbb_stats['date']== i]
    this_era = this_day[this_day['stat_name']=='ERA']
    for index, row in this_era.iterrows():
        true = rate_percentile('ERA', float(row['value']), float(row['IP']))
        true_eras.append(true)
    this_whip = this_day[this_day['stat_name']=='WHIP']
    for index, row in this_whip.iterrows():
        true = rate_percentile('WHIP', float(row['value']), float(row['IP']))
        true_whips.append(true)

eras = pd.DataFrame(true_eras)
whips = pd.DataFrame(true_whips)
