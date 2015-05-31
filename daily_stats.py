import pandas as pd
import scipy.stats as stats

fbb_stats = pd.read_csv('data\\team_by_date_all.csv')
era = pd.read_csv('data\\era_sim.csv')
whip = pd.read_csv('data\\w)


def stat_percentile(stat_name, stat_value, IP=None):
    """
    wrapper function that determines correct distribution function
    """
    #dispatch (different for ERA, WHIP)
    if(stat_name in ['ERA', 'WHIP']):
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