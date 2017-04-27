import pandas as pd

t = 600

df = pd.read_csv('player_time.csv')

df['team_1_gold'] = (df.gold_t_0 + df.gold_t_1 + df.gold_t_2 +
                     df.gold_t_3 + df.gold_t_4)
df['team_2_gold'] = (df.gold_t_128 + df.gold_t_129 + df.gold_t_130 +
                     df.gold_t_131 + df.gold_t_132)

df = df[['match_id', 'times', 'team_1_gold', 'team_2_gold']]
df = df.loc[df.times == t]
df['gold_lead'] = list((df.team_1_gold > df.team_2_gold))
df['gold_lead_by'] = (df.team_1_gold - df.team_2_gold) / df.team_2_gold
df = df[['match_id', 'gold_lead', 'gold_lead_by']]

match = pd.read_csv('match.csv')
match = match[['match_id', 'radiant_win']]

n = 0
gl = pd.merge(df, match)
gl = gl.loc[gl.gold_lead_by > n]

sum(gl.gold_lead == gl.radiant_win)
sum(gl.gold_lead == gl.radiant_win) / len(gl)

