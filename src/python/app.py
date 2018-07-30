# Data manipulation
import numpy as np
import pandas as pd
# Data visualization
import dash
import dash_core_components as dcc
import dash_html_components as html
import plotly as pl
import plotly.graph_objs as pgo
import PlotlyChartGenerator as pcg
# Credentials
from getpass import getpass

####################
# DATA PREPARATION #
####################

print('Preparing Data')

# Set paths
out_dir = '../../out/'
data_dir = '../../data/'

# Get credentials
user = 'marvinward'
pw = getpass('Enter Password: ')
pl.tools.set_credentials_file(username=user, api_key=pw)

# Read in data
lcc = pd.read_csv(data_dir + 'lcc_fulldata.csv')

# Convert months to pandas periods
lcc['month'] = lcc['periodid'].apply(lambda x: pd.Period(x, freq='M'))

# Map income categories to quintiles
int_dict = {
    '<21': 'q1',
    '21-40': 'q2',
    '41-60': 'q3',
    '61-80': 'q4',
    '81-100': 'q5'
}
lcc['category'] = lcc['category'].replace(int_dict)

# Construct lists of areas, dimensions and measures
areas = sorted(set(lcc['area_name']))
dims = sorted(set(lcc['dimname']))
measures = ['growth_rate', 'spend_share', 'growth_contribution']


####################
# APP CONSTRUCTION #
####################

app = dash.Dash()

print('Launching app')

# Construct header
ttl = html.H1(children='Local Consumer Commerce')
desc = html.Div(children= '''
    An LCC Data Exploration Tool
''')

# Construct dropdown menus for subset selection
area_options = []
for a in [a for a in areas if 'Metro' in a] + ['National']:
    area_options.append({'label': a.split(' - ')[0], 'value': a})
area_select = dcc.Dropdown(options=area_options, value='National')

dim_options = []
for d in dims:
    dim_options.append({'label': d.title(), 'value': d})
dim_select = dcc.Dropdown(options=dim_options, value='Total')

meas_options = []
for m in measures:
    meas_options.append({'label': ' '.join(m.split('_')).title(), 'value': m})
meas_select = dcc.Dropdown(options=meas_options, value='growth_rate')

dropdown_div = html.Div([
    html.Table(
        html.Tr([
            html.Td(html.Div([area_select]), style={'width': '30%'}),
            html.Td(html.Div([dim_select]), style={'width': '30%'}),
            html.Td(html.Div([meas_select]), style={'width': '30%'})
        ])
    )
])


# Construct chart
# 7xVBokctuBoAVfBrSMs8
x = np.random.uniform(size=100)
y = np.random.normal(size=100)
chart = dcc.Graph(
    id = 'random-scatter',
    figure = {
        'data': [{'x': x, 'y': y, 'type': 'scatter', 'name': 'Series'}],
        'layout': {'title': 'Normal by Uniform'}
    }
)

# chart = dcc.Graph(
#     id='example-graph',
#     figure={
#         'data': [
#             {'x': [1, 2, 3], 'y': [4, 1, 2], 'type': 'bar', 'name': 'SF'},
#             {'x': [1, 2, 3], 'y': [2, 4, 5], 'type': 'bar', 'name': u'Montréal'},
#         ],
#         'layout': {
#             'title': 'Dash Data Visualization'
#         }
#     }
# )

# Construct body of the app
app.layout = html.Div(children=[ttl, desc, dropdown_div])

if __name__ == '__main__':
    print('Launching app')
    app.run_server(debug=True)
