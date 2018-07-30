import plotly.graph_objs as pgo
import pandas as pd

class PlotlyChartGenerator:

    def __init__(self, a, d, m, df):
        # Parametric inputs
        self.area = a
        self.dim = d
        self.measure = m
        # Default values (which can be reset)
        self.df = df
        self.min_per = self.df.month.min()
        self.max_per = self.df.month.max()
        self.colors = ['#00a0dd', '#a2dadb', '#bbd976', '#ffe18b', '#fbaf5d', '#f57f32']
        self.dim_labs = self.set_labels()

    def set_labels(self, lab_dict=None):
        if lab_dict == None:
            dim_labs = {
                'age': ['<25', '25-34', '35-44', '45-54', '55-64', '65+'],
                'income': ['q1', 'q2', 'q3', 'q4', 'q5'],
                'bizsize': ['SMALL', 'MEDIUM', 'LARGE'],
                'location': ['Same Neighborhood', 'Same Region', 'Different Region'],
                'product': ['Durables', 'Fuel', 'Nondurables', 'Other Services', 'Restaurants']
            }
        return dim_labs

    def to_string(self, n=5):
        '''
        Method returns a string representation of attribute states and the input data.
        '''
        attr_dict = {
            'Area': self.area,
            'Dimension': self.dim,
            'Measure': self.measure,
            'Minimum Period': self.min_per,
            'Maximum Period': self.max_per,
            'Color Palette': self.colors
        }
        s = 'plotlyChartGenerator(\n'
        for attr in attr_dict:
            s += '\t{k} = {v}\n'.format(k=attr, v=attr_dict[attr])
        s += ')\n\nInput Data Sample:'
        print(s)
        df_sub = self.get_lcc_sub(self.area, self.dim, self.measure, self.df)
        print(df_sub.head(n))

    def get_lcc_sub(self):
        # Define area and dimension subset conditions
        a = (self.df['area_name'] == self.area)
        d = (self.df['dimname'] == self.dim)
        # Subset data
        df_sub = self.df[a & d][['month', 'category', self.measure]]
        # Set month and category to the index
        df_sub.set_index(['month', 'category'], inplace=True)
        # Unstack category (aka - convert from long to wide format)
        df_sub = df_sub.unstack('category') * 100
        # Drop multiindex columns in favor of category names
        df_sub.columns = [c[1] for c in df_sub.columns]
        # If we are look at anything but the total growth rate, fix label order
        if self.dim != 'Total':
            df_sub = df_sub[self.dim_labs[self.dim]]
        return df_sub.reset_index()

    def get_traces(self, plot_func):
        '''
        Method returns a list of traces that can be paired with a layout for plotly charts.

        Note that self method leverages higher-lever function capability. One must choose the kind of
        trace they seek (e.g. Scatter or Bar). Right now self is flimsily built to only support a
        couple chart types
        '''
        # Grab relevant subset
        df_sub = self.get_lcc_sub(self.area, self.dim, self.measure, self.df)
        df_sub = df_sub[(df_sub['month'] >= self.min_per) & (df_sub['month'] <= self.max_per)]
        # Generate string version of the month variable because it must be serializable
        # (it will be converted to javascript)
        df_sub['month_str'] = df_sub['month'].apply(lambda x: x.strftime('%m-%Y'))
        # Capture traces for each category from subset (expanded for conceptual clarity)
        traces = []
        for i,d in enumerate(self.dim_labs[self.dim]):
            if plot_func == pgo.Scatter:
                next_trace = plot_func(x=df_sub['month_str'], y=df_sub[d], name=d, line=dict(color=self.colors[i]))
            elif plot_func == pgo.Bar:
                next_trace = plot_func(x=df_sub['month_str'], y=df_sub[d], name=d, marker=dict(color=self.colors[i]))
            traces.append(next_trace)
        return traces

    def get_layout(self, barmode=None):
        '''
        Method returns a layout with plot title and y-axis label tailored to the given
        combination of area, dimension, and measure.
        '''
        meas_str = ' '.join(self.measure.split('_')).title()
        dim_str = self.dim.title()
        ttl = '{m} in {a} by {d}'.format(a=self.area, d=dim_str, m=meas_str)
        y_ttl='Year-over-Year {} (%)'.format(meas_str)
        yaxis_config = {
            'title': y_ttl
        }
        return pgo.Layout(title=ttl, yaxis=yaxis_config, barmode=barmode)

    def get_fig(self, plot_func, barmode=None):
        '''
        Method returns a plotly figure that is ready for plotting, building on the traces
        and layout returned in above methods.
        '''
        traces = self.get_traces(plot_func)
        layout = self.get_layout(barmode)
        return pgo.Figure(data=traces, layout=layout)