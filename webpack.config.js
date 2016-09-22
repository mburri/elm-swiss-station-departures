var DashboardPlugin = require('webpack-dashboard/plugin');

module.exports = {
    entry: './src/entry.js',
    output: {
        path: './dist',
        filename: 'bundle.js'
    },

    resolve: {
        modulesDirectories: ['node_modules'],
        extensions: ['', '.js', '.elm']
    },

    module : {
        loaders: [
            {
                test: /\.html$/,
                exclude: /node_modules/,
                loader: 'file?name=[name].[ext]'
            },
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader: 'elm-hot!elm-webpack'
            }
        ]
    },

    plugins: [
        new DashboardPlugin()
    ],

    devServer: {
        inline: true,
        stats: 'errors-only'
    }
};
