// Based on https://github.com/fable-compiler/webpack-config-template/blob/master/webpack.config.js and https://github.com/MangelMaxime/fulma-demo/blob/master/webpack.config.js.

var path = require("path");
var webpack = require("webpack");
var HtmlWebpackPlugin = require('html-webpack-plugin');
var CopyWebpackPlugin = require('copy-webpack-plugin');
var MiniCssExtractPlugin = require("mini-css-extract-plugin");

var isProduction = !hasArg(/serve/);
var outputWebpackStatsAsJson = hasArg('--json');

var CONFIG = {
    indexHtmlTemplate: './src/ui/index.html',
    fsharpEntry: './src/ui/program/run.fs.js',
    sassEntry: './src/ui/style/sweepstake-2026-bulma.sass',
    cssEntry: './src/ui/style/sweepstake-2026.css',
    outputDir: './src/ui/publish',
    assetsDir: './src/ui/public',
    publicPath: '/',
    devServerPort: 8080,
	babel: {
        presets: [
            ["@babel/preset-env", {
                "modules": false,
                "useBuiltIns": "usage",
                "corejs": 3,
            }]
        ],
    }
}

if (!outputWebpackStatsAsJson) {
	console.log("Bundling for " + (isProduction ? "production" : "development") + "...");
}

var commonPlugins = [
    new HtmlWebpackPlugin({
        filename: 'index.html',
        template: resolve(CONFIG.indexHtmlTemplate)
    })
];

module.exports = {
    entry: isProduction ? {
        app: [resolve(CONFIG.fsharpEntry), resolve(CONFIG.sassEntry), resolve(CONFIG.cssEntry)]
    } : {
        app: [resolve(CONFIG.fsharpEntry)],
        style: [resolve(CONFIG.sassEntry), resolve(CONFIG.cssEntry)]
    },
    output: {
        publicPath: CONFIG.publicPath,
        path: resolve(CONFIG.outputDir),
        filename: isProduction ? '[name].[contenthash].js' : '[name].js',
    },
    mode: isProduction ? 'production' : 'development',
    devtool: isProduction ? 'source-map' : 'eval-source-map',
	optimization: {
        splitChunks: {
            cacheGroups: {
                commons: {
                    test: /node_modules/,
                    name: "vendors",
                    chunks: "all"
                }
            }
        },
    },
    plugins: isProduction ?
        commonPlugins.concat([
            new MiniCssExtractPlugin({ filename: 'style.[contenthash].css' }),
            new CopyWebpackPlugin({
                patterns: [{
                    from: resolve(CONFIG.assetsDir) }]
                })
        ])
        : commonPlugins.concat([
            new webpack.HotModuleReplacementPlugin(),
        ]),
    resolve: {
        symlinks: false
    },
    devServer: {
        historyApiFallback: {
            index: '/'
        },
        publicPath: CONFIG.publicPath,
        contentBase: resolve(CONFIG.assetsDir),
        host: '0.0.0.0',
        port: CONFIG.devServerPort,
        proxy: CONFIG.devServerProxy,
        hot: true,
        inline: true
    },
    module: {
        rules: [
            {
                test: /\.js$/,
				enforce: "pre",
                use: ['source-map-loader']
            },
            {
                test: /\.js$/,
                exclude: /node_modules/,
                use: {
                    loader: 'babel-loader',
                    options: CONFIG.babel
                },
            },
            {
                test: /\.(sass|scss|css)$/,
                use: [
                    isProduction
                        ? MiniCssExtractPlugin.loader
                        : 'style-loader',
                    'css-loader',
                    {
                        loader: 'sass-loader',
                        options: { implementation: require('sass') }
                    }
                ],
            }
        ]
    }
};

function resolve(filePath) {
    return path.isAbsolute(filePath) ? filePath : path.join(__dirname, filePath);
}

function hasArg(arg) {
    return arg instanceof RegExp
        ? process.argv.some(x => arg.test(x))
        : process.argv.indexOf(arg) !== -1;
}
